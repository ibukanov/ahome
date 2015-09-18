package lib.x;

import java.io.*;
import java.net.*;

public class XConnection {

  String displayName;

  int protocolMajorVersion;
  int protocolMinorVersion;
  int releaseNumber;
  int resourceIdBase;
  int resourceIdMask;
  int resourceIdIncrement;
  int resourceIdNext;
  int motionBufferSize;
  int maxRequestLength;
  
  boolean imageMSFByteOrder;
  boolean bitmapFormatMSFBitOrder;
  int bitmapFormatScanlineUnit;
  int bitmapFormatScanlinePad;

  int minKeycode;
  int maxKeycode;

  String vendorData;
  Screen[] screens;
  ZPixmapFormat[] zPixmapFormats; 
  
  Screen defaultScreen;

  public final String displayName() { return displayName; }
  public final int protocolMajorVersion() { return protocolMajorVersion; }
  public final int protocolMinorVersion() { return protocolMinorVersion; }
  public final int releaseNumber() { return releaseNumber; }
  
  public final int bitmapFormatScanlinePad() { return bitmapFormatScanlinePad; }

  public final String vendorData() { return vendorData; }
  public final Screen screen(int i) { return screens[i]; }
  public final int screenCount() { return screens.length; }
  public final ZPixmapFormat zPixmapFormat(int i) { 
    return zPixmapFormats[i]; 
  } 
  public final int zPixmapFormatsCount() { return zPixmapFormats.length; } 
  
  public final Screen defaultScreen() { return defaultScreen; }

  public static String displayNameFromArgs(String[] args) {
    int end = args.length - 1;
    for (int i = 0; i < end; ++i) {
      if (args[i].equals("-display")) {
        return args[i + 1];
      }
    }
    return null;
  }
  
  public XConnection(String displayName) {
    if (displayName == null) {
      displayName = System.getProperty("X.DISPLAY");
      if (displayName == null) {
        throw new XIOError("Display name is not defined");
      }  
    }
    this.displayName = displayName;
    initConnection();
    serverInputThread = new Thread(dataInput, "X socket input data thread");
    serverInputThread.setDaemon(true);
    serverInputThread.start();
  }
  
  protected void finalize() { close(); }

  public void close() {
    output.flush();
    if (null != serverInputThread) {
      serverInputThread.stop();
      serverInputThread = null;
    }
    if (null != inputFromSocket) { 
      try { inputFromSocket.close(); } catch (IOException e) { }
      inputFromSocket = null; 
    }
    if (null != outputToSocket) { 
      try { outputToSocket.close(); } catch (IOException e) { }
      outputToSocket = null; 
    }
    if (xSocket != null) { 
      try { xSocket.close(); } catch (IOException e) { }
      xSocket = null; 
    }
  }
  
  private static final int X_SOCKET_PORT_BASE = 6000;
  private static final int MSB_FIRST_FLAG = 0x42;

  private Socket xSocket;
  private InputStream inputFromSocket;
  private OutputStream outputToSocket;
  private XEventConsumer eventConsumer = null;
  private Thread serverInputThread = null;
  
  Thread dpyThread;
  XBufferedOutputStream output;  
  final XReplyDataInput dataInput = new XReplyDataInput(this);
  

  private void initConnection() { 
    this.displayName = displayName;
    int colonPos = displayName.lastIndexOf(':');
    if (colonPos == -1) {
      throw new XIOError("Can not parse display name '" + displayName + "'");
    }
    int defaultScreenNumber;
    int socketNumber;
    int dotPos = displayName.indexOf('.', colonPos + 1);
    try {
      if (dotPos == -1) {
        socketNumber = Integer.parseInt(displayName.substring(colonPos + 1));
        defaultScreenNumber = 0;
      }
      else {
        socketNumber = Integer.
          parseInt(displayName.substring(colonPos + 1, dotPos));
        defaultScreenNumber = Integer.
          parseInt(displayName.substring(dotPos + 1));
      }
      if (socketNumber < 0) {
        throw new XIOError("Invalid display number '" + socketNumber
                           + "' for display '" + displayName + "'");
      }  
      if (defaultScreenNumber < 0) {
        throw new XIOError("Invalid screen number '" + defaultScreenNumber
                           + "' for display '" + displayName + "'");
      }
      socketNumber += X_SOCKET_PORT_BASE;
    }
    catch (NumberFormatException  e) {
      throw new XIOError("Can not parse display name '" + displayName + "'");
    }  
    dpyThread = Thread.currentThread();
    outputToSocket = null;
    inputFromSocket = null;
    xSocket = null;
    try {
      xSocket = new Socket(displayName.substring(0, colonPos), socketNumber);
      outputToSocket = xSocket.getOutputStream();
      output = new XBufferedOutputStream(outputToSocket);
      
      output.writeByte(MSB_FIRST_FLAG); // high-byte first order
      output.writeByte((byte)0);   // unused
      output.writeShort(X.XPROTOCOL_MAJOR_VERSION); 
      output.writeShort(X.XPROTOCOL_MINOR_VERSION); 

      output.writeShort((short)0);  // No authorization protocol name 
      output.writeShort((short)0);  // No authorization protocol data
      output.writeShort((short)0);  // unused 
      output.flush();

      inputFromSocket = xSocket.getInputStream();
      
      XInput firstReply = 
        new XBufferedInputStream(inputFromSocket, X.INPUT_BUF_SIZE);
      
      if (firstReply.readBoolean()) {
        // Connection established
        readDisplayInfo(defaultScreenNumber, firstReply);
      }
      else {
        // Connection failed
        int reasonLength = firstReply.readUnsignedByte();
        protocolMajorVersion = firstReply.readUnsignedShort();
        protocolMinorVersion = firstReply.readUnsignedShort();
        firstReply.readUnsignedShort(); // additional data length
        byte reason[] = new byte[reasonLength];
        firstReply.read(reason, 0, reasonLength);
        firstReply.pad4();
        throw new XIOError("Can not connect to the display '" 
                           + displayName + "'\n" 
                           + new String(reason, 0, 0, reasonLength));
      }
    }  
    catch(IOException e) {
      close();
      throw new XIOError("Can not connect to the display '" 
                           + displayName + "'\n" + e);
    }
  }
  
  private void readDisplayInfo(int defaultScreenNumber, XInput firstReply) {
    firstReply.skip(1);
    
    protocolMajorVersion = firstReply.readUnsignedShort();
    protocolMinorVersion = firstReply.readUnsignedShort();
 
    int dwordCount = firstReply.readUnsignedShort();

    releaseNumber = firstReply.readInt();
    resourceIdBase = firstReply.readInt();
    resourceIdMask = firstReply.readInt();
    resourceIdBase &= ~resourceIdMask; 

    resourceIdIncrement = Tk.calculateFirstBitForMask(resourceIdMask);
    resourceIdNext = 0;

    motionBufferSize = firstReply.readInt();
    
    byte[] vendorDataBytes = new byte[firstReply.readUnsignedShort()];

    maxRequestLength = X.INT_SIZE * firstReply.readUnsignedShort();
    
    screens = new Screen[firstReply.readUnsignedByte()];
    
    zPixmapFormats = new ZPixmapFormat[firstReply.readUnsignedByte()];
    
    imageMSFByteOrder = (0 != firstReply.readUnsignedByte());
    bitmapFormatMSFBitOrder = (0 != firstReply.readUnsignedByte());
    bitmapFormatScanlineUnit = firstReply.readUnsignedByte();
    bitmapFormatScanlinePad = firstReply.readUnsignedByte();
    minKeycode = firstReply.readUnsignedByte();
    maxKeycode = firstReply.readUnsignedByte();
    
    firstReply.skip(4);
    
    if (vendorDataBytes.length != 0) {
      firstReply.read(vendorDataBytes);
      firstReply.pad4();
    }
    vendorData = new String(vendorDataBytes, 0);
    
    for (int i = 0; i < zPixmapFormats.length; ++i) {
      zPixmapFormats[i] = new ZPixmapFormat(firstReply);
    }

    for (int i = 0; i < screens.length; ++i) {
      screens[i] = new Screen(i, firstReply, zPixmapFormats);
    }
    
    if (defaultScreenNumber >= screens.length) {
      throw new XIOError("Given screen number '" + defaultScreenNumber 
                         + "' do not present on the display '");
    }
    defaultScreen = screens[defaultScreenNumber];
    
  }  
  
  public final XEventConsumer eventConsumer() { return eventConsumer; }
  public final void setEventConsumer(XEventConsumer eventConsumer) {
    if (eventConsumer == null) {
      throw new XIOError("eventConsumer can not be null");
    }
    this.eventConsumer = eventConsumer;
  }
  
  public final void flush() {
    if (X.DEBUG) Tk.assert(!inReply);
    output.flush();
  }
  
  final void readInput(byte array[], int offset, int n) {
    if (X.DEBUG) Tk.assert(n + offset <= array.length);
    if (n > 0) {
      try {
        for (;;) {
          int ret = inputFromSocket.read(array, offset, n);
          if (ret == n) {
            return;
          }
          if (X.DEBUG) Tk.assert(ret > 0);
          n -= ret;
          offset += ret;
        }  
      }
      catch (IOException e) {
        throw new XIOError(e);
      }  
    }  
  }                       

  final void skipInput(long n) {
    if (n > 0) {
      try {
        for (;;) {
          long ret = inputFromSocket.skip(n);
          if (ret == n) {
            return;
          }
          if (X.DEBUG) Tk.assert(ret > 0);
          n -= ret;
        }  
      }
      catch (IOException e) {
        throw new XIOError(e);
      }  
    }  
  }                       

  final Object replyLock = new Object();
    boolean inReply = false;
    boolean waitForServerAnswer = false;
    long requestCount = 0;
    final byte[] reply = new byte[X.MIN_REPLY_LENGTH];
    XError replyError = null;
    XError lastAsynchronousError = null;
  
  public final long requestCount() { return requestCount; }
  
  final void readServer() throws IOException {
    byte buf[] = new byte[X.INPUT_BUF_SIZE];
    XError[] asynchronousErrors = new XError[X.MAX_ERRORS_IN_BUF];
    int[] eventsShifts = new int[X.MAX_EVENTS_IN_BUF];
    int eventsCount = 0;
    int asynchronousErrorsCount = 0;
    for (;;) {
      int available = inputFromSocket.available();
      if (available < X.MIN_REPLY_LENGTH) {
        available = X.MIN_REPLY_LENGTH;
      }
      else if (available > X.INPUT_BUF_SIZE - X.MIN_REPLY_LENGTH + 1) {
        available = X.INPUT_BUF_SIZE - X.MIN_REPLY_LENGTH + 1;
      }  
      readInput(buf, 0, available);

      int shift = 0;
      int newShift = X.MIN_REPLY_LENGTH;
      boolean shouldFinishReplyData = false;
    dataLoop:
      for (;;) {
        int opcode = 0x7F & buf[shift];
        switch (opcode) {
        case X.REPLY_OPCODE:
          int dataShift = newShift;
          int dataSize = Tk.xReplyDataSize(buf, shift);
          synchronized (replyLock) {
            while (inReply) {
              try { replyLock.wait(); } catch (InterruptedException e) { }
            }
            if (X.DEBUG) Tk.assert(Tk.isAnswerNumber(requestCount, buf, shift));
            inReply = true;
            System.arraycopy(buf, shift, reply, 0, X.MIN_REPLY_LENGTH);
            if (dataSize == 0) {
              dataInput.putNoData();
              break;
            }
            else {
              newShift += dataSize;
              if (newShift <= available) {
                dataInput.putFirstData(buf, dataShift, dataSize, 0);
                break;
              }
              else if (newShift > X.INPUT_BUF_SIZE) {
                int rest = available - dataShift;
                dataInput.putFirstData(buf, dataShift, rest, dataSize - rest);
                break dataLoop;
              }  
            }
          }
          // here  available < newShift && newShift <= X.INPUT_BUF_SIZE 
          readInput(buf, available, newShift - available);
          synchronized (replyLock) {
            dataInput.putFirstData(buf, dataShift, dataSize, 0);
          }  
          break dataLoop;
        case X.ERROR_OPCODE:  
          long number;
          boolean errorForCurrentReply;
          synchronized (replyLock) {
            number = Tk.buildAnswerNumber(requestCount, buf, shift);
            errorForCurrentReply = (number == requestCount 
                                    && waitForServerAnswer);
          }  
          XError err = XError.create(buf, shift, number);
          if (errorForCurrentReply) {
            synchronized (replyLock) {
              while (inReply) {
                try { replyLock.wait(); } catch (InterruptedException e) { }
              } 
              inReply = true;
              replyError = err;
              replyLock.notify();
            }
          }
          else {
            lastAsynchronousError = err;
            asynchronousErrors[asynchronousErrorsCount++] = err; 
          }
          break;
        default: // Event
          eventsShifts[eventsCount++] = shift;
          break;
        }  
        if (newShift == available) {
          break;
        }
        shift = newShift;
        newShift += X.MIN_REPLY_LENGTH;
        int lookForward = newShift - available;
        if (lookForward > 0) {
          readInput(buf, available, lookForward);
          available = newShift;
        }
      }
      if (asynchronousErrorsCount > 0) {
        // ??? Big questions is how to handle asynchronous Errors
        // Currently the last error is stored in lastAsynchronousError
        System.err.println("Got " + asynchronousErrorsCount 
                           + "asynchronous errors");
        for (int i = 0; i < asynchronousErrorsCount; i++) {
          System.err.println(i + ": " + asynchronousErrors[i].toString());
        }  
        asynchronousErrorsCount = 0;
      }  
      if (eventsCount > 0) {
        if (eventConsumer != null) {
          eventConsumer.xEvents(buf, eventsShifts, eventsCount);
        }
        eventsCount = 0;  
      }
    }  
  }    
    
  int       debugCurRequestType = -1;
  int       debugRequestBytesCount;
  long      debugRequestStartPos;
  boolean   debugInRequestOutput = false;
  
  final void startRequest(int opcode, int firstByte, int addLength) {
    startRequest(opcode, firstByte, addLength, X.ASYNCHRONOUS_REQUEST);
  }

  final void startRequest(int opcode, int firstByte, int addLength, 
                          boolean waitForAnswer) {
    int requestLength = REQUEST_HEAD_LENGTHS[opcode] + addLength;
    if (X.DEBUG) {
      Tk.assert(dpyThread == Thread.currentThread());
      if (debugInRequestOutput) {
        throw new XProgErr("Reuqest " + debugCurRequestType 
                           + "does not contain endRequest call");
      }
      debugCurRequestType = opcode;
      Tk.assert(addLength % X.INT_SIZE == 0);
      Tk.assert(requestLength % X.INT_SIZE == 0);
      Tk.assert(requestLength <= X.MAX_REQUEST_SIZE);
      debugRequestStartPos = output.bytesCount();
      Tk.assert(debugRequestStartPos % X.INT_SIZE == 0);
      debugRequestBytesCount = requestLength;
      debugInRequestOutput = true;
    }
    if (maxRequestLength < requestLength) {
      throw new XIOError("Request length " + requestLength 
                         + " exceeds X server limit " 
                         + maxRequestLength);
    }
    synchronized (replyLock) { 
      ++requestCount; 
      this.waitForServerAnswer = waitForAnswer;
    }  
    output.startRequest(requestLength);
    output.writeByte(opcode);
    output.writeByte(firstByte);
    output.writeShort(requestLength / 4);
  } 

  final void endRequest() {
    if (X.DEBUG) {
      long curPos = output.bytesCount();
      Tk.assert(curPos % X.INT_SIZE == 0);
      Tk.assert(curPos - debugRequestStartPos == debugRequestBytesCount);
      debugInRequestOutput = false;
    }  
  } 

  private int addReplyDataLength;
  protected final int addReplyDataLength() { return addReplyDataLength; }
  
  final byte[] getReply() throws XError {
    output.flush();
    synchronized (replyLock) {
      if (X.DEBUG) Tk.assert(!inReply);
      if (X.DEBUG) Tk.assert(waitForServerAnswer);
      do {
        try { replyLock.wait(); } catch (InterruptedException e) { }
      } while (!inReply);
    }
    if (replyError != null) {
      XError err = replyError;
      replyError = null;
      inReply = false;
      throw err;
    }  
    return reply;
  }
  
  final void finishReply() {
    synchronized (replyLock) {
      if (X.DEBUG) Tk.assert(inReply);
      if (X.DEBUG) Tk.assert(dataInput.available() == 0);
      inReply = false;
      waitForServerAnswer = false;
      replyLock.notify();
    }
  }
  
  int maxRequestDataLength(int request) {
    return maxRequestLength - REQUEST_HEAD_LENGTHS[request];
  }
  
  static final int REQUEST_HEAD_LENGTHS[] = {
      0, //000
     32, // CREATE_WINDOW_REQUEST
     12, // CHANGE_WINDOW_ATTRIBUTES_REQUEST
      0, //003
      8, // DESTROY_WINDOW_REQUEST
      0, //005
      0, //006
      0, //007
      8, // MAP_WINDOW_REQUEST
      0, //009
      8, // UNMAP_WINDOW_REQUEST
      0, //011
     12, // CONFIGURE_WINDOW_REQUEST
      0, //013
      0, //014
      0, //015
      8, // INTERN_ATOM_REQUEST
      8, // GET_ATOM_NAME_REQUEST
     24, // CHANGE_PROPERTY_REQUEST
     12, // DELETE_PROPERTY_REQUEST
     24, // GET_PROPERTY_REQUEST
      8, // LIST_PROPERTIES_REQUEST
      0, //022
      0, //023
      0, //024
      0, //025
      0, //026
      0, //027
      0, //028
      0, //029
      0, //030
      0, //031
      0, //032
      0, //033
      0, //034
      0, //035
      0, //036
      0, //037
      0, //038
      0, //039
      0, //040
      0, //041
      0, //042
      0, //043
      0, //044
     12, // OPEN_FONT_REQUEST
      8, // CLOSE_FONT_REQUEST
      8, // QUERY_FONT_REQUEST
      0, //048
      0, //049
      0, //050
      0, //051
      0, //052
     16, // CREATE_PIXMAP_REQUEST
      8, // FREE_PIXMAP_REQUEST
     16, // CREATE_GC_REQUEST
     12, // CHANGE_GC_REQUEST
      0, //057
      0, //058
     12, // SET_CLIP_RECTANGLES_REQUEST
      8, // FREE_GC_REQUEST
      0, //061
     28, // COPY_AREA_REQUEST
      0, //063
     12, // POLY_POINT_REQUEST
     12, // POLY_LINE_REQUEST
     12, // POLY_SEGMENT_REQUEST
     12, // POLY_RECTANGLE_REQUEST
     12, // POLY_ARC_REQUEST
     16, // FILL_POLY_REQUEST
     12, // POLY_FILL_RECTANGLE_REQUEST
     12, // POLY_FILL_ARC_REQUEST
     24, // PUT_IMAGE_REQUEST
     20, // GET_IMAGE_REQUEST
     16, // POLY_TEXT8_REQUEST
     16, // POLY_TEXT16_REQUEST
      0, //076
      0, //077
     16, // CREATE_COLORMAP_REQUEST
      8, // FREE_COLORMAP_REQUEST
      0, //080
      0, //081
      0, //082
      0, //083
     16, // ALLOC_COLOR_REQUEST
      0, //085
      0, //086
      0, //087
     12, // FREE_COLORS_REQUEST
      0, //089
      0, //090
      8, // QUERY_COLORS_REQUEST
      0, //092
     32, // CREATE_CURSOR_REQUEST
     32, // CREATE_GLYPH_CURSOR_REQUEST
      8, // FREE_CURSOR_REQUEST
     20, // RECOLOR_CURSOR_REQUEST
      0, //097
      0, //098
      0, //099
      0, //100
      8, // GET_KEYBOARD_MAPPING_REQUEST
      0, //102
      0, //103
      0, //104
      0, //105
      0, //106
      0, //107
      0, //108
      0, //109
      0, //110
      0, //111
      0, //112
      0, //113
      0, //114
      0, //115
      0, //116
      0, //117
      4, // SET_MODIFIER_MAPPING_REQUEST
      4, // GET_MODIFIER_MAPPING_REQUEST
      0, //120
      0, //121
      0, //122
      0, //123
      0, //124
      0, //125
      0, //126
      0, //127
    
  };

}

class XBufferedOutputStream {
  
  static final int bufLength = X.OUTPUT_BUF_SIZE;

  OutputStream os;
  byte buffer[];
  int cursor;
  int curRequestPos;

  final boolean canPutToBuffer(int n) { return cursor + n <= bufLength; } 
  final void startRequest(int n) {
    if (cursor != 0 && cursor + n > bufLength) {
      flush();
    }  
    curRequestPos = cursor;
  }

  XBufferedOutputStream(OutputStream os) {
    this.os = os;
    buffer = new byte[bufLength];
    cursor = 0;
    curRequestPos = -1;
  }
  
  private long bytesCount = 0;
  public final long bytesCount() { return bytesCount; }

  public final void write(int b) {
    if (cursor == bufLength) {
      flush();
    }  
    buffer[cursor++] = (byte)b;
    ++bytesCount;
  }
    
  public final void write(byte[] b, int off, int len) {
    int freeSpace = bufLength - cursor;
    if (len > freeSpace) {
      flush();
      try { os.write(b, off, len); }  
      catch (IOException e) { throw new XIOError(e); }  
    }
    else {
      System.arraycopy(b, off, buffer, cursor, len);
      cursor += len;
    }
    bytesCount += len;
  }
  
  public final void write(byte[] b) {
    write(b, 0, b.length);
  }

  public final void writeInts(int[] b, int off, int len) {
    while (len > 0) {
      --len;
      writeInt(b[off++]);
    }
  }
  
  public final void writeInts(int[] b) {
    writeInts(b, 0, b.length);
  }

  public final void writeShorts(short[] b, int off, int len) {
    while (len > 0) {
      --len;
      writeShort(b[off++]);
    }
  }
  
  public final void writeShorts(short[] b) {
    writeShorts(b, 0, b.length);
  }

  public final void writeChars(char[] b, int off, int len) {
    while (len > 0) {
      --len;
      writeChar(b[off++]);
    }
  }
  
  public final void writeChars(char[] b) {
    writeChars(b, 0, b.length);
  }

  public final void flush() {
    try {
      os.write(buffer, 0, cursor);
      cursor = 0;
      curRequestPos = -1;
      os.flush();
    }
    catch (IOException e) {
      throw new XIOError(e);
    }  
  }  
  
  public final void writeBoolean(boolean b) {
    write(b ? 1 : 0);
  }

  public final void writeByte(byte value) {
    write(value);
  }

  public final void writeByte(int value) {
    write(value);
  }

  public final void writeInt(int value) {
    if (X.DEBUG) Tk.assert(bytesCount % X.INT_SIZE == 0);
    write(value >> 24);
    write(value >> 16);
    write(value >> 8);
    write(value);
  }

  public final void writeShort(int value) {
    if (X.DEBUG) Tk.assert(bytesCount % X.SHORT_SIZE == 0);
    write(value >> 8);
    write(value);
  }

  public final void writeShort(short value) {
    writeShort((int)value);
  }

  public final void writeChar(int value) {
    writeShort(value);
  }

  public final void writeChar(char value) {
    writeShort((int)value);
  }

  public final void skip(int n) {
    while (n > 0) {
      --n;
      write(0);
    }
  }

  public final void pad4() {
    switch (((int)bytesCount) % 4) {
      case 3: write(0); break;  
      case 2: write(0); write(0); break;  
      case 1: write(0); write(0); write(0); break;  
    }
  }

  public final void pad2() {
    if (0 != bytesCount % 2) {
      write(0);
    }
  }
  
}

class XBufferedInputStream extends XInput {
// Buffered input functionality is not implemented currently...

  static final int EOF = -1;

  private InputStream is;
  
  XBufferedInputStream(InputStream is, int bufSize) {
    this.is = is;
  }

  protected int readImpl() {
    try {
      int b = is.read();
      if (X.DEBUG) Tk.assert(b != EOF);
      return b;
    }  
    catch(IOException e) { throw new XIOError(e); }
  }
    
  protected void readImpl(byte[] array, int offset, int length) {
    try {
      for (;;) {
        int count = is.read(array, offset, length);
        if (X.DEBUG) Tk.assert(count != -1);
        length -= count;
        if (0 == length) {
          break;
        }  
      }  
    }  
    catch(IOException e) {
      throw new XIOError(e);
    }
  }
    
  protected void skipImpl(int n) {
    try {
      long ret = is.skip(n);
      if (X.DEBUG) Tk.assert(n == ret);
    }  
    catch(IOException e) {
      throw new XIOError(e);
    }
  }
    
  protected int availableImpl() {
    try {
      return is.available();
    }  
    catch(IOException e) {
      throw new XIOError(e);
    }
  }

}

class XReplyDataInput extends XInput implements Runnable {

  final byte[] buf = new byte[X.INPUT_BUF_SIZE];
  int length = 0;
  int cursor = 0;
  int fromStream = 0;

  XConnection dpy;
  XReplyDataInput(XConnection dpy) {
    this.dpy = dpy;
  }
  
  final void putNoData() {
    if (X.DEBUG) Tk.assert(length == cursor);
    if (X.DEBUG) Tk.assert(fromStream == 0);
    length = 0;
    cursor = 0;              
    dpy.replyLock.notify();
  }
  
  final void putFirstData(byte[] array, int offset, int n, int rest) {
    if (X.DEBUG) {
      Tk.assert(length == cursor);
      Tk.assert(fromStream == 0);
      Tk.assert(n <= buf.length);
      Tk.assert(rest >= 0);
    }  
    System.arraycopy(array, offset, buf, 0, n);
    length = n;
    cursor = 0;              
    fromStream = rest;
    dpy.replyLock.notify();
    while (fromStream != 0) {
      try { dpy.replyLock.wait(); } catch (InterruptedException e) { }
    }
  }  
  
  protected int availableImpl() {
    return length - cursor + fromStream;
  }
  
  private void fillBuffer() {
    if (fromStream > buf.length) {
      dpy.readInput(buf, 0, buf.length);
      length = buf.length;
      fromStream -= buf.length;
    }
    else {
      if (fromStream > 0) {
        dpy.readInput(buf, 0, fromStream);
        length = fromStream;
        fromStream = 0;
      }
      synchronized (dpy.replyLock) {
        dpy.replyLock.notify(); // All data from socket are in buffer;
      }
    }  
    cursor = 0;
    
  }
  
  protected int readImpl() {
    if (X.DEBUG) Tk.assert(length + fromStream >= cursor + 1);
    if (cursor == length) { 
      fillBuffer();
    }  
    return 0xFF & buf[cursor++];
  }

  protected void skipImpl(int n) {
    if (X.DEBUG) Tk.assert(length + fromStream >= cursor + n);
    int rest = n - (length - cursor);
    if (rest <= 0) {
      cursor += n;
    }
    else {
      dpy.skipInput(rest);
      fromStream -= rest;
      fillBuffer();
    }  
  }  

  protected void readImpl(byte array[], int offset, int n) {
    if (X.DEBUG) Tk.assert(n + offset <= array.length);
    if (X.DEBUG) Tk.assert(length + fromStream >= cursor + n);
    int inBuffer = length - cursor;
    int rest = n - inBuffer;
    if (rest <= 0) {
      System.arraycopy(buf, cursor, array, offset, n);
      cursor += n;
    }  
    else {
      System.arraycopy(buf, cursor, array, offset, inBuffer);
      dpy.readInput(array, offset + inBuffer, rest);
      fromStream -= rest;
      fillBuffer();
    }  
  }

  public void run() {
    try {
      dpy.readServer();
    }
    catch (ThreadDeath e) {
      if (X.DEBUG) {
        Tk.dbg("X input thread has finished normally by the stop method call");
      }
    }
    catch (Throwable e) {
      if (X.DEBUG) {
        Tk.dbg("Uncaught exception terminated X input thread.");
        e.printStackTrace(System.err);
      }  
    }
  }

}


