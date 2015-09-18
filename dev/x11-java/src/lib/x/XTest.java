package lib.x;

import java.io.*;

class XEventDataQueue implements XEventConsumer {

  public static final int DEFAULT_INCREMENT = 32;
  public static final int DEFAULT_INIT_SIZE = DEFAULT_INCREMENT;
  
  private Display dpy;

  public XEventDataQueue(Display dpy) {
    this.dpy = dpy;
    this.increment = DEFAULT_INCREMENT;
    bufferSize = DEFAULT_INIT_SIZE;
    data = new byte[bufferSize][];    
    head = 0;
    queueSize = 0;
  }
  
  public final boolean empty() { return queueSize == 0; }
  public final int size() { return queueSize; }

  public final byte[] popHead() {
    if (empty()) {
      dpy.flush();
    }
    synchronized (this) {
      while (empty()) {
        try { wait(); } catch (InterruptedException e) { }
      }
      byte[] ret = data[head];
      head =  (head + 1) % bufferSize;
      --queueSize;
      return ret;
    }  
  }  

  public void xEvent(byte[] array, int shift) {
    byte[] event = new byte[X.EVENT_LENGTH];
    System.arraycopy(array, shift, event, 0, X.EVENT_LENGTH);
    putToTail(event);
  }
  
  public void xEvents(byte[] data, int[] shifts, int count) {
    for (int i = 0; i < count; ++i) {
      xEvent(data, shifts[i]);
    }
  }

  Command command;
  public synchronized void performLater(Command command) {
    if (this.command != null) {
      Tk.dbg("Warning: asynchronous command '" + this.command 
              + "' is replaced by'" + command + "'");
    }
    this.command = command;
  }
  
  public final synchronized void putToTail(byte[] obj) {
    if (queueSize == bufferSize) {
      int newSize = bufferSize + increment;
      byte[][] temp = new byte[newSize][];
      System.arraycopy(data, head, temp, 0, bufferSize - head);
      System.arraycopy(data, 0, temp, bufferSize - head, head);
      data = temp;
      head = 0;      
      bufferSize = newSize;
    }     
    data[(head + queueSize) % bufferSize] = obj;
    ++queueSize;
    notify();
  }  

  private int bufferSize;
  private int increment;
  private int head;
  private int queueSize;
  private byte[][] data;

}

public class XTest {
  
  Display dpy;
  XEventDataQueue eventQueue;
  int window;
  int gc;
  int wndWidth;
  int wndHeight;
  int colorFg;
  int colorBg;
  int colorTest;
  int pixmap = 0;
  int pixmapMask = 0;
  XBitmapData xBitmap = null;
  PixmapData zPixmap = null;
  XFontInfo fontInfo = null;
  ColorTranslator ct = null;
  XImage image = null;
  
  void testDrawImage() {
    if (image != null) {
      image.draw(window, gc, 10, 10, 
                 0, 0, wndWidth, wndHeight);
      image.draw(window, gc, 10 + image.width(), 10 + image.height(),
                 0, 0, wndWidth, wndHeight);
    }
  }

  void testCopyArea() {
    dpy.changeGC(gc, X.SET_GC_FOREGROUND | X.SET_GC_FONT, colorBg, fontInfo, 0);
    dpy.fillRectangle(window, gc, 0, 0, wndWidth, wndHeight);
    
    dpy.setGCForeground(gc, colorTest);
    dpy.fillArc(window, gc, 0, 0, wndWidth, wndHeight, 90, 90);
/*
    if (pixmapMask != 0) {
      dpy.setGCClipping(gc, pixmapMask, 10, 10);      
      dpy.copyArea(pixmap, window, gc, 0, 0, xBitmap.width(), xBitmap.height(),  
                   10, 10);
      dpy.clearGCClipping(gc);      
    }  
    else {
      dpy.copyArea(pixmap, window, gc, 0, 0, xBitmap.width(), xBitmap.height(),  
                   10, 10);
    }               
*/                 
  }

  void testTextOutput() {
    dpy.changeGC(gc, X.SET_GC_FOREGROUND | X.SET_GC_FONT, colorBg, fontInfo, 0);
    byte[] text = 
      {(byte)'P', (byte)'r', (byte)'i', (byte)'v', (byte)'e', (byte)'t'};
    char[] text2 = {'H', 'e', 'l', 'l', 'o'};
    dpy.drawBytes(window, gc, 50, 50, text);             
    dpy.drawChars(window, gc, 50, 100, text2);             
    dpy.drawString(window, gc, 50, 300, "Nu poka");         
    
String veryLong = 
"0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789";
    dpy.drawString(window, gc, 10, 10, veryLong);         
  }
  
  void testDrawPoints() {
    dpy.setGCForeground(gc, colorFg);
    for (int i = 0; i < wndWidth; i += 5) {
      for (int j = 0; j < wndHeight; j += 5) {
        dpy.drawPoint(window, gc, i, j);
      }
    }
  }

  void testDrawGraphics() {
    
    dpy.setGCForeground(gc, colorFg);
    
    dpy.drawLine(window, gc, 0, 0, wndWidth/2, wndHeight/2);
    dpy.drawLine(window, gc, wndWidth/2, wndHeight/2, wndWidth - 1, 0);
    dpy.drawLine(window, gc, 0, wndHeight, wndWidth/2, 0);
    dpy.drawLine(window, gc, wndWidth/2, 0, wndWidth - 1, wndHeight - 1);
    dpy.drawLine(window, gc, wndWidth/2, wndHeight/2, wndWidth - 1, 0);

    dpy.setGCForeground(gc, colorTest);
    dpy.fillRectangle(window, gc, 0, 0, wndWidth/2, wndHeight/2);
    dpy.fillRectangle(window, gc, 
                      wndWidth/2, wndHeight/2,wndWidth/2, wndHeight/2);
    dpy.fillArc(window, gc, 0, 0, wndWidth, wndHeight, 100, 35);

    int[] xs = {80, 100, 120, 140};
    int[] ys = {100, 50, 100, 50};
    dpy.setGCForeground(gc, colorBg);
    dpy.drawPolygon(window, gc, 0, 0, xs, ys, 4);
    dpy.fillPolygon(window, gc, 10, 10, xs, ys, 4);
    dpy.setGCForeground(gc, colorTest);
    for (int i = 0; i < wndWidth; i += 5) {
      dpy.drawLine(window, gc, 0, 0, i, wndHeight - 1);
    }  
    for (int i = 0; i < wndHeight; i += 5) {
      dpy.drawLine(window, gc, 0, 0, wndWidth - 1, i);
    }  
  }

  void testDrawGraphics2() {
    dpy.setGCForeground(gc, colorBg);
    dpy.fillRectangle(window, gc, 0, 0, wndWidth, wndHeight);
    dpy.setGCForeground(gc, colorFg);
    dpy.drawLine(window, gc, 0, 0, wndWidth - 1, wndHeight - 1);
    dpy.fillArc(window, gc, 0, 0, wndWidth, wndHeight, 90, 90);
    dpy.setGCForeground(gc, colorTest);
    dpy.fillArc(window, gc, 0, 0, wndWidth, wndHeight, 90, -90);
    dpy.setGCForeground(gc, colorBg);
    dpy.fillArc(window, gc, 0, 0, wndWidth, wndHeight, 0, -90);
    dpy.setGCForeground(gc, colorTest);
    dpy.fillArc(window, gc, 0, 0, wndWidth, wndHeight, -90, -90);
    int[] xs = {80, 100, 120, 140};
    int[] ys = {100, 50, 100, 50};
    dpy.drawPolygon(window, gc, 0, 0, xs, ys, 4);
    dpy.fillPolygon(window, gc, 10, 10, xs, ys, 4);
  }
  void testGraphics() {
//    long time = System.currentTimeMillis();
    testCopyArea();
    testDrawImage();
//    testTextOutput();
//    testDrawGraphics();
//    testDrawGraphics2();
//    testDrawPoints();
//    time = System.currentTimeMillis() - time;
//    Tk.dbg("Test time: " + time + "ms");
  }
  
  void testGCCreation() {
    int[] t = new int[10];
    for (int k = 0; k < 10; k++) {
      t[k] = dpy.createGC(window, X.SET_GC_FOREGROUND, colorBg, null, 0);
    }
    for (int k = 0; k < 10; k++) {
      dpy.freeGC(t[k]);
    }
  }
  
  void initGraphics() {
    ct = dpy.colorTranslator();
    colorFg = ct.bestDevicePixel(0, 255, 0); 
    colorBg = ct.bestDevicePixel(128, 179, 220);
    colorTest = ct.bestDevicePixel(255, 0, 0);
    fontInfo = dpy.loadFont("Courier", java.awt.Font.PLAIN, 18);
    gc = dpy.createGC(window);

    xBitmap = new XBitmapData(dpy, 400, 400);
    zPixmap  = new PixmapData(dpy, 150, 150);
//    xBitmap.testFill();
//    zPixmap.testFill(ct);

    pixmap = dpy.createPixmap(dpy.frameRootWindowId(), 
                              dpy.frameWindowVisual().depth(),
                              xBitmap.width(), xBitmap.height());
    int pixmapGC = dpy.createGC(pixmap, X.SET_GC_FOREGROUND, 
                                colorTest, null, 0);
    xBitmap.draw(dpy, pixmap, pixmapGC, 0, 0);
    zPixmap.draw(dpy, pixmap, pixmapGC, 
                xBitmap.width() - zPixmap.width(),
                xBitmap.height() - zPixmap.height());
    dpy.freeGC(pixmapGC);                         
  }

  void onExpose(byte[] buf, int shift) throws IOException {
    int window = Tk.getInt(buf, shift + 4);
    int xClip = Tk.getUnsignedShort(buf, shift + 8);
    int yClip = Tk.getUnsignedShort(buf, shift + 10);
    int widthClip = Tk.getUnsignedShort(buf, shift + 12);
    int heightClip = Tk.getUnsignedShort(buf, shift + 14);
    if (gc == 0) {
      initGraphics();
    }
    testGraphics();
    Tk.dbg("Graphic end");
                
  } 

  void onConfigureNotify(byte[] buf, int shift) {
    int window = Tk.getInt(buf, shift + 4);
    wndWidth = Tk.getUnsignedShort(buf, shift + 20);
    wndHeight = Tk.getUnsignedShort(buf, shift + 22);
    Tk.dbg("New window size: " + wndWidth + "x" + wndHeight);
  }

  int cursorShape = X.XC_X_CURSOR;

  void eventLoop() throws IOException, XError{
    Tk.dbg(dpy.vendorData());

    mainLoop: 
    for (;;) {
      byte[] event = eventQueue.popHead();
      Command cmd = eventQueue.command;
      if (cmd != null) {
        synchronized(eventQueue) {
          eventQueue.command = null;
        }
        cmd.perform();
      }
      
      int opcode = 0x7F & event[0];
      switch (opcode) {
      case XEvent.CLIENT_MESSAGE:
        Tk.dbg("Client message");
        int windowToDelete = Tk.getInt(event, 4);
        if (window == windowToDelete) {
          if (Tk.getInt(event, 12) == dpy.wmDeleteWindowAtom()) {
            break mainLoop;
          }
        }  
        break;  
      case XEvent.CONFIGURE_NOTIFY:
        onConfigureNotify(event, 0);
        break;  
      case XEvent.EXPOSE:
        onExpose(event, 0);
        break;  
      case XEvent.KEY_PRESS:
        int keycode = Tk.getUnsignedByte(event, 1);
        int modifiers = Tk.getUnsignedShort(event, 28);
        int keysym = dpy.translateKeycodeToKeysym(keycode, modifiers);
        int state = dpy.mapKeyboardModifiers(1, 2, 4, 8, modifiers);
        Tk.dbg(Integer.toHexString(modifiers));

        Tk.dbg("Shift: " + ((state & 1) != 0 ? "on" : "off") + ", "
               + "Control: " + ((state & 2) != 0 ? "on" : "off") + ", "
               + "Alt: " + ((state & 4) != 0 ? "on" : "off") + ", "
               + "Meta: " + ((state & 8) != 0 ? "on" : "off"));
        Tk.dbg("Keycode = " + keycode + ", Keysym = " 
              + Integer.toHexString(keysym));
        int key = dpy.awtKey(keycode, modifiers);
        Tk.dbg("AWT key = " + key + " = '" + (char)key + "'");
        if (key == java.awt.Event.F12) {
          break mainLoop;
        }
        else if (key == 't') {
          xBitmap.test();
          zPixmap.test(dpy.colorTranslator());
          int pixmapGC = dpy.createGC(pixmap, X.SET_GC_FOREGROUND, 
                                      colorTest, null, 0); 
          xBitmap.draw(dpy, pixmap, pixmapGC, 0, 0);
          zPixmap.draw(dpy, pixmap, pixmapGC, 0, 0);
          dpy.freeGC(pixmapGC);               
          testGraphics();          
        }
        else if (key == '+') {
          dpy.setWindowSize(window, wndWidth + 10, wndHeight + 10);
        }
        else if (key == '-') {
          dpy.setWindowSize(window, wndWidth - 10, wndHeight - 10);
        }
        else if (key == '*') {
          dpy.setWindowPos(window, 0, 0);
        }
        else if (key == 'g') {
          gifTest();
        }
        else if (key >= '0' && key <= '9') {
          sysImageTest(key - '0');
        }
        else if (key == 'c') {
          dpy.setCursorFromShape(window, cursorShape);
          cursorShape += 2;
        }
        else if (key == 'C') {
          dpy.setCursorFromShape(window, cursorShape);
          cursorShape -= 2;
        }
        break;
      case XEvent.DESTROY_NOTIFY:
        break mainLoop;
      default:
        Tk.dbg("New event: " + XEvent.eventName(opcode));
      }
    }
  }

  void gifTest() {
    if (location != null) {
      try {
        java.net.URL url = new java.net.URL(location);
        GifImageDecoder g =
          new GifImageDecoder(url.openStream(), dpy, dpy.colorTranslator());
        if (image != null) {
          image.dispose();
        }
        image = new XImage(dpy, g.pixmapData.width(), g.pixmapData.height());
        image.putData(0, 0, g.pixmapData, g.transPositions);
        testGraphics();          
      }
      catch (Exception e) {
        e.printStackTrace();
      }
    }  
  }
  
  void sysImageTest(int i) {
    if (image != null) {
      image.dispose();
      image = null;
    }
    switch (i) {

      case 0: image = alertWarning(dpy); break;
      case 1: image = alertNotification(dpy); break;
      case 2: image = alertQuestion(dpy); break;
      case 3: image = scrollDownArrow(dpy); break;
      case 4: image = radioButtonOn(dpy); break;
      case 5: image = radioButtonOn(dpy); break;
      case 6: image = radioButtonOn(dpy); break;
      case 7: image = radioButtonOn(dpy); break;
      case 8: image = radioButtonOn(dpy); break;
      case 9: image = radioButtonOn(dpy); break;
    }
    
    testGraphics();          
  }


  String location;

  XTest(Display dpy, String location) {
    this.dpy = dpy;
    this.location = location;
    eventQueue = new XEventDataQueue(dpy);
    dpy.setEventConsumer(eventQueue);
    dpy.setupKeyboardSupport();

    wndWidth = 450;
    wndHeight = 450;
    window = dpy.createFrameWindow(0, 0, wndWidth, wndHeight);
    dpy.selectInput(window, 
                    X.EXPOUSURE_EVENT_MASK 
                    | X.KEY_PRESS_EVENT_MASK 
                    | X.BUTTON_PRESS_EVENT_MASK 
                    | X.BUTTON_RELEASE_EVENT_MASK 
//                    | X.POINTER_MOTION_EVENT_MASK 
                    | X.BUTTON_MOTION_EVENT_MASK  
                    | X.STRUCTURE_NOTIFY_EVENT_MASK
                    );
    dpy.setWindowTitle(window, "Test on " + dpy.displayName());
    gc = 0;
    dpy.mapWindow(window);
  }
  
  public void close() {
    
    if (image != null) {
      image.dispose();
    }
    
    if (gc != 0) {
      dpy.freeGC(gc);
    }  
    if (window != 0) {
      dpy.destroyWindow(window);
    }
    if (pixmapMask != 0) {
      dpy.freePixmap(pixmapMask);
    }
    if (pixmap != 0) {
      dpy.freePixmap(pixmap);
    }
    if (fontInfo != null) {
      dpy.freeFont(fontInfo);
    }
  }

  private static boolean should_show_usage(String[] args) {
    for (int i = 0, N = args.length; i != N; ++i) {
	  String arg = args[i];
	  if ("--".equals(arg)) { break; }
	  else if ("-h".equals(arg) || "--help".equals(arg)) { return true; }
	}
	return false;
  }
  
  private static void show_usage() {
    System.out.println
("Usage: java lib.x.XTest -display DISPLAY_NAME [GIF_URL]\n"
+"Under a typical shell DISPLAY_NAME will be simply $DISPLAY.\n"
+"If that does not work, try using 127.0.0.1:0 after xhost +127.0.0.1\n"
+"If GIF_URL is given, show gif image located at that URL, otherwise start\n"
+"showing a series of built-in images\n"
);
  }
  
  public static void main(String[] args) {
    if (should_show_usage(args)) { show_usage(); }
	else {
	  try {
    	Display dpy = new Display(args);
    	try {
          XTest test = new XTest(dpy, args.length > 0 ? args[0] : null);
          test.eventLoop();
          test.close();
    	}
    	finally {
          dpy.close();
    	}
      } 
      catch (Throwable e) {
    	e.printStackTrace(System.err);
      }
	}
  }



    static int pixelForMapColor(int index, long map[]) {
        if ((index % 2) == 0)
            return (int)(map[index / 2] >>> 32L);
        else
            return (int)(map[index / 2] & 0xffffffffL);
    }

    static XImage bitmapForEncoding(
        Display dpy, int w, int h, long rle[], long map[]) {
        
        int i, len, pix, pixCount, pixels[];
        long chunk, shift;

        pixels = new int[w * h];
        pixCount = 0;

        for (i = 0; i < rle.length; i++) {
            chunk = rle[i];
            shift = 56L;

            while (shift > 0L) {
                len = (int)((chunk >>> shift) & 0xffL);
                if (len == 0)
                    break;

                shift -= 8L;
                pix = pixelForMapColor((int)((chunk >>> shift) & 0xffL), map);

                while (len-- > 0)
                    pixels[pixCount++] = pix;

                shift -= 8L;
            }
        }

        if (pixCount != (w * h))
            throw new Error("Bad Image!");

        XImage img = new XImage(dpy, w, h);
        img.putRGB(0, 0, w, h, pixels, 0, w);
        return img;

    }

    static XImage alertWarning(Display dpy) {
        XImage bitmap;
        int width = 39;
        int height = 34;
        long map[] = {
            0xffc6c6c6ffcf9788L, 0xffc8bdbaffdf4219L, 0xffd8684affd57b63L,
            0xffe33001ffcbaaa1L, 0xffe1390dffdc5532L, 0xffd18e7cffcda095L,
            0xffde4c25ffcab3adL, 0xffd3846fffb59b95L, 0xffbeb1adffda5e3eL,
            0xffc2bbbaff913820L, 0xff972e14ff892308L, 0xffad867cff982d11L,
            0xffff633affe0512cL, 0xff882409ffa73618L, 0xffef5a33ff882309L,
            0xff90280eff984029L, 0xff831c01ffc13f1dL, 0xffa06657ffd67157L,
            0xff92361dff9a4d38L, 0xffd04825ffa47163L, 0xffb93b1aff8d2d13L,
            0xffb19188ff9f5744L, 0xffb9a6a1ffd84d28L, 0xffb03819ff95432dL,
            0xffa97b6ffff75f36L
        };
        long rle[] = {
            0x1300010101022500L, 0x0103010424000105L, 0x0206010722000102L,
            0x0108020601092200L, 0x01040406010a2000L, 0x010b050601030102L,
            0x1f0001090206010cL, 0x030601051e00010aL, 0x0206010801020109L,
            0x02060108010d1c00L, 0x010201030206010eL, 0x0100010703060104L,
            0x1c0001050206010cL, 0x030001040306010bL, 0x1a00010d01080206L,
            0x010b0100010f0110L, 0x010201080206010cL, 0x1a00011102060111L,
            0x0112011301140115L, 0x011601050306010aL, 0x1800010b0306010dL,
            0x0116011701180119L, 0x011a010001030206L, 0x010801021700010cL,
            0x0206010501000116L, 0x011b0118011c011dL, 0x0100010103060105L,
            0x1600010a02060108L, 0x01020100010f011eL, 0x01180119011f0200L,
            0x01090306010d1400L, 0x010201080206010aL, 0x0300012001180121L,
            0x01220200010d0306L, 0x0109140001230206L, 0x0109040001240119L,
            0x011b011603000123L, 0x0306010b1200010dL, 0x0306010b04000125L,
            0x01260114010f0300L, 0x0102010802060103L, 0x1200010902060104L,
            0x0500012701280120L, 0x0500010e0306010eL, 0x1000010b02060108L,
            0x010d0500010f0120L, 0x01290600010c0206L, 0x010801020f000103L,
            0x0206010506000110L, 0x012001250600010bL, 0x030601230e00010eL,
            0x020601030800011aL, 0x0127070001110306L, 0x01070c0001020108L,
            0x020601010800012aL, 0x01120700010d0306L, 0x01090c0001040206L,
            0x0109130001050306L, 0x01010a0001070306L, 0x01070800012a011fL,
            0x012b011207000102L, 0x0108020601030a00L, 0x0109020601040800L,
            0x012c0115012d012eL, 0x012f0800010a0306L, 0x0105080001010206L,
            0x0108010208000130L, 0x011b011801310120L, 0x0900010902060108L,
            0x0102070001030206L, 0x010e09000110011dL, 0x0128011b012b0900L,
            0x010b030601040600L, 0x0105020601030b00L, 0x012c012201300112L,
            0x0a0001040306010bL, 0x0400010d01080206L, 0x010b1900010d0108L,
            0x0206010904000104L, 0x2206010a0200010bL, 0x2306010301020100L,
            0x010924060105010dL, 0x2505010e00000000L
        };

        bitmap = bitmapForEncoding(dpy, width, height, rle, map);

        return bitmap;
    }

    static XImage alertQuestion(Display dpy) {
        XImage bitmap;
        int width = 35;
        int height = 33;
        long map[] = {
            0xffc6c6c6ffb7bfadL, 0xff91ad6fff7aa24aL, 0xff6b9b32ff4d8d01L,
            0xff639825ff89a963L, 0xffafbba1ff98b17cL, 0xff5c9419ffbec2baL,
            0xffa8b895ff54900dL, 0xff82a657ff729f3eL, 0xffadb5a4ffa5af98L,
            0xffb5baafff688643L, 0xff416a10ff59802bL, 0xff4e751fff4f7522L,
            0xff83986bff8c9e76L, 0xff547a25ff7da44dL, 0xff98bf69ff88af58L,
            0xff5e852fff587b2fL, 0xffbec0bbff476f18L, 0xff8db45eff729a42L,
            0xff6c943cff628934L, 0xff7a925dffa0b488L, 0xff82aa53ff5f8137L,
            0xff587f2bff93ba63L, 0xff486f18ff699039L, 0xff4d751dff698745L,
            0xff779f47ff608038L, 0xff587b2dff60803aL, 0xff829868ff92a37eL,
            0xff527a24ff507523L, 0xff4e741fff476f17L, 0xff94a381ff698646L,
            0xff718c51ff587f29L, 0xff678f3700000000L
        };
        long rle[] = {
            0x0b00010101020103L, 0x0104040501060104L, 0x0107010815000109L,
            0x01040c05010a0107L, 0x010b1000010c0106L, 0x1005010d01020e00L,
            0x010e14050104010bL, 0x0a00010b01040605L, 0x01060107010c010bL,
            0x0300010c0109010fL, 0x0605010d01080800L, 0x010b010605050103L,
            0x0101030001100311L, 0x01120200010b0107L, 0x010d0405010d0108L,
            0x0700010f0405010aL, 0x010c020001110113L, 0x0114041501160117L,
            0x01180200010b0104L, 0x0405010a010b0500L, 0x010204050106010bL,
            0x02000119011a011bL, 0x061c011d011e011fL, 0x0120020001030405L,
            0x010f04000101010dL, 0x03050106010b0300L, 0x0121022201230224L,
            0x011b041c01250126L, 0x0300010304050127L, 0x030001030305010dL,
            0x010103000111011aL, 0x0128012101260218L, 0x0129012a031c012bL,
            0x012c040001040305L, 0x010a0200010b010dL, 0x0305010204000119L,
            0x012d012e01110400L, 0x012f0130031c0115L, 0x0111030001010405L,
            0x0127010001090305L, 0x010a050001190114L, 0x0131050001180124L,
            0x031c012c01200400L, 0x01030305010f0100L, 0x010f0305010e0d00L,
            0x01320128021c0123L, 0x0133050001080305L, 0x010d0100010a0305L,
            0x010c0c000134012aL, 0x021c0130012c0112L, 0x0600010d03050108L,
            0x04050c0001350136L, 0x022b012d01370110L, 0x070001040305010cL,
            0x04050b0001350136L, 0x012b012801380126L, 0x0120080001040305L,
            0x012704050a000111L, 0x0139012b012d0137L, 0x01110a00010f0305L,
            0x010904050a00012fL, 0x0130012d011f0120L, 0x0b0001040305010cL,
            0x010d030501080900L, 0x0129013001140120L, 0x0c00010a0305010cL,
            0x0104030501020900L, 0x0134012101350c00L, 0x0101040501000102L,
            0x0305010618000107L, 0x0305010401000101L, 0x0405010c0900013aL,
            0x013b011901200900L, 0x010b010d03050109L, 0x0200010f0305010aL,
            0x010b0700013c012aL, 0x01230125011f0120L, 0x0800010e0305010dL,
            0x010b0200010c0405L, 0x0103060001120139L, 0x012b021c013d013aL,
            0x0700010904050107L, 0x0400010e04050103L, 0x05000111011a031cL,
            0x013e011806000109L, 0x040501060500010bL, 0x01060405010f010bL,
            0x040001370130011cL, 0x011d012c01120500L, 0x01070405010d0108L,
            0x06000108010d0405L, 0x010d010903000110L, 0x01370114012c013aL,
            0x0400010c01040505L, 0x010908000108010dL, 0x0505010a010e010cL,
            0x0600010101020106L, 0x060501090a00010bL, 0x0106080501060304L,
            0x0805010d010c0d00L, 0x0107010d1105010fL, 0x010b0f0001010103L,
            0x010d0d050104010cL, 0x1300010b0109010fL, 0x010a070501040102L,
            0x01011a00040c010bL, 0x0f00000000000000L
        };

        bitmap = bitmapForEncoding(dpy, width, height, rle, map);

        return bitmap;
    }

    static XImage alertNotification(Display dpy) {
        XImage bitmap;
        int width = 37;
        int height = 34;
        long map[] = {
            0xffc6c6c6ffa4a5c8L, 0xff8183caff7073caL, 0xff5f62cbff4549cdL,
            0xff3d41cdff676acbL, 0xff9b9cc8ff4e51ccL, 0xffbdbec6ff797bcaL,
            0xffb5b5c7ff565accL, 0xff8a8cc9ffacadc7L, 0xffb4b4bfff7d7eacL,
            0xffa1a2b9ff8f90b2L, 0xff3a3d96ff5255c0L, 0xff5457c8ff40429dL,
            0xff686aa7ff686ceaL, 0xff7478ffff7074f8L, 0xff3e419dff9899b6L,
            0xff494ca9ff9294c9L, 0xffababbcff3f429dL, 0xff4e51b9ff8687afL,
            0xff5457a1ff494cb2L, 0xff6c70f1ff3b3e97L, 0xffbdbdc3ff3b3d97L,
            0xff343792ff5e61a4L, 0xff4a4c9fff7a7cadL, 0xff3c3f9fff6064ddL,
            0xff5355a3ff6a6ca5L, 0xff484baaff585bcfL, 0xff7173aaff7475a9L,
            0xff5053c1ff6468e4L, 0xff393c97ff5053b8L, 0xff5c5fa6ff5c60d6L,
            0xff42449bff4346a4L, 0xff5658a0ff464aabL, 0xff4d4fb000000000L
        };
        long rle[] = {
            0x0d00010101020103L, 0x0404010301020101L, 0x1800010101030105L,
            0x0b06010701081300L, 0x0101010410060109L, 0x01010f00010a010bL,
            0x14060107010a0c00L, 0x010c010d06060109L, 0x010301020401010eL,
            0x0103010906060105L, 0x010f0a00010f0105L, 0x0506010401010300L,
            0x0110021101120300L, 0x010f010305060105L, 0x010f0800010a0105L,
            0x0406010901010400L, 0x0113011401150116L, 0x011701180400010fL,
            0x010d04060105010fL, 0x0700010d04060103L, 0x010a040001100114L,
            0x0119021a011b011cL, 0x011d0500010b0406L, 0x0109010a05000102L,
            0x0406010306000112L, 0x011e041a01160111L, 0x0600011f04060103L,
            0x0400010c04060107L, 0x070001200121041aL, 0x0122012307000103L,
            0x0406010103000103L, 0x03060105010a0800L, 0x012401250126011bL,
            0x0116012701280800L, 0x010d030601040200L, 0x010a01050306011fL,
            0x0900012801180129L, 0x012a012b01200900L, 0x01010406010c0100L,
            0x0108030601091a00L, 0x010d0306010e0100L, 0x01030306010b1a00L,
            0x010e030601040100L, 0x0104030601010c00L, 0x0128021201100a00L,
            0x010c030601050100L, 0x0406010a09000128L, 0x0123012c012a021eL,
            0x011401130a000406L, 0x010004060900012dL, 0x0114012a012e0119L,
            0x021a012f01300a00L, 0x01090306010f0406L, 0x0900011302110131L,
            0x0132021a01330134L, 0x0a00010d03060101L, 0x04060c0001350136L,
            0x021a0132011d0a00L, 0x04060100010d0306L, 0x01010b00012c0137L,
            0x011a011b012a0128L, 0x0900010a04060100L, 0x01070306010e0a00L,
            0x01100138021a012fL, 0x01240a00011f0306L, 0x01040100011f0306L,
            0x010d0a0001230139L, 0x021a012201230a00L, 0x01070306010b0100L,
            0x010c040601010900L, 0x013a013b021a0117L, 0x01200900010c0406L,
            0x010f020001070306L, 0x010d0900012a011bL, 0x011a0119013c0a00L,
            0x0107030601090300L, 0x0101040601020700L, 0x011d013d021a0116L,
            0x01340900011f0406L, 0x011f040001070406L, 0x011f060001350116L,
            0x021a013201110112L, 0x0111012b01130400L, 0x010f040601040500L,
            0x010a01050406011fL, 0x050001300137021aL, 0x0133013d022a013eL,
            0x0128030001080105L, 0x03060105010f0600L, 0x010f05060107010aL,
            0x03000134013f0216L, 0x01400114012b0112L, 0x0300010a010b0506L,
            0x011f0800011f0506L, 0x01050102010a0200L, 0x0113021101130110L,
            0x0400011f01090506L, 0x011f0a0001080105L, 0x050601050103011fL,
            0x01010400010f011fL, 0x010301050606011fL, 0x0c00010f010d1406L,
            0x010901010f000102L, 0x010511060102010aL, 0x1100010a01020109L,
            0x0c060105010b010cL, 0x1600010f01020104L, 0x0109040601090104L,
            0x010201010d000000L
        };

        bitmap = bitmapForEncoding(dpy, width, height, rle, map);

        return bitmap;
    }

    static XImage radioButtonOn(Display dpy) {
        XImage bitmap;
        int width = 15;
        int height = 15;
        long map[] = {
            0x00c0c0c0ffb2b2b2L, 0xffb7b7b7ffbcbcbcL, 0xffe0e0e0fff0f0f0L,
            0xffe1e1e1ffd2d2d2L, 0xffadadadffc3c3c3L, 0xfff4f4f4ffe9e9e9L,
            0xffdadadaffcbcbcbL, 0xffb4b4b4ff8f8f8fL, 0xff979797ffa5a5a5L,
            0xff696969ff808080L, 0xff818180ff656565L, 0xff929191ffa7a7a7L,
            0xff9e9e9effa1a2a2L, 0xffb7b8b8ffcececeL, 0xff9c9c9cffc9c8c8L,
            0xffdededeff404040L, 0xffd6d6d6ffadadacL, 0xffc3c2c2ffd9d9d9L,
            0xff7f7f7fffa7a8a7L, 0xffbdbdbdffd3d3d3L, 0xffa2a2a1ffb8b7b8L,
            0xffcdceceffe3e4e4L, 0xffa2a2a200000000L
        };
        long rle[] = {
            0x0500010102020103L, 0x0900010101040105L, 0x0106010701000108L,
            0x010001090600020aL, 0x010b010c0107010dL, 0x010e010f01020109L,
            0x03000102020a0100L, 0x0310010901020111L, 0x0112010301090200L,
            0x020a010004130114L, 0x01000108010f0115L, 0x0200010e010a0100L,
            0x0413011001160117L, 0x0111010f01150118L, 0x0100010901050100L,
            0x031302100119011aL, 0x011b010f02150100L, 0x0109010601000213L,
            0x0210011c0101011dL, 0x011e01130115011fL, 0x0100010201200100L,
            0x0213021001210122L, 0x0123010b01240115L, 0x011f010001030100L,
            0x010d010002100125L, 0x01260127010b0113L, 0x0215011f02000102L,
            0x01080102010e0128L, 0x0129012a012b010fL, 0x0215011f01130300L,
            0x0111010f02180110L, 0x010f01240315011fL, 0x0500010e01120515L,
            0x011f01150800012cL, 0x012402150112012cL, 0x1300000000000000L
        };

        bitmap = bitmapForEncoding(dpy, width, height, rle, map);

        return bitmap;
    }

    static XImage scrollDownArrow(Display dpy) {
        XImage bitmap;
        int width = 16;
        int height = 16;
        long map[] = {
            0x00c6c6c6ff808080L, 0xff66666600000000L
        };
        long rle[] = {
            0x63000a0107000101L, 0x0602010109000101L, 0x040201010b000101L,
            0x020201010d000201L, 0x5700000000000000L
        };

        bitmap = bitmapForEncoding(dpy, width, height, rle, map);

        return bitmap;
    }

}
