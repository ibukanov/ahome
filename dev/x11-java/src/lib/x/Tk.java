package lib.x;


public class Tk {  // Tk here means X Tool Kit

  public static final boolean dbg = true;
  
  public static void dbg(String s) {
    if (dbg) { System.err.println(s); }  
  }
  public static void dbg(int i) {
    if (dbg) { System.err.println(i); }  
  }

  public static void assert(boolean condition) {
    if (X.DEBUG && !condition) { throw new XProgErr("ASSERTION FAILED"); }  
  }

  public static void outMessage(String title, String body) {
    System.err.println(title + ": " + body);   
  }
  
  public static void warning(String str) {
    System.err.println("Warning: " + str);   
  }
  
  public static int getInt(byte[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 4 == 0);  
    return ((0xFF & array[offset]) << 24) | ((0xFF & array[++offset]) << 16) 
           | ((0xFF & array[++offset]) << 8) | (0xFF & array[++offset]);
  }

  public static int getInt(short[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 2 == 0);  
    return ((0xFFFF & array[offset]) << 16) | (0xFFFF & array[++offset]); 
  }

  public static short getShort(byte[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 2 == 0);  
    return (short)(((0xFF & array[offset]) << 8) | (0xFF & array[++offset]));
  }

  public static int getUnsignedShort(byte[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 2 == 0);  
    return ((0xFF & array[offset]) << 8) | (0xFF & array[++offset]);
  }

  public static int getUnsignedByte(byte[] array, int offset) {
    return 0xFF & array[offset];
  }

  public static void putInt(int i, byte[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 4 == 0);  
    array[offset]   = (byte)(i >> 24);
    array[++offset] = (byte)(i >> 16);
    array[++offset] = (byte)(i >> 8);
    array[++offset] = (byte)(i);
  }

  public static void putInt(int i, short[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 2 == 0);  
    array[offset]   = (short)(i >> 16);
    array[++offset] = (short)(i);
  }

  public static void putShort(short s, byte[] array, int offset) {
    putShort((int)s, array, offset);
  }

  public static void putShort(int i, byte[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 2 == 0);  
    array[offset]   = (byte)(i >> 8);
    array[++offset] = (byte)(i);
  }

  public static void putByte(byte b, byte[] array, int offset) {
    array[offset] = b;
  }

  public static void putByte(int i, byte[] array, int offset) {
    array[offset] = (byte)i;
  }

// The following function helps to avoid a creation of new String object
  public static int printInt(int n, byte[] buf) {
    boolean putSign = false;
    if (n < 0) {
      n = -n;
      putSign = true;
    }  
    int count = 0;
    while (n >= 10) {
      buf[count++] = (byte)Character.forDigit(n % 10, 10);
      n = n / 10;
    }
    buf[count] = (byte)Character.forDigit(n, 10);
    int i = 0, j = count;
    while (i < j) {
      byte temp = buf[i];
      buf[i++] = buf[j];
      buf[j--] = temp;
    }
    if (putSign) {
      for (i = count; i >= 0; --i)   {
        buf[i + 1] = buf[i];
      }
      buf[0] = (byte)'-';
      count++; 
    }
    return count + 1;
  }     
        
  public static int lowPart(long sym) {
    return (int)(sym & 0xFFFFFFFFL);
  }

  public static int upperPart(long sym) {
    return (int)(sym >>> 32);
  }
  
  public static long makeLong(int upper, int low) {
    return (((long)upper) << 32) | (0xFFFFFFFFL & (long)low);
  }
  
  public static byte[] lowBytes(String s) {
    int len = s.length();
    byte[] ret = new byte[len];
    s.getBytes(0, len, ret, 0);
    return ret;
  }
  
  public static int pad4(int i) {
    return (4 - i & 0x3) & 0x3;
  }

  public static int roundup(int i, int v) {
    return (i + v - 1) / v * v;
  }

  public static int roundupToDword(int i) {
    return (i + 3) & ~0x3;
  }

  public static int bound(int i, int min, int max) {
    return (i < min) ? min : (i > max) ? max : i;
  }

  public static int boundToByte(int i) {
    return bound(i, X.BYTE_MIN, X.BYTE_MAX);
  }

  public static int boundToUnsignedByte(int i) {
    return ((i & ~0xFF) == 0) ? i : bound(i, 0, X.UBYTE_MAX);
  }

  public static int boundToPositiveByte(int i) {
    return bound(i, 1, X.UBYTE_MAX);
  }

  public static int boundToShort(int i) {
    return bound(i, X.SHORT_MIN, X.SHORT_MAX);
  }

  public static int boundToUnsignedShort(int i) {
    return ((i & ~0xFFFF) == 0) ? i : bound(i, 0, X.USHORT_MAX);
  }

  public static int boundToPositiveShort(int i) {
    return bound(i, 1, X.USHORT_MAX);
  }
  
  public static int maskLength(int n) {
    int length = 0;
    for (; n != 0; n >>>= 1) {
      if ((n & 1) != 0) { ++length; }
    }
    return length;
  }

  public static int maskLength(short n) {
    return maskLength(n & 0xFFFF);
  }

  public static int maskLength(byte n) {
    return maskLength(n & 0xFF);
  }

  public static int calculateFirstBitForMask(int mask) {
    int i;
    for (i = 1; (i & mask) == 0; i <<= 1) { }
    return i;
  }
  
  public static int shiftOutRightZeroBits(int i) {
    while ((i & 1) == 0) {
      i >>>= 1;
    }
    return i;
  }

  public static int calculateShiftForMask(int mask) {
    int i;
    for (i = 0; ((1 << i) & mask) == 0; ++i) { }
    return i;
  }

  public static int mapIntensity(int i, int maxSource, int maxDestination) {
    return (2 * i * maxDestination + maxSource) / (2 * maxSource);
  }

  public static int mapIntensity(int i, int minSource, int maxSource, 
                                 int minDestination, int maxDestination) {
    int scale = maxSource - minSource;
    return minDestination + 
      (2 * (i - minSource) * (maxDestination - minDestination) + scale) 
        / (2 * scale);
  }


  static int xReplyDataSize(byte[] data) {
    return xReplyDataSize(data, 0);
  }

  static int xReplyDataSize(byte[] data, int shift) {
    return 4 * getInt(data, shift + X.REPLY_DATA_SIZE_OFFSET);
  }

  static long buildAnswerNumber(long base, byte[] data, int shift) {
    return (base & (~0xFFFFL)) | getUnsignedShort(data, shift + 2);
  }

  static long buildAnswerNumber(long base, byte[] data) {
    return buildAnswerNumber(base, data, 0);
  }

  static boolean isAnswerNumber(long number, byte[] data, int shift) {
    return (number & 0xFFFFL) == getUnsignedShort(
                                    data, shift + X.REPLY_NUMBER_OFFSET);
  }

  static boolean isAnswerNumber(long number, byte[] data) {
    return isAnswerNumber(number, data, 0);
  }

  public static int xEventOpcode(byte[] event) {
    int shift = 0;
    return 0x7F & event[shift];
  }
  
}
