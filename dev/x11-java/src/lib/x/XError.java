package lib.x;

import java.io.*; 
//import lib.x.error.*; 

public class XError extends Throwable {

  public static final int X_ERROR_REQUEST        = 1; 
  public static final int X_ERROR_VALUE          = 2; 
  public static final int X_ERROR_WINDOW         = 3; 
  public static final int X_ERROR_PIXMAP         = 4; 
  public static final int X_ERROR_ATOM           = 5; 
  public static final int X_ERROR_CURSOR         = 6; 
  public static final int X_ERROR_FONT           = 7; 
  public static final int X_ERROR_MATCH          = 8; 
  public static final int X_ERROR_DRAWABLE       = 9; 
  public static final int X_ERROR_ACCESS         = 10; 
  public static final int X_ERROR_ALLOC          = 11; 
  public static final int X_ERROR_COLORMAP       = 12; 
  public static final int X_ERROR_GCONTEXT       = 13; 
  public static final int X_ERROR_IDCHOICE       = 14; 
  public static final int X_ERROR_NAME           = 15; 
  public static final int X_ERROR_LENGTH         = 16; 
  public static final int X_ERROR_IMPLEMENTATION = 17; 
  
  private static final String  errorNames[] = {
    "X_ERROR_REQUEST",
    "X_ERROR_VALUE",
    "X_ERROR_WINDOW",
    "X_ERROR_PIXMAP",
    "X_ERROR_ATOM",
    "X_ERROR_CURSOR",
    "X_ERROR_FONT",
    "X_ERROR_MATCH",
    "X_ERROR_DRAWABLE",
    "X_ERROR_ACCESS",
    "X_ERROR_ALLOC",
    "X_ERROR_COLORMAP",
    "X_ERROR_GCONTEXT",
    "X_ERROR_IDCHOICE",
    "X_ERROR_NAME",
    "X_ERROR_LENGTH",
    "X_ERROR_IMPLEMENTATION",
  };
  
  public static String errorName(int opcode) {
    if (opcode < X_ERROR_REQUEST || opcode > X_ERROR_IMPLEMENTATION) {
      return "Unknown(" + opcode + ")";
    }
    return errorNames[opcode - X_ERROR_REQUEST];
  }

  int opcode;
  long sequenceNumber;
  int errorValue;
  int minorOpcode;
  int majorOpcode;

  public final int opcode() { return opcode; }
  public final long sequenceNumber() { return sequenceNumber; }
  public final int errorValue() { return errorValue; } 
  public final int minorOpcode() { return minorOpcode; }
  public final int majorOpcode() { return majorOpcode; }

  public XError(int opcode, byte data[], int shift, long number) {
    sequenceNumber = number;
    this.opcode = opcode;
    errorValue  = Tk.getInt(data, shift + 4);
    minorOpcode = Tk.getUnsignedShort(data, shift + 8);
    majorOpcode = Tk.getUnsignedByte(data, shift + 10);
  }

  public static XError create(byte data[], int shift, long number) {
    int opcode = Tk.getUnsignedByte(data, shift + 1);
    XError e = new XError(opcode, data, shift, number);
    return e;
  }

  public String toString() {
    StringBuffer b = new StringBuffer(errorName(opcode));
    b.append(" for request " + sequenceNumber);
    b.append(", errorValue=0x" + Integer.toHexString(errorValue));
    b.append(", majorOpcode=" + majorOpcode);
    b.append(", minorOpcode=" + minorOpcode);
    return b.toString();
  }

}
