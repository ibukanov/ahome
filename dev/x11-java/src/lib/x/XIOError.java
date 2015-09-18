package lib.x;

import java.io.*;

public class XIOError extends Error {
  private IOException io = null;
  public XIOError() { }
  public XIOError(String s) { super(s); }
  public XIOError(IOException io)  { this.io = io; }
  public String toString() {
    if (io == null) {
      return super.toString();
    }
    else {
      StringBuffer buf = new StringBuffer(getClass().getName());
      buf.append(" for ");
      buf.append(io.getClass().getName());
      String msg = io.getMessage();
      if (msg != null) {
        buf.append(": ");
        buf.append(msg);
      }       
      return buf.toString();
    }
  }
}
