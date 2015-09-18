package lib.x;

import java.io.*;

public abstract class XOutput {

  protected abstract void writeImpl(int b);
  protected abstract void writeImpl(byte  b[], int  off, int len);
  protected abstract void flushImpl();


  private long bytesCount = 0;
  public final long bytesCount() { return bytesCount; }

  public XOutput() { }  

  public final void write(int b) {
    writeImpl(b);
    ++bytesCount;
  }
    
  public final void write(byte b[], int off, int len) {
    writeImpl(b, off, len);
    bytesCount += len;
  }
  
  public final void write(byte b[]) {
    write(b, 0, b.length);
  }

  public final void write(int b[], int off, int len) {
    while (len > 0) {
      --len;
      writeInt(b[off++]);
    }
  }
  
  public final void write(int b[]) {
    write(b, 0, b.length);
  }

  public final void write(short b[], int off, int len) {
    while (len > 0) {
      --len;
      writeShort(b[off++]);
    }
  }
  
  public final void write(short b[]) {
    write(b, 0, b.length);
  }

  public final void flush() {
    flushImpl();
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













