package lib.x;

import java.io.*;

public abstract class XInput {

  protected abstract int availableImpl();
  protected abstract int readImpl();
  protected abstract void readImpl(byte[] array, int off, int length);
  protected abstract void skipImpl(int n);

  private long bytesCount;
  public final long bytesCount() { return bytesCount; }
  
  public XInput() { }
  
  public final int read() {
    int ret = readImpl();
    ++bytesCount;
    return ret;
  }

  public final void read(byte[] array, int offset, int length) {
    readImpl(array, offset, length);
    bytesCount += length;
  }

  public final void read(byte[] array) {
    read(array, 0, array.length);
  }
  
  public final byte[] readBytes(int length) {
    byte[] array = new byte[length];
    read(array);
    return array;
  }
  
  public final void readInts(int[] array, int offset, int length) {
    while (length > 0) {
      --length;
      array[offset++] = readInt();    
    }
  }

  public final void readInts(int[] array) {
    readInts(array, 0, array.length);
  }
  
  public final int[] readInts(int length) {
    int[] array = new int[length];
    readInts(array);
    return array;
  }
  
  public final void readShorts(short[] array, int offset, int length) {
    while (length > 0) {
      --length;
      array[offset++] = readShort();    
    }
  }

  public final void readShorts(short[] array) {
    readShorts(array, 0, array.length);
  }
  
  public final short[] readShorts(int length) {
    short[] array = new short[length];
    readShorts(array);
    return array;
  }
  
  public final void skip(int n) {
    if (X.DEBUG) Tk.assert(n >= 0);
    if (n > 0) {
      skipImpl(n);
      bytesCount += n;
    }  
  }

  public final int available() {
    return availableImpl();
  }

  public final boolean readBoolean() {
    return read() != 0;
  }

  public final byte readByte() {
    return (byte)read();
  }

  public final int readInt() {
    if (X.DEBUG) Tk.assert(bytesCount % X.INT_SIZE == 0);
    return (read() << 24) | (read() << 16) | (read() << 8) | read();
  }

  public final short readShort() {
    if (X.DEBUG) Tk.assert(bytesCount % X.SHORT_SIZE == 0);
    return (short)((read() << 8) | read());
  }

  public final int readUnsignedByte() {
    return read();
  }

  public final int readUnsignedShort() {
    if (X.DEBUG) Tk.assert(bytesCount % X.SHORT_SIZE == 0);
    return (read() << 8) | read();
  }

  public final void pad4() {
    skip(Tk.pad4((int)bytesCount));
  }

}

