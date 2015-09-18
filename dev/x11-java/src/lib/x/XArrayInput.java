package lib.x;

public class XArrayInput extends XInput {
  private byte[] data;
  private int cursor;
  private int maxCursor;

  public XArrayInput() {
    data = null;
    cursor = 0;
    maxCursor = 0;
  }
   
  public XArrayInput(byte[] array) { 
    this(array, 0, array.length);
  }

  public XArrayInput(byte[] array, int offset, int size) { 
    bindWith(array, offset, cursor);
  }

  public final void bindWith(byte[] array) {
    bindWith(array, 0, array.length);
  }
   
  public final void bindWith(byte[] array, int offset, int size) { 
    data = array;
    cursor = offset;
    maxCursor = size;
  }  
    
  protected int readImpl() {
    if (X.DEBUG) Tk.assert(cursor < maxCursor);
    return 0xFF & data[cursor++];  
  }
    
  protected void readImpl(byte[] array, int offset, int length) {
    int newCursor = cursor + length;
    if (X.DEBUG) Tk.assert(newCursor <= maxCursor);
    System.arraycopy(data, cursor, array, offset, length);  
    cursor = newCursor;
  }
    
  protected void skipImpl(int n) {
    cursor += n;
    if (X.DEBUG) Tk.assert(cursor <= maxCursor);
  }
    
  protected int availableImpl() {
    return maxCursor - cursor;
  }

}

