package lib.x.util;

public class ObjectQueue
{
  public static final int DEFAULT_INCREMENT = 4;
  public static final int DEFAULT_INIT_SIZE = DEFAULT_INCREMENT;

  public ObjectQueue() { this(DEFAULT_INIT_SIZE); }
  public ObjectQueue(int initSize) { this(initSize, DEFAULT_INCREMENT); }
  
  public ObjectQueue(int initSize, int increment)
  {
    this.increment = increment;
    bufferSize = initSize;
    data = new Object[bufferSize];    
    head = 0;
    queueSize = 0;
  }
  
  public final boolean empty() { return queueSize == 0; }
  public final int size() { return queueSize; }
  public final Object elementAt(int i) 
  { 
    if (i > queueSize) throw new ArrayIndexOutOfBoundsException(i);
    return data[(head + i) % bufferSize]; 
  }

  public final Object peekHead()
  {
    if (empty()) throw new EmptyQueueException();
    return data[head];
  }  

  public final Object peekTail()
  {
    if (empty()) throw new EmptyQueueException();
    return data[(head + queueSize - 1) % bufferSize];
  }  

  public final Object popHead()
  {
    if (empty()) throw new EmptyQueueException();
    Object ret = data[head];
    head =  (head + 1) % bufferSize;
    --queueSize;
    return ret;
  }  

  public final void putToTail(Object obj)
  {
    if (queueSize == bufferSize)
    {
      int newSize = (increment == 0) ? bufferSize * 2 : bufferSize + increment;
      Object temp[] = new Object[newSize];
      System.arraycopy(data, head, temp, 0, bufferSize - head);
      System.arraycopy(data, 0, temp, bufferSize - head, head);
      data = temp;
      head = 0;      
      bufferSize = newSize;
    }     
    data[(head + queueSize) % bufferSize] = obj;
    ++queueSize;
  }  

  private int bufferSize;
  private int increment;
  private int head;
  private int queueSize;
  private Object data[];

}
