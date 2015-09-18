package lib.x.util;

public class IntStack
{
  public static final int DEFAULT_INCREMENT = 4;
  public static final int DEFAULT_INIT_SIZE = DEFAULT_INCREMENT;

  public IntStack() { this(DEFAULT_INIT_SIZE); }
  public IntStack(int initSize) { this(initSize, DEFAULT_INCREMENT); }
  
  public IntStack(int initSize, int increment)
  {
    this.increment = increment;
    top = 0;
    bufferSize = initSize;
    data = new int[bufferSize];    
  }
  
  public final boolean empty() { return top == 0; }
  public final int size() { return top; }
  public final int elementAt(int i) { return data[i]; }

  public final void push(int i)
  {
    if (top == bufferSize) 
    {
      int newSize = (increment == 0) ?  bufferSize * 2 : bufferSize + increment;
      int temp[] = new int[newSize];
      System.arraycopy(data, 0, temp, 0, bufferSize);
      data = temp;
      bufferSize = newSize;
    }
    data[++top] = i;
  }
  
  public final int peek()
  {
    if (empty()) throw new EmptyStackException();
    return data[top - 1];
  }  

  public final int pop()
  {
    if (empty()) throw new EmptyStackException();
    return data[--top];
  }  

  private int bufferSize;
  private int increment;
  private int top;
  private int data[];

}
