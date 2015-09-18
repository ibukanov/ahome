package lib.x;

import java.io.*;

public interface XEventConsumer {
  void xEvent(byte[] data, int shift);
  void xEvents(byte[] data, int[] shifts, int count);
  void performLater(Command command);
}

