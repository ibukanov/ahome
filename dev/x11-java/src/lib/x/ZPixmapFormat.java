package lib.x;

import java.io.*;

public class ZPixmapFormat {
  
  int depth;
  int bitsPerPixel;
  int scanlinePad;

  public final int depth() { return depth; }
  public final int bitsPerPixel() { return bitsPerPixel; }
  public final int scanlinePad() { return scanlinePad; }
  
  ZPixmapFormat(XInput input) {
    depth        = input.readUnsignedByte();
    bitsPerPixel = input.readUnsignedByte();
    scanlinePad  = input.readUnsignedByte();
    input.skip(5);
  }

}

