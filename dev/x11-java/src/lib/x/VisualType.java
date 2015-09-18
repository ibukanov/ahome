package lib.x;

import java.io.*;

public class VisualType {

  static final int STATIC_GRAY_CLASS  = 0;
  static final int GRAY_SCALE_CLASS   = 1;
  static final int STATIC_COLOR_CLASS = 2;
  static final int PSEUDO_CLASS       = 3;
  static final int TRUE_COLOR_CLASS   = 4;
  static final int DIRECT_COLOR_CLASS = 5;

  ZPixmapFormat zPixmapFormat;
  int depth;
  int visualId;
  int visualClass;
  int bitsPerRgbValue;
  int colormapEntriesNumber;
  int redMask;
  int greenMask;
  int blueMask;
  
  public final ZPixmapFormat zPixmapFormat() { return zPixmapFormat; }
  public final int depth() { return depth; }
  public final int visualId() { return visualId; }
  public final int visualClass() { return visualClass; }
  public final int bitsPerRgbValue() { return bitsPerRgbValue; }
  public final int colormapEntriesNumber() { return colormapEntriesNumber; }
  public final int redMask() { return redMask; }
  public final int greenMask() { return greenMask; }
  public final int blueMask() { return blueMask; }

  VisualType(ZPixmapFormat format, int depth, XInput input) {
    this.zPixmapFormat = format;
    this.depth = depth;
    
    visualId = input.readInt();

    visualClass = input.readUnsignedByte();

    bitsPerRgbValue = input.readUnsignedByte();

    colormapEntriesNumber = input.readUnsignedShort();

    redMask = input.readInt();
    greenMask = input.readInt();
    blueMask = input.readInt();
  
    input.skip(4);
    
  }
  
}

