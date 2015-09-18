package lib.x;

public class XBitmapData extends XImageData {

  private static final int BITMAP_DEPTH = 1;

  int testMask;
  int bytesSwap;
  int bitsSwap;
 
  XBitmapData(WindowSubsystem dpy, int width, int height) {
    this(dpy, width, height, 1);
  }

  XBitmapData(WindowSubsystem dpy, int width, int height, int testMask) {

    super(width, height, BITMAP_DEPTH, dpy.colorTranslator(),
      dpy.bitmapFormatScanlinePad, 1);

    this.testMask = testMask;

    if (dpy.bitmapFormatMSFBitOrder) {
      bitsSwap = dpy.bitmapFormatScanlineUnit - 1;
    }
    else {
      bitsSwap = 0;
    }
    
    if (dpy.imageMSFByteOrder) {
      bytesSwap = (dpy.bitmapFormatScanlineUnit >> 3) - 1;
    }
    else {
      bytesSwap = 0;
    }

  }

  private void put(int x, int y, boolean set) {

    x ^= bitsSwap;
    int shift = ((x >> 3) ^ bytesSwap) + y * bytesPerScan;
    int bit = (1 << (x & 0x7));

    if (set) {
      data[shift] |= bit;
    }
    else {
      data[shift] &= ~bit;
    }

  }

  public void putDevicePixel(int x, int y, int pixel) {
    put(x, y, 0 != (pixel & testMask));
  }
  
  public void putDevicePixels(
    int x, int y, int w, int h, int[] pixels, int offset, int scansize) {

    int i, x1, deltaOffset;
    
    deltaOffset = scansize - w;

    while (h > 0) {
      --h;
      i = w;
      x1 = x;
      while (i > 0) {
        --i;
        put(x1++, y, 0 != (testMask & pixels[offset++]));
      }
      ++y;
      offset += deltaOffset;
    } 

  }

  public void putPixels(
    int x, int y, int w, int h, int[] pixels, int offset, int scanSize) {

    int i, x1, deltaOffset = scanSize - w;
    while (h > 0) {
      --h;
      i = w;
      x1 = x;
      while (i > 0) {
        --i;
        put(x1++, y, 0 != (testMask & pixels[offset++]));
      }
      ++y;
      offset += deltaOffset;
    } 

  }

  public void putRGB(
    int x, int y, int w, int h, int[] rgb, int offset, int scanSize) {
  
    int i, x1, deltaOffset = scanSize - w;
    while (h > 0) {
      --h;
      i = w;
      x1 = x;
      while (i > 0) {
        --i;
        put(x1++, y, 0 == (0xFF000000 & rgb[offset++]));
      }
      ++y;
      offset += deltaOffset;
    } 

  }  

  public void invertAll() {
    int i = data.length;
    while (i > 0) {
      --i;
      data[i] = (byte)~data[i];
    }
  }                      
  
  public boolean allBitsAreZero() {
    int i = data.length;
    while (i > 0) {
      --i;
      if (0 != data[i]) {
        return false;
      }
    }
    return true;
  }

  
  public void testFill() {
    int h = height();
    int w = width();
    for (int j = 0; j < h; ++j) {
      for (int i = 0; i < j; ++i) {
        putDevicePixel(i, j, testMask);
      }  
    }
  }
  
  public void test() {
    java.util.Random r = new java.util.Random();
    for (int i = 0; i < width(); ++i) {
      for (int j = 0; j < height(); ++j) {
        putDevicePixel(i, j, r.nextInt());
      }
    }
  }

}
