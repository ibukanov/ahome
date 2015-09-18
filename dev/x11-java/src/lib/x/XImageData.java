package lib.x;

public abstract class XImageData {

  int               width;
  int               height;
  int               depth;
  int               unitsPerScan;
  int               bytesPerScan;
  ColorTranslator   colorTranslator;
  int               transparentMask;

  byte[]            data;
  
  XImageData(
    int width, int height, int depth, ColorTranslator colorTranslator, 
    int unitPad, int unitSize) {
    
    this.width = width;
    this.height = height;
    this.depth = depth;
    int  bitsPerScan  = Tk.roundup(width * unitSize, unitPad);
    this.unitsPerScan = bitsPerScan / unitSize;
    this.bytesPerScan = bitsPerScan / 8;
    this.colorTranslator = colorTranslator;
    this.transparentMask = colorTranslator.transparentMask;
    this.data = new byte[bytesPerScan * height];
  }  
  
  public final int width() { return width; }
  public final int height() { return height; }
  public final int depth() { return depth; }
  public final int unitsPerScan() { return unitsPerScan; }
  public final ColorTranslator colorTranslator() { return colorTranslator; }
  public final int transparentMask() { return transparentMask; }
  
  public abstract void putDevicePixel(int x, int y, int pixel);
  public abstract void putDevicePixels(
    int x, int y, int w, int h, int[] pixels, int offset, int scansize);

  public void putRGB(int x, int y, int rgb) {
    putDevicePixel(x, y, colorTranslator.closeDevicePixel(rgb));
  }

  public void putRGB(
    int x, int y, int w, int h, int[] rgb, int offset, int scanSize) {
  
    int[] pixels = new int[w * h];

    colorTranslator.
      toCloseDevicePixels(w, h, rgb, offset, scanSize, pixels, 0, w);
    
    putDevicePixels(x, y, w, h, pixels, 0, w);
  }  

  public void draw(
    WindowSubsystem dpy, int drawable, int gc, int x, int y) {

    int format = depth == 1 ? X.IMAGE_FORMAT_BITMAP : X.IMAGE_FORMAT_Z_PIXMAP;
    dpy.putImageData(
      drawable, gc, format, depth, width, height, x, y, data, 0, data.length);
  }  

}
