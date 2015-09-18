package lib.x;

public class XImage {

  WindowSubsystem   dpy;
  int               depth;
  int               width;
  int               height;
  int               pixmapId;
  int               bitmapId;

  public final WindowSubsystem dpy() { return dpy; }
  public final int width() { return width; }
  public final int height() { return height; }
  public final int depth() { return depth; }
  public final int pixmapId() { 
    ensurePixmap();
    return pixmapId; 
  }
  
  public XImage(WindowSubsystem dpy, int width, int height) {
    this(dpy, width, height, false);
  }

  public XImage(
    WindowSubsystem dpy, int width, int height, boolean depthOne) {
    
    this.dpy   = dpy;
    this.depth = depthOne ? 1 : dpy.frameWindowVisual.depth;
    this.width  = Tk.bound(width, 1, dpy.frameWindowScreen.pixelWidth);
    this.height = Tk.bound(height, 1, dpy.frameWindowScreen.pixelHeight);

    this.pixmapId = 0;
    this.bitmapId = 0;
    
  }
  
  public void dispose() {
    if (pixmapId != 0) {
      dpy.freePixmap(pixmapId);
      pixmapId = 0;
    }
    if (bitmapId != 0) {
      dpy.freePixmap(bitmapId);
      bitmapId = 0;
    }
  }
  
  protected void finalize() {
    Tk.dbg("From XImage finalize");
    dispose();
  }

  public final boolean contains(int x, int y) {
    return x >= 0 && y >= 0 && x < width && y < height; 
  }

  public final boolean canDrawIn(int x, int y, int w, int h) {
    return x >= 0 && w > 0 && x + w <= width 
           && y >= 0 && h > 0 && y + h <= height; 
  }

  private void ensurePixmap() {
    if (pixmapId == 0) {
      pixmapId = dpy.createPixmap(dpy.frameRootWindowId, depth, width, height);
    }
  }   
      
  private void ensureBitmap() {
    if (bitmapId == 0) {
      bitmapId = dpy.createBitmap(dpy.frameRootWindowId, width, height);
    }    
  }   
      
  public void draw(int drawable, int gc, int x, int y) {

    if (width <= 0 || height <= 0) { return; }

    ensurePixmap();
    if (bitmapId != 0) {
      dpy.setGCClipBitmap(gc, bitmapId, x, y);
    }
    dpy.copyArea(pixmapId, drawable, gc, 0, 0, width, height, x, y);
    if (bitmapId != 0) {
      dpy.clearGCClipping(gc);
    }
  }
  
  public void draw(int drawable, int gc, int x, int y, 
                   int clipX, int clipY, int clipWidth, int clipHeight) {

    if (clipWidth <= 0 || clipHeight <= 0) { return; }

    int srcX = 0, srcY = 0, w = width, h = height;
    if (x < clipX) {
      srcX = clipX - x;
      x = clipX;
      w -= srcX;
    }
    if (x + w > clipX + clipWidth) {
      w = clipX + clipWidth - x;
    } 
    if (y < clipY) {
      srcY = clipY - y;
      y = clipY;
      h -= srcY;
    }
    if (y + h > clipY + clipHeight) {
      h = clipY + clipHeight - y;
    } 
    if (w <= 0 || h <= 0) { return; }

    ensurePixmap();
    if (bitmapId != 0) {
      dpy.setGCClipBitmap(gc, bitmapId, x - srcX, y - srcY);
      dpy.copyArea(pixmapId, drawable, gc, srcX, srcY, w, h, x, y);
      dpy.setGCClipRect(gc, clipX, clipY, clipWidth, clipHeight);
    }
    else {
      dpy.copyArea(pixmapId, drawable, gc, srcX, srcY, w, h, x, y);
    }
  }
  
  public int getGC() {
    ensurePixmap();
    return dpy.createGC(pixmapId);
  }    
    
  public void putData(
    int x, int y, XImageData imageData, XBitmapData transparentMask) {
    
    int gc;
    if (transparentMask != null 
        && canDrawIn(x, y, transparentMask.width, transparentMask.height)) {
      ensureBitmap();
      transparentMask.draw(dpy, bitmapId, dpy.bitmapImageGC(), x, y);
    }
    if (imageData != null 
        && canDrawIn(x, y, imageData.width, imageData.height)) {

      if (imageData.depth != depth) {
        throw new XProgErr("XImage can handle pixels only of depth " + depth);
      }

      gc = getGC();
      imageData.draw(dpy, pixmapId, gc, x, y);
      dpy.freeGC(gc);
    }
  }  
    
  public void putRGB(
    int x, int y, int w, int h, 
    int[] rgbSource, int offset, int scanSize) {
    
    if (!canDrawIn(x, y, w, h)) { return; }

    XImageData imageData = PixmapData.create(dpy, w, h);
    imageData.putRGB(0, 0, width, height, rgbSource, offset, scanSize);

    int gc = getGC();
    imageData.draw(dpy, pixmapId, gc, x, y);
    dpy.freeGC(gc);

    imageData = null;

    if (!ColorTranslator.allColorsOpaque(
           width, height, rgbSource, offset, scanSize)) {

      XBitmapData transparentMask = new XBitmapData(dpy, w, h);
      transparentMask.putRGB(0, 0, w, h, rgbSource, offset, scanSize);
      
      ensureBitmap();
      transparentMask.draw(dpy, bitmapId, dpy.bitmapImageGC(), x, y);
    }
  }
  
  public void putImage(
    int x, int y, int h, int w, XImage src, int srcX, int srcY) {
    
    if (!canDrawIn(x, y, w, h) || !src.canDrawIn(srcX, srcY, w, h)) { return; }

    src.ensurePixmap();
    int gc = getGC();
    dpy.copyArea(src.pixmapId, pixmapId, gc, srcX, srcY, w, h, x, y);
    dpy.freeGC(gc);
    if (src.bitmapId != 0) {
      ensureBitmap();
      dpy.copyArea(src.bitmapId, bitmapId, dpy.bitmapImageGC(), 
                   srcX, srcY, w, h, x, y);
    }
  }  
    
}
