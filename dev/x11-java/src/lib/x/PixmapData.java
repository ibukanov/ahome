package lib.x;

public class PixmapData extends XImageData {

  public static XImageData create(WindowSubsystem dpy, int width, int height) {
    ZPixmapFormat format = dpy.frameWindowVisual().zPixmapFormat();
    if (format.depth != 1) {
      return new PixmapData(
        dpy.imageMSFByteOrder, dpy.colorTranslator(), format, width, height);
    }
    else {
      return new XBitmapData(dpy, width, height);
    }
  }

  int       mode;

  PixmapData(WindowSubsystem dpy, int width, int height) {
    this(dpy.imageMSFByteOrder, dpy.colorTranslator(), 
      dpy.frameWindowVisual().zPixmapFormat(), width, height);
  }
  
  PixmapData(
    boolean imageMSFByteOrder, ColorTranslator colorTranslator, 
    ZPixmapFormat format, int width, int height) {
    
    super(width, height, format.depth, colorTranslator, 
      format.scanlinePad, format.bitsPerPixel);

    if (format.bitsPerPixel == 8 || imageMSFByteOrder) {
      mode = format.bitsPerPixel;
    }
    else {
      mode = -format.bitsPerPixel;
    }
  }

  public void putDevicePixel(int x, int y, int pixel) {
    int shift;
    switch (mode) {
    case 32:
      shift = (x + y * unitsPerScan) * 4;
      data[shift] = (byte)(pixel >> 24);
      data[++shift] = (byte)(pixel >> 16);
      data[++shift] = (byte)(pixel >> 8);
      data[++shift] = (byte)(pixel);
      break;
    case -32:
      shift = (x + y * unitsPerScan) * 4;
      data[shift] = (byte)(pixel);
      data[++shift] = (byte)(pixel >> 8);
      data[++shift] = (byte)(pixel >> 16);
      data[++shift] = (byte)(pixel >> 24);
      break;
    case 16:  
      shift = (x + y * unitsPerScan) * 2;
      data[shift] = (byte)(pixel >> 8);
      data[++shift] = (byte)(pixel);
      break;
    case -16:  
      shift = (x + y * unitsPerScan) * 2;
      data[shift] = (byte)(pixel);
      data[++shift] = (byte)(pixel >> 8);
      break;
    case 8:  
      shift = (x + y * unitsPerScan);
      data[shift] = (byte)(pixel);
      break;
    case 4:  
      shift = (x + y * unitsPerScan) / 2;
      data[shift] = (0 == (x & 1)) 
                    ? (byte)(((pixel << 4) & 0xF0) | (data[shift] & ~0xF0))
                    : (byte)((data[shift] & ~0x0F) | (pixel & 0x0F));
      break;
    case -4:  
      shift = (x + y * unitsPerScan) / 2;
      data[shift] = (0 != (x & 1)) 
                    ? (byte)(((pixel << 4) & 0xF0) | (data[shift] & ~0xF0))
                    : (byte)((data[shift] & ~0x0F) | (pixel & 0x0F));
      break;
    }
  }
  
  public void putDevicePixels(
    int x, int y, int w, int h, int[] pixels, int offset, int scansize) {

    int i, shift, pixel, deltaOffset, deltaShift, oddTest;
    
    deltaOffset = scansize - w;

    switch (mode) {
    case 32:
      shift = 4 * (x + y * unitsPerScan);
      deltaShift = 4 * (unitsPerScan - w);
      while (h > 0) {
        --h;
        i = w;
        while (i > 0) {
          --i;
          pixel = pixels[offset++];
          data[shift++] = (byte)(pixel >> 24);
          data[shift++] = (byte)(pixel >> 16);
          data[shift++] = (byte)(pixel >> 8);
          data[shift++] = (byte)(pixel);
        }
        shift += deltaShift;
        offset += deltaOffset;
      } 
      break;
    case -32:
      shift = 4 * (x + y * unitsPerScan);
      deltaShift = 4 * (unitsPerScan - w);
      while (h > 0) {
        --h;
        i = w;
        while (i > 0) {
          --i;
          pixel = pixels[offset++];
          data[shift++] = (byte)(pixel);
          data[shift++] = (byte)(pixel >> 8);
          data[shift++] = (byte)(pixel >> 16);
          data[shift++] = (byte)(pixel >> 24);
        }
        shift += deltaShift;
        offset += deltaOffset;
      } 
      break;
    case 16:
      shift = 2 * (x + y * unitsPerScan);
      deltaShift = 2 * (unitsPerScan - w);
      while (h > 0) {
        --h;
        i = w;
        while (i > 0) {
          --i;
          pixel = pixels[offset++];
          data[shift++] = (byte)(pixel >> 8);
          data[shift++] = (byte)(pixel);
        }
        shift += deltaShift;
        offset += deltaOffset;
      } 
      break;
    case -16:
      shift = 2 * (x + y * unitsPerScan);
      deltaShift = 2 * (unitsPerScan - w);
      while (h > 0) {
        --h;
        i = w;
        while (i > 0) {
          --i;
          pixel = pixels[offset++];
          data[shift++] = (byte)(pixel);
          data[shift++] = (byte)(pixel >> 8);
        }
        shift += deltaShift;
        offset += deltaOffset;
      } 
      break;
    case 8:  
      shift = (x + y * unitsPerScan);
      deltaShift = (unitsPerScan - w);
      while (h > 0) {
        --h;
        i = w;
        while (i > 0) {
          --i;
          data[shift++] = (byte)(pixels[offset++]);
        }
        shift += deltaShift;
        offset += deltaOffset;
      } 
      break;
    case 4:  
      shift = (x + y * unitsPerScan) / 2;
      oddTest = x & 1;
      deltaShift = (unitsPerScan - (w + oddTest)) / 2;
      while (h > 0) {
        --h;
        if (oddTest != 0) {
          pixel = (data[shift] & ~0x0F) | (pixels[offset++] & 0x0F);
          data[shift++] = (byte)(pixel);
        }
        i = (w - oddTest) / 2;
        while (i > 0) {
          --i;
          pixel = ((pixels[offset++] << 4) & 0xF0) | (pixels[offset++] & 0x0F);
          data[shift++] = (byte)(pixel);
        }
        if (((x + w) & 1) != 0) {
          pixel = ((pixels[offset++] << 4) & 0xF0) | (data[shift] & ~0xF0);
          data[shift] = (byte)(pixel);
        }
        shift += deltaShift;
        offset += deltaOffset;
      } 
      break;
    case -4:  
      shift = (x + y * unitsPerScan) / 2;
      oddTest = x & 1;
      deltaShift = (unitsPerScan - (w + oddTest)) / 2;
      while (h > 0) {
        --h;
        if (oddTest != 0) {
          pixel = ((pixels[offset++] << 4) & 0xF0) | (data[shift] & ~0xF0);
          data[shift++] = (byte)(pixel);
        }
        i = (w - oddTest) / 2;
        while (i > 0) {
          --i;
          pixel = (pixels[offset++] & 0x0F) | ((pixels[offset++] << 4) & 0xF0);
          data[shift++] = (byte)(pixel);
        }
        if (((x + w) & 1) != 0) {
          pixel = (data[shift] & ~0x0F) | (pixels[offset++] & 0x0F);
          data[shift] = (byte)(pixel);
        }
        shift += deltaShift;
        offset += deltaOffset;
      } 
      break;
    }
  }                      

  public void testFill(ColorTranslator ct) {
    int p1 = ct.bestDevicePixel(27, 128, 200);
    int p2 = ct.bestDevicePixel(200, 100, 29);
    java.util.Random r = new java.util.Random();
    for (int i = Math.min(width(), height()) - 2; i >= 1; --i) {
      putDevicePixel(i, i, ct.closeDevicePixel(r.nextInt()));
      putDevicePixel(i + 1, i, ct.closeDevicePixel(r.nextInt()));
      putDevicePixel(i - 1, i, ct.closeDevicePixel(r.nextInt()));
      putDevicePixel(i, height - i, ct.closeDevicePixel(255, 0, 0));
      putDevicePixel(i + 1, height - i, ct.closeDevicePixel(255, 0, 0));
      putDevicePixel(i - 1, height - i, ct.closeDevicePixel(255, 0, 0));
    }
  }

  public void test(ColorTranslator ct) {
    java.util.Random r = new java.util.Random();
    for (int i = 0; i < width(); ++i) {
      for (int j = 0; j < height(); ++j) {
        putDevicePixel(i, j, ct.closeDevicePixel(r.nextInt()));
      }
    }
  }

}


