package lib.x;

/** Class to translator rgb color to X server pixel */ 
public class ColorTranslator {
  
  ColorTranslator() { }
  
  int transparentMask;
 
  public final int transparentMask() { return transparentMask; }
 
  public final int bestDevicePixel(int red, int green, int blue) {
    return matchBest(Tk.boundToUnsignedByte(red),
                     Tk.boundToUnsignedByte(green), 
                     Tk.boundToUnsignedByte(blue));
  }                   
  
  /** rgbPixel = (red << 16) | (green << 8) | blue */
  public final int bestDevicePixel(int rgb) {
    return matchBest((rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, rgb & 0xFF);
  }
  
  /** rgbPixel = (red << 16) | (green << 8) | blue */
  public final int closeDevicePixel(int rgb) {
    return matchClose((rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, rgb & 0xFF);
  }
  
  public final int closeDevicePixel(int red, int green, int blue) {
    return matchClose(Tk.boundToUnsignedByte(red),
                      Tk.boundToUnsignedByte(green), 
                      Tk.boundToUnsignedByte(blue));
  }                   

// ??? Do not forget to copy....
  public void toCloseDevicePixels(
    int width, int height, 
    int[] rgbSource, int sourceOffset, int sourceScanSize,
    int[] pixelDest, int destOffset, int destScanSize) {
  
    int i, rgb, sourceDelta, destDelta;

    sourceDelta = sourceScanSize - width;
    destDelta = destScanSize - width;

    while (height > 0) {
      --height;
      i = width;
      while (i > 0) {
        --i;
        rgb = rgbSource[sourceOffset++];
        pixelDest[destOffset++] = matchClose(
          rgb & 0xFF, (rgb >> 8) & 0xFF, (rgb >> 16) & 0xFF);
      }
      sourceOffset += sourceDelta;
      sourceOffset += sourceDelta;
    } 
  }  

  public static boolean isColorOpaque(int rgb) {
    return (rgb & 0xFF000000) != 0;
  }

  public static boolean allColorsOpaque(
    int w, int h, int[] rgb, int offset, int scanSize) {
  
    int i, deltaOffset = scanSize - w;
    while (h > 0) {
      --h;
      i = w;
      while (i > 0) {
        --i;
        if (!isColorOpaque(rgb[offset++])) {
          return false;
        }
      }
      offset += deltaOffset;
    } 
    return true;
  }

  public final boolean isDevicePixelOpaque(int pixel) {
    return (pixel & transparentMask) == 0;
  }

  public final boolean allDevicePixelsOpaque(
    int w, int h, int[] pixels, int offset, int scanSize) {
  
    int i, deltaOffset = scanSize - w;
    while (h > 0) {
      --h;
      i = w;
      while (i > 0) {
        --i;
        if (!isDevicePixelOpaque(pixels[offset++])) {
          return false;
        }
      }
      offset += deltaOffset;
    } 
    return true;
  }

  int matchBest(int red, int green, int blue) { return 0; }
  int matchClose(int red, int green, int blue) { return 0; }
  
  void close() { }
  protected void finalize() { close(); }

  static ColorTranslator create(WindowSubsystem dpy) {
    VisualType v = dpy.frameWindowVisual;
    switch (v.visualClass) {
    case VisualType.STATIC_COLOR_CLASS:
    case VisualType.PSEUDO_CLASS:
      return new IndexColorTranslator(dpy, v, dpy.frameColorMapId());
    case VisualType.TRUE_COLOR_CLASS:
    case VisualType.DIRECT_COLOR_CLASS:
      return new TrueColorTranslator(v);
    case VisualType.STATIC_GRAY_CLASS:
    case VisualType.GRAY_SCALE_CLASS:
    default:
      return new ColorTranslator();
    }
  }                              
  
}


final class TrueColorTranslator extends ColorTranslator {

  int redMask;
  int greenMask;
  int blueMask;

  int redShift;
  int greenShift;
  int blueShift;
  
  int redRoundShift;
  int greenRoundShift;
  int blueRoundShift;
  
  TrueColorTranslator(VisualType v) {
    redMask   = v.redMask;
    greenMask = v.greenMask;
    blueMask  = v.blueMask;
    
    transparentMask = ~(redMask | greenMask | blueMask);

    redShift   = Tk.calculateShiftForMask(redMask);
    greenShift = Tk.calculateShiftForMask(greenMask);
    blueShift  = Tk.calculateShiftForMask(blueMask);

    int maskLength = Tk.maskLength(redMask);
    if (maskLength >= 8) {
      redRoundShift = 0;
      redShift += (maskLength - 8);      
    }
    else {
      redRoundShift = 8 - maskLength;
    }
    maskLength = Tk.maskLength(greenMask);
    if (maskLength >= 8) {
      greenRoundShift = 0;
      greenShift += (maskLength - 8);      
    }
    else {
      greenRoundShift = 8 - maskLength;
    }
    maskLength = Tk.maskLength(blueMask);
    if (maskLength >= 8) {
      blueRoundShift = 0;
      blueShift += (maskLength - 8);      
    }
    else {
      blueRoundShift = 8 - maskLength;
    }
  }
  
  final int matchBest(int red, int green, int blue) {
    return ((red >>> redRoundShift) << redShift)
           | ((green >>> greenRoundShift) << greenShift)
           | ((blue >>> blueRoundShift) << blueShift);
  }         

  final int matchClose(int red, int green, int blue) { 
    return matchBest(red, green, blue);
  }

}

