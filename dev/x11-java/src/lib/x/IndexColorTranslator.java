package lib.x;

final class IndexColorTranslator extends ColorTranslator {

// Copied here from Tk.java to permit inline optimization
  private static int mapIntensity(int i, int maxSource, int maxDestination) {
    return (2 * i * maxDestination + maxSource) / (2 * maxSource);
  }

  WindowSubsystem dpy;
  VisualType v; 
  int colormap;
  
  int maxColorCount;
  int colorCount;
  int[] rgbData;
  int[] pixelData;


  static final int RED_MAX   = 5;
  static final int GREEN_MAX = 5;
  static final int BLUE_MAX  = 5;
  static final int RGB_BOX_VOLUME = 
    (BLUE_MAX + 1) * (GREEN_MAX + 1) * (RED_MAX + 1);
  
  final int[] rgbBox = new int[RGB_BOX_VOLUME]; 
  
  private static int mapColorIntensityQuickly(int i, int maxDestination) {
    return (i * maxDestination + 128) >> 8;
  }

  static int boxIndex(int r, int g, int b) {
    return mapColorIntensityQuickly(r, RED_MAX) 
           + (RED_MAX + 1) * (mapColorIntensityQuickly(g, GREEN_MAX)
                               + (GREEN_MAX + 1) 
                                 * mapColorIntensityQuickly(b, BLUE_MAX));
  }

  int state;
  static final int ALLOCATE_COLOR_STATE = 0;
  static final int QUERY_COLORMAP_STATE = 1;
  static final int MATCH_COLOR_STATE    = 2;
  
  IndexColorTranslator(WindowSubsystem dpy, VisualType v, int colormap) {
    this.dpy = dpy;
    this.v   = v;
    this.colormap = colormap;
    
    maxColorCount = v.colormapEntriesNumber;
    
    colorCount = 0;
    rgbData = new int[maxColorCount];
    pixelData = new int[maxColorCount];
    state = ALLOCATE_COLOR_STATE;
    
    transparentMask = (1 << 31);

    FromSun.preallocateSomeColors(this);

    int boxIndex = 0;
    for (int b = 0; b <= BLUE_MAX; ++b) {
      for (int g = 0; g <= GREEN_MAX; ++g) {
        for (int r = 0; r <= RED_MAX; ++r) {
          rgbBox[boxIndex++] = allocColor(mapIntensity(r, RED_MAX, 255), 
                                          mapIntensity(g, GREEN_MAX, 255), 
                                          mapIntensity(b, BLUE_MAX, 255));
        }
      }
    }
    Tk.dbg("IndexColorTranslator constructed");

  }
  
  final int matchBest(int red, int green, int blue) {
    int i = colorCount - 1;
    if (i >= 0) {
      int t, d;
      int c = rgbData[i];
      d = (t = (c >> 16) - red) * t 
          + (t = ((c >> 8) & 0xFF) - green) * t 
          + (t = (c & 0xFF) - blue) * t;
      if (d == 0) { return pixelData[i]; }
      int best = i;
      while (i > 0) {
        c = rgbData[--i];
        t = (t = (c >> 16) - red) * t 
            + (t = ((c >> 8) & 0xFF) - green) * t 
            + (t = (c & 0xFF) - blue) * t;
        if (t == 0) { return pixelData[i]; }
        if (t < d) { 
          d = t;
          best = i;
        }
      }
      return pixelData[best];
    }
    return pixelData[0];
  }

  final int matchClose(int red, int green, int blue) { 
    return rgbBox[boxIndex(red, green, blue)];
  }

  final int allocColor(int r, int g, int b) {
    switch (state) {
    case ALLOCATE_COLOR_STATE: 
      try { 
        int pixel = dpy.allocColor(colormap, 
                                   (r << 8) | r, (g << 8) | g, (b << 8) | b);
        rgbData[colorCount] = (r << 16) | (g << 8) | b;
        pixelData[colorCount] = pixel;
        if (++colorCount == maxColorCount) {
          state = QUERY_COLORMAP_STATE;
        }
        return pixel;
      } 
      catch (XError e) { }  
      state = QUERY_COLORMAP_STATE;
      // No break here !
    case QUERY_COLORMAP_STATE:           
      Tk.dbg("QUERY_COLORMAP_STATE");
      int[] xPixels = new int[maxColorCount];
      int i = maxColorCount;
      while (i > 0) {
        --i;
        xPixels[i] = i;
      } 
      byte[] rgbXList = new byte[maxColorCount * X.RGB_SIZE];
      try {
        dpy.queryColormap(colormap, xPixels, maxColorCount, rgbXList);
      }
      catch (XError e) {
        // Now I have race conditions, i.e. somebody else just free a color.
        // Not to complicate the code I just return something like black
        return 0;
      }
      // Allocate all available colors to prevent them from releasing. 
      i = maxColorCount;
      int j = i * X.RGB_SIZE;
      while (i > 0) {
        --i;
        j -= X.RGB_SIZE;
        try { 
          int xr = Tk.getUnsignedShort(rgbXList, j);
          int xg = Tk.getUnsignedShort(rgbXList, j + 2);
          int xb = Tk.getUnsignedShort(rgbXList, j + 4);
          dpy.allocColor(colormap, xr, xg, xb);
          rgbData[i] = ((xr & 0xFF00) << 8) | (xg & 0xFF00) | (xb >> 8);
        } 
        catch (XError e) {
          rgbData[i] = 0;
        }
      }
      // Free previous colors
      dpy.freeColors(colormap, pixelData, 0, colorCount);  
      colorCount = maxColorCount;
      pixelData = xPixels;
      state = MATCH_COLOR_STATE;
      // No break here !
    case MATCH_COLOR_STATE:
      return matchBest(r, g, b);
    }
    return 0; 
  }
  
  void close() { 
    if (pixelData != null) {
      dpy.freeColors(colormap, pixelData, 0, colorCount);  
      pixelData = null;
    }  
    rgbData = null;
    dpy = null;
  }

}
