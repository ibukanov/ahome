package lib.x;

import java.io.*;

public class Screen {
  
  int screenNumber;
  int rootWindow;
  int defaultColormapId;
  int whitePixel;
  int blackPixel;
  int currentInputMask;
  int pixelWidth;
  int pixelHeight;
  int mmWidth;
  int mmHeight;
  int minInstalledColormaps;
  int maxInstalledColormaps;
  int rootVisualId;
  int backingStoreType;
  boolean saveUnders;
  int rootDepth;
  VisualType visuals[];
  VisualType rootVisual;

  public final int screenNumber() { return screenNumber; }
  public final int rootWindow() { return rootWindow; }
  public final int defaultColormapId() { return defaultColormapId; }
  public final int whitePixel() { return whitePixel; }
  public final int blackPixel() { return blackPixel; }
  public final int currentInputMask() { return currentInputMask; }
  public final int pixelWidth() { return pixelWidth; }
  public final int pixelHeight() { return pixelHeight; }
  public final int mmWidth() { return mmWidth; }
  public final int mmHeight() { return mmHeight; }
  public final int minInstalledColormaps() { return minInstalledColormaps; }
  public final int maxInstalledColormaps() { return maxInstalledColormaps; }
  public final int rootVisualId() { return rootVisualId; }
  public final VisualType rootVisual() { return rootVisual; }
  public final int backingStoreType() { return backingStoreType; }
  public final boolean saveUnders() { return saveUnders; }
  public final int rootDepth() { return rootDepth; }
  public final VisualType visual(int i) { return visuals[i]; }
  public final int visualCount() { return visuals.length; }

  Screen(int screenNumber, XInput input, ZPixmapFormat[] zPixmapFormats) {

    this.screenNumber = screenNumber;
    rootWindow = input.readInt();
    defaultColormapId = input.readInt();

    whitePixel = input.readInt();
    blackPixel = input.readInt();

    currentInputMask = input.readInt();

    pixelWidth = input.readUnsignedShort();
    pixelHeight = input.readUnsignedShort();
    mmWidth = input.readUnsignedShort();
    mmHeight = input.readUnsignedShort();

    minInstalledColormaps = input.readUnsignedShort();
    maxInstalledColormaps = input.readUnsignedShort();

    rootVisualId = input.readInt();

    backingStoreType = input.readUnsignedByte();

    saveUnders = input.readBoolean();

    rootDepth = input.readUnsignedByte();

    int depthCount = input.readUnsignedByte();
    VisualType[][] visualsTmp = new VisualType[depthCount][];
    int visualCount = 0;
    VisualType[] current;

    for (int i = 0; i < depthCount; ++i) {
      int depth = input.readUnsignedByte(); 
      input.skip(1);
      int visualPerDepth = input.readUnsignedShort();
      input.skip(4);
      if (visualPerDepth > 0) {
        int j = zPixmapFormats.length;
        while (j > 0) {
          ZPixmapFormat format = zPixmapFormats[--j];
          if (depth == format.depth) {
            visualsTmp[i] = current = new VisualType[visualPerDepth];
            for (int k = 0; k < current.length; ++k) { 
              current[k] = new VisualType(format, depth, input);
              visualCount++;
            }
            break;  
          }
        }
      }  
    }
    
    visuals = new VisualType[visualCount]; 
    int i = 0, j = 0;
    for (int k = 0; k < visualCount; ++k) {
      if (visualsTmp[i] == null || j == visualsTmp[i].length) {
        while (visualsTmp[++i] == null) { }
        j = 0;  
      }
      visuals[k] = visualsTmp[i][j];
      ++j;
    }  
    
    for (i = 0; i < visualCount; ++i) {
      if ((rootVisual = visuals[i]).visualId == rootVisualId) {
        return;
      }
    }
    // Something bad happend...
    throw new XIOError("Can not find root visual for visual id 0x" 
                         + Integer.toHexString(rootVisualId));

  }

}

