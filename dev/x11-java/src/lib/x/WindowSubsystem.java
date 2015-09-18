package lib.x;

public class WindowSubsystem extends XConnection implements Command {

  XIdsStore windowIdsStore;
  XIdsStore pixmapIdsStore;
  XIdsStore colormapIdsStore;
  XIdsStore fontIdsStore;
  XIdsStore cursorIdsStore;
  XIdsStore gcIdsStore;

  Screen  frameWindowScreen;
  int  frameRootWindowId;
  VisualType frameWindowVisual;
  ColorTranslator colorTranslator = null;

  private int frameColorMapId;
  private int cursorFontId = 0;
  private int bitmapImageGC = 0;

  public final Screen frameWindowScreen() { return frameWindowScreen; }
  public final int frameRootWindowId() { return frameRootWindowId; }
  public final VisualType frameWindowVisual() { return frameWindowVisual; }

  public final int frameColorMapId() { 
    if (frameColorMapId == 0) {
      frameColorMapId = createColormap(frameRootWindowId, frameWindowVisual, 
                                       ALLOC_NONE_COLORS);
    }
    return frameColorMapId;
  }

  public final ColorTranslator colorTranslator() {
    if (colorTranslator == null) {
      colorTranslator = ColorTranslator.create(this);
    }
    return colorTranslator;
  }

  public WindowSubsystem(String displayName) {
    super(displayName);
    windowIdsStore = new XIdsStore(this);
    pixmapIdsStore = new XIdsStore(this);
    colormapIdsStore = new XIdsStore(this);
    fontIdsStore = new XIdsStore(this);
    cursorIdsStore = new XIdsStore(this);
    gcIdsStore = new XIdsStore(this);
    
    frameWindowScreen = defaultScreen;
    frameRootWindowId = frameWindowScreen.rootWindow;
    frameWindowVisual = frameWindowScreen.rootVisual;
    frameColorMapId   = frameWindowScreen.defaultColormapId;

    int i = frameWindowScreen.visuals.length;
    while (i > 0) {
      --i;
      VisualType cur = frameWindowScreen.visuals[i];
      if (cur != frameWindowVisual && cur.depth == 24
          && (cur.visualClass == VisualType.TRUE_COLOR_CLASS
              || cur.visualClass == VisualType.DIRECT_COLOR_CLASS)) {
        frameWindowVisual = cur;
        frameColorMapId = 0;
        break;
      }
    }
  }                  
   
  public void close() {
    System.gc();
    try {
      freeAllQueuedIds();
      if (bitmapImageGC != 0) {
        freeGC(bitmapImageGC);
        bitmapImageGC = 0;
      }  
      if (cursorFontId != 0) {
        freeFont(cursorFontId);
        cursorFontId = 0;
      }
      if (colorTranslator != null) {
        colorTranslator.close();
        colorTranslator = null;
      }
      if (frameWindowVisual != frameWindowScreen.rootVisual 
          && frameColorMapId != 0) {
        freeColormap(frameColorMapId); 
      }  
      if (!gcIdsStore.empty()) {
        System.err.println("Some gc ids were not destroyed");
      }
      if (!cursorIdsStore.empty()) {
        System.err.println("Some cursor ids were not destroyed");
      }
      if (!fontIdsStore.empty()) {
        System.err.println("Some font ids were not destroyed");
      }
      if (!colormapIdsStore.empty()) {
        System.err.println("Some colormap ids were not destroyed");
      }
      if (!pixmapIdsStore.empty()) {
        System.err.println("Some pixmap ids were not destroyed");
      }
      if (!windowIdsStore.empty()) {
        System.err.println("Some window ids were not destroyed");
      }
    }  
    finally {
      super.close();
    }  
  }

// Code to allocate and free server ids.
  final boolean isAllocatedId(int id) {
    id &= ~resourceIdBase;
    return resourceIdIncrement <= id && id <= resourceIdMask;
  }
  
  final int getNextResourceId() {
    resourceIdNext += resourceIdIncrement;
    if (resourceIdNext > resourceIdMask) {
      resourceIdNext -= resourceIdIncrement;
      throw new XIOError("can not allocate new resource ID");
    }
    return resourceIdNext | resourceIdBase;
  }

  private final Object freeQueueLock = new Object();
  FreeRequestQueueNode firstFreeRequest = null;
  FreeRequestQueueNode lastFreeRequest = null;
  
  
  private final void freeSingleServerId(int id, int request, XIdsStore store) {
    if (X.DEBUG) Tk.assert(store.present(id));
    startRequest(request, X.UNUSED_BYTE, 0);
    output.writeInt(id);
    endRequest();
    store.freeId(id);
  }
  
  final void freeAllQueuedIds() {
    FreeRequestQueueNode node;
    synchronized(freeQueueLock) {
      node = firstFreeRequest;
      lastFreeRequest = firstFreeRequest = null;
    }
    for (; node != null; node = node.next) { 
      freeSingleServerId(node.id, node.request, node.store);
    }
    Tk.dbg("freeAllQueuedIds call");
  }
  
  final void freeServerId(int id, int request, XIdsStore store) {
    if (X.DEBUG) Tk.assert(store.present(id));
    if (dpyThread != Thread.currentThread()) {
      Tk.dbg("Asynchronous free id call");
      // Currently this is a dirty hook to support calling free id functions
      // from finalize methods that can be called by system from any thread. 
      FreeRequestQueueNode tmp = new FreeRequestQueueNode(id, request, store);
      synchronized(freeQueueLock) {
        if (firstFreeRequest == null) {
          firstFreeRequest = tmp;
        }
        else {
          lastFreeRequest.next = tmp;
        }
        lastFreeRequest = tmp;
      }   
      eventConsumer().performLater(this);
    } 
    else {
      freeSingleServerId(id, request, store);
    }  
  }
  
  public void perform() {
    freeAllQueuedIds();
  }
  
// Window relatad functions  

  private final int[] intBuffer 
    = new int[Math.max(WM_HINTS_FIELDS_COUNT, WM_SIZE_FIELDS_COUNT)];

  private boolean addAtomsLoaded = false;
  private int wmProtocolsAtom = 0;
  private int wmDeleteWindowAtom = 0;
  public final int wmProtocolsAtom() { return wmProtocolsAtom; }
  public final int wmDeleteWindowAtom() { return wmDeleteWindowAtom; }
  
  private void loadAddAtoms() {
    if (!addAtomsLoaded) {
      try {
        wmProtocolsAtom = getAtomForName("WM_PROTOCOLS");
        wmDeleteWindowAtom = getAtomForName("WM_DELETE_WINDOW");
        addAtomsLoaded = true;
      }
      catch (XError e) {
        throw new 
          XIOError("Can not allocate X Atoms WM_PROTOCOLS and WM_DELETE_WINDOW");
      }
    }
  }
  
  private static final byte[] classBytes = 
    Tk.lowBytes("JavaClient\0javaClient\0");
  
  public final int createFrameWindow(int x, int y, int width, int height) {

    int window = createSimpleWindow(frameRootWindowId, frameWindowVisual,
                                    frameColorMapId(),
                                    x, y, width, height);
    loadAddAtoms();

    // AWT do not have XWindows Instance and class names 
    changeProperty(window, X.XA_WM_CLASS, X.XA_STRING, classBytes);    

    // I want to get delete window notification
    intBuffer[0] = wmDeleteWindowAtom;
    changeProperty(window, wmProtocolsAtom, X.XA_ATOM, intBuffer, 0, 1);

    setHints(window);
    
    return window;
  }
  
  static final int PARENT_WINDOW_DEPTH          = 0;
  static final int NULL_WINDOW_BORDER           = 0;
  static final int INPUT_OUTPUT_WINDOW_CLASS    = 1;

  private static final int SET_BACKGROUND_PIXEL_FLAG   = 0x00000002;
  private static final int SET_BORDER_PIXEL_FLAG       = 0x00000008;
  private static final int SET_EVENT_MASK_FLAG         = 0x00000800;
  private static final int SET_COLORMAP_FLAG           = 0x00002000;
  private static final int SET_CURSOR_FLAG             = 0x00004000;
  
                                     
  public final int createSimpleWindow(int parentId, 
                                      VisualType visual, int colormapId,
                                      int x, int y, int width, int height) {
    int windowId = windowIdsStore.newId();
    startRequest(X.CREATE_WINDOW_REQUEST, visual.depth, 4);
    output.writeInt(windowId);
    output.writeInt(parentId);
    output.writeShort(Tk.boundToShort(x));
    output.writeShort(Tk.boundToShort(y));
    output.writeShort(Tk.boundToPositiveShort(width));
    output.writeShort(Tk.boundToPositiveShort(height));
    output.writeShort(NULL_WINDOW_BORDER);
    output.writeShort(INPUT_OUTPUT_WINDOW_CLASS);
    output.writeInt(visual.visualId);
    output.writeInt(SET_COLORMAP_FLAG); 
    output.writeInt(colormapId);
    endRequest();

    return windowId;
  }

  public final void selectInput(int window, int mask) {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.CHANGE_WINDOW_ATTRIBUTES_REQUEST, X.UNUSED_BYTE, 4);
    output.writeInt(window);
    output.writeInt(SET_EVENT_MASK_FLAG);
    output.writeInt(mask);
    endRequest();
  }

  public final void setWindowCursor(int window, int cursorId) {
    if (X.DEBUG) {
      Tk.assert(windowIdsStore.present(window));
      Tk.assert(cursorId == X.ID_NONE || cursorIdsStore.present(cursorId));
    }  
    startRequest(X.CHANGE_WINDOW_ATTRIBUTES_REQUEST, X.UNUSED_BYTE, 4);
    output.writeInt(window);
    output.writeInt(SET_CURSOR_FLAG);
    output.writeInt(cursorId);
    endRequest();
  }
  
  public final void setCursorFromShape(int window, int shape) {
    if (shape < 0) {
      setWindowCursor(window, X.COPY_ID_FROM_PARENT);
    }
    else {
      int cursorId = createStandardCursor(shape);
      setWindowCursor(window, cursorId);
      freeCursor(cursorId);
    }  
  }
  
  public final void setCursorFromAWTShape(int window, int awtShape) {
    setCursorFromShape(window, FromSun.awtCursorShapeToX(awtShape));
  }
  

  public final void mapWindow(int window) {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.MAP_WINDOW_REQUEST, X.UNUSED_BYTE, 0);
    output.writeInt(window);
    endRequest();
  }
  
  public final void unmapWindow(int window) {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.UNMAP_WINDOW_REQUEST, X.UNUSED_BYTE, 0);
    output.writeInt(window);
    endRequest();
  }
  
  public final void destroyWindow(int window) {
    freeServerId(window, X.DESTROY_WINDOW_REQUEST, windowIdsStore);
  }
  
  private static final int SET_X_MASK        = 0x0001; 
  private static final int SET_Y_MASK        = 0x0002; 
  private static final int SET_POSITION_MASK = SET_X_MASK | SET_Y_MASK; 
  private static final int SET_WIDTH_MASK    = 0x0004; 
  private static final int SET_HEIGHT_MASK   = 0x0008; 
  private static final int SET_SIZE_MASK     = SET_WIDTH_MASK | SET_HEIGHT_MASK;
  private static final int SET_STACK_MASK    = 0x0040; 
    private static final int STACK_MODE_ABOVE = 0; 
    private static final int STACK_MODE_BELOW = 1; 

  private final void startConfigureRequest(int window, int flags, int size) {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.CONFIGURE_WINDOW_REQUEST, X.UNUSED_BYTE, size);
    output.writeInt(window);
    output.writeShort(flags);
    output.writeShort(X.UNUSED_SHORT);
  }

  public final void configureWindow(int window, 
                                    int x, int y, int width, int height) {
    startConfigureRequest(window, SET_POSITION_MASK | SET_SIZE_MASK, 
                          4 * X.INT_SIZE);
    output.writeInt(Tk.boundToShort(x));
    output.writeInt(Tk.boundToShort(y));
    output.writeInt(Tk.boundToPositiveShort(width));
    output.writeInt(Tk.boundToPositiveShort(height));
    endRequest();
  }
  
  public final void setWindowPos(int window, int x, int y) {
    startConfigureRequest(window, SET_POSITION_MASK, 2 * X.INT_SIZE);
    output.writeInt(Tk.boundToShort(x));
    output.writeInt(Tk.boundToShort(y));
    endRequest();
  }
  
  public final void setWindowSize(int window, int width, int height) {
    startConfigureRequest(window, SET_SIZE_MASK, 2 * X.INT_SIZE);
    output.writeInt(Tk.boundToPositiveShort(width));
    output.writeInt(Tk.boundToPositiveShort(height));
    endRequest();
  }
  
  public final void bringWindowToTop(int window) {
    startConfigureRequest(window, SET_STACK_MASK, X.INT_SIZE);
    output.writeInt(STACK_MODE_ABOVE);
    endRequest();
  }
  
  public final void bringWindowToBack(int window) {
    startConfigureRequest(window, SET_STACK_MASK, X.INT_SIZE);
    output.writeInt(STACK_MODE_BELOW);
    endRequest();
  }
  
  final int getAtomForName(String name) throws XError {
    int length = name.length();
    if (X.DEBUG) Tk.assert(length <= 0xFFFF && length >= 1);
    byte[] nameBytes = Tk.lowBytes(name);
    startRequest(X.INTERN_ATOM_REQUEST, X.UNUSED_BYTE, 
                 length + Tk.pad4(length), 
                 X.WAIT_SERVER_ANSWER);
    output.writeShort(length);
    output.pad4();
    output.write(nameBytes);
    output.pad4();
    endRequest();
    byte[] reply = getReply();
    int atom = Tk.getInt(reply, 8);
    finishReply();
    return atom;
  }
  
  final String getNameForAtom(int atom) throws XError {
    startRequest(X.GET_ATOM_NAME_REQUEST, X.UNUSED_BYTE, 0, 
                 X.WAIT_SERVER_ANSWER);
    output.writeInt(atom);
    endRequest();
    byte[] reply = getReply();
    int nameLength = Tk.getUnsignedShort(reply, 8);
    String name = new String(dataInput.readBytes(nameLength), 0);
    dataInput.pad4();
    finishReply();
    return name;
  }
  
  final void changeProperty(int window, int property, int type, byte[] data) {
    changeProperty(window, property, type, data, 0, data.length);
  }                                 

  private static final int ReplacePropertyMode = 0;

  final void changeProperty(int window, int property, int type,
                            byte[] data, int offset, int length) {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.CHANGE_PROPERTY_REQUEST, ReplacePropertyMode, 
                 Tk.roundupToDword(length));
    output.writeInt(window);
    output.writeInt(property);
    output.writeInt(type);
    output.writeByte(8);
    output.pad4();
    output.writeInt(length);
    output.write(data, offset, length);
    output.pad4();
    endRequest();
  }
  
  final void changeProperty(int window, int property, int type,
                            int[] data) {
    changeProperty(window, property, type, data, 0, data.length);
  }                                 

  final void changeProperty(int window, int property, int type,
                            int[] data, int offset, int length) {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.CHANGE_PROPERTY_REQUEST, ReplacePropertyMode, 
                 length * X.INT_SIZE);
    output.writeInt(window);
    output.writeInt(property);
    output.writeInt(type);
    output.writeByte(32);
    output.pad4();
    output.writeInt(length);
    output.writeInts(data, offset, length);
    endRequest();
  }
  
  public final void deleteProperty(int window, int property) {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.DELETE_PROPERTY_REQUEST, X.UNUSED_BYTE, 0);
    output.writeInt(window);
    output.writeInt(property);
    endRequest();
  }
  
  private static final int DO_NOT_DELETE_PROPERTY = 0;

  private byte[] getPropertyReply(int window, int property, int type,
                                  int offsetInProperty, int length) 
                                  throws XError {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.GET_PROPERTY_REQUEST, DO_NOT_DELETE_PROPERTY, 0, 
                 X.WAIT_SERVER_ANSWER);
    output.writeInt(window);
    output.writeInt(property);
    output.writeInt(type);
    output.writeInt(offsetInProperty);
    output.writeInt(length);
    endRequest();
    return getReply();
  }  
                                
  private int propertyValueLength(byte[] reply) {
    int format = Tk.getUnsignedByte(reply, 1) / 8;
    int typeAtom = Tk.getInt(reply, 8);
    int propertyLength;
    if (typeAtom == 0 && format == 0) {
      return PROPERTY_NOT_PRESENT;
    }
    else {
      int valueLength = Tk.getInt(reply, 16) * format;
      if (valueLength == 0) {
        return INVALID_PROPERTY_TYPE;
      }
      return valueLength;
    }  
  }

  public static final int PROPERTY_NOT_PRESENT   = -1;
  public static final int INVALID_PROPERTY_TYPE  = -2;

  public final int getProperty(int window, int property, int type, 
                               int offsetInProperty, int length, 
                               byte[] data, int offset) throws XError {
    int propertyLength;
    int realOffset = offsetInProperty / X.INT_SIZE;
    int realLength = 
      (offsetInProperty + length + X.INT_SIZE - 1) / X.INT_SIZE - realOffset;
    byte[] reply = 
      getPropertyReply(window, property, type, realOffset, realLength);
    int valueLength = propertyValueLength(reply);
    if (valueLength < 0) {
       propertyLength = valueLength;
    }
    else {
      int dataStartShift = offsetInProperty % X.INT_SIZE;
      if (X.DEBUG) Tk.assert(valueLength >= length + dataStartShift);
      dataInput.skip(dataStartShift);
      dataInput.read(data, offset, length);
      dataInput.skip(Tk.roundupToDword(valueLength) 
                     - (length + dataStartShift));
      propertyLength = 
        realOffset * 4 + valueLength + Tk.getInt(reply, 12);
    }    
    finishReply();
    return propertyLength;
  }
  
  final int getProperty(int window, int property, int type, 
                        int offsetInProperty, int length, 
                        int[] data, int offset) throws XError {
    int propertyLength;
    byte[] reply = 
      getPropertyReply(window, property, type, offsetInProperty, length);
    int valueLength = propertyValueLength(reply);
    if (valueLength < 0) {
       propertyLength = valueLength;
    }
    else {
      if (X.DEBUG) Tk.assert(valueLength == length * X.INT_SIZE);
      dataInput.readInts(data, offset, length);
      propertyLength = 
        offsetInProperty * 4 + valueLength + Tk.getInt(reply, 12);
    }    
    finishReply();
    return propertyLength;
  }
  
  final int[] listProperties(int window) throws XError {
    if (X.DEBUG) Tk.assert(windowIdsStore.present(window));
    startRequest(X.LIST_PROPERTIES_REQUEST, X.UNUSED_BYTE, 0, 
                 X.WAIT_SERVER_ANSWER);
    output.writeInt(window);
    endRequest();
    byte[] reply = getReply();
    int atomsCount = Tk.getUnsignedShort(reply, 8);
    int[] atoms = dataInput.readInts(atomsCount);
    finishReply();
    return atoms;
  }

  public final void setWindowTitle(int window, String title) {
    byte[] bytes = Tk.lowBytes(title);
    changeProperty(window, X.XA_WM_NAME, X.XA_STRING, bytes);    
    changeProperty(window, X.XA_WM_ICON_NAME, X.XA_STRING, bytes);  
  }

  private static final int WM_HINTS_FIELDS_COUNT = 9;
    private static final int HINT_FLAGS_OFFSET = 0;
      private static final int INPUT_HINT_FLAG = 0x1;
      private static final int STATE_HINT_FLAG = 0x2;
    
    private static final int INPUT_OFFSET = 1;

    private static final int STATE_OFFSET = 2;
      private static final int STATE_NORMAL = 1;
  
  private final void setHints(int window) {
    // It seems that among all WM_HINTS only input and initial state should
    // be set for jave client...
    intBuffer[HINT_FLAGS_OFFSET] = INPUT_HINT_FLAG | STATE_HINT_FLAG;
    intBuffer[INPUT_OFFSET] = 1; // true
    intBuffer[STATE_OFFSET] = STATE_NORMAL; 
    changeProperty(window, X.XA_WM_HINTS, X.XA_WM_HINTS, 
                   intBuffer, 0, WM_HINTS_FIELDS_COUNT);  
  }

  private static final int WM_SIZE_FIELDS_COUNT = 18;
    private static final int SIZE_HINT_FLAGS_OFFSET = 0;
      private static final int POS_BY_PROGRAM_FLAG  = 0x4;
      private static final int SIZE_BY_PROGRAM_FLAG = 0x8;
      private static final int MIN_SIZE_FLAG        = 0x10;
      private static final int MAX_SIZE_FLAG        = 0x20;

    private static final int MIN_WIDTH_OFFSET  = 5;
    private static final int MIN_HEIGHT_OFFSET = 6;
    private static final int MAX_WIDTH_OFFSET  = 7;
    private static final int MAX_HEIGHT_OFFSET = 8;

  private void setSizeProperty(int window, int[] buffer) {
    changeProperty(window, X.XA_WM_NORMAL_HINTS, X.XA_WM_SIZE_HINTS, 
                   buffer, 0, WM_SIZE_FIELDS_COUNT);  
  }

  public final void setMinMaxSize(int window,
                                  int minWidth, int minHeight, 
                                  int maxWidth, int maxHeight) {
    intBuffer[SIZE_HINT_FLAGS_OFFSET] = POS_BY_PROGRAM_FLAG 
                                        | SIZE_BY_PROGRAM_FLAG 
                                        | MIN_SIZE_FLAG 
                                        | MAX_SIZE_FLAG;
    minWidth = Tk.boundToPositiveShort(minWidth); 
    minHeight = Tk.boundToPositiveShort(minHeight); 
    intBuffer[MIN_WIDTH_OFFSET]  = minWidth; 
    intBuffer[MIN_HEIGHT_OFFSET] = minHeight; 
    intBuffer[MAX_WIDTH_OFFSET]  = Tk.bound(maxWidth, minWidth, X.USHORT_MAX); 
    intBuffer[MAX_HEIGHT_OFFSET] = Tk.bound(maxHeight, minHeight, X.USHORT_MAX); 
    setSizeProperty(window, intBuffer);
  }

  public final void setMaxSize(int window, int maxWidth, int maxHeight) {
    intBuffer[SIZE_HINT_FLAGS_OFFSET] = POS_BY_PROGRAM_FLAG 
                                        | SIZE_BY_PROGRAM_FLAG 
                                        | MAX_SIZE_FLAG;
    intBuffer[MAX_WIDTH_OFFSET]  = Tk.boundToPositiveShort(maxWidth); 
    intBuffer[MAX_HEIGHT_OFFSET] = Tk.boundToPositiveShort(maxHeight); 
    setSizeProperty(window, intBuffer);
  }

  public final void setMinSize(int window, int minWidth, int minHeight) {
    intBuffer[SIZE_HINT_FLAGS_OFFSET] = POS_BY_PROGRAM_FLAG 
                                        | SIZE_BY_PROGRAM_FLAG 
                                        | MIN_SIZE_FLAG; 
    intBuffer[MIN_WIDTH_OFFSET]  = Tk.boundToPositiveShort(minWidth); 
    intBuffer[MIN_HEIGHT_OFFSET] = Tk.boundToPositiveShort(minHeight); 
    setSizeProperty(window, intBuffer);
  }

// Pixmap relatad functions  
  public final int createPixmap(int drawable, int depth,
                                int width, int height)  {
    int pixmapId = pixmapIdsStore.newId();
    startRequest(X.CREATE_PIXMAP_REQUEST, Tk.boundToPositiveByte(depth), 0);
    output.writeInt(pixmapId);
    output.writeInt(drawable);
    output.writeShort(Tk.boundToPositiveShort(width));
    output.writeShort(Tk.boundToPositiveShort(height));
    endRequest();
    return pixmapId;
  }

  public final int createBitmap(int drawable, int width, int height)  {
    return createPixmap(drawable, X.BITMAP_DEPTH, width, height);
  }

  public final void freePixmap(int pixmap)  {
    freeServerId(pixmap, X.FREE_PIXMAP_REQUEST, pixmapIdsStore);
  }

// Colormaps and colors
  static final boolean ALLOC_NONE_COLORS = false;
  static final boolean ALLOC_ALL_COLORS  = true;
  static final int EMPTY_PLANE_MASK = 0;

  public final int createColormap(int windowId, 
                                  VisualType visual, boolean alloc)  {
    int colormapId = colormapIdsStore.newId();
    startRequest(X.CREATE_PIXMAP_REQUEST, alloc ? 1 : 0, 0);
    output.writeInt(colormapId);
    output.writeInt(windowId);
    output.writeShort(visual.visualId);
    endRequest();
    return colormapId;
  }                              

  public final void freeColormap(int colormapId)  {
    freeServerId(colormapId, X.FREE_COLORMAP_REQUEST, colormapIdsStore);
  }

  final void queryColormap(int colormapId, int[] pixels, int count, 
                           byte[] rgbXList) throws XError {
    if (X.DEBUG) Tk.assert(colormapId == frameWindowScreen.defaultColormapId 
                           || colormapIdsStore.present(colormapId));
    int length = count * X.PIXEL_SIZE;
    startRequest(X.QUERY_COLORS_REQUEST, X.UNUSED_BYTE, length, 
                 X.WAIT_SERVER_ANSWER);
    output.writeInt(colormapId);
    output.writeInts(pixels, 0, count);
    endRequest();
    byte[] reply = getReply();
    dataInput.read(rgbXList, 0, count * X.RGB_SIZE);
    finishReply();
  }
  
  final int allocColor(int colormapId, int red, int green, int blue) 
                              throws XError {
    if (X.DEBUG) Tk.assert(colormapId == frameWindowScreen.defaultColormapId 
                           || colormapIdsStore.present(colormapId));
    startRequest(X.ALLOC_COLOR_REQUEST, 0, 0, X.WAIT_SERVER_ANSWER);
    output.writeInt(colormapId);
    output.writeShort(red);
    output.writeShort(green);
    output.writeShort(blue);
    output.skip(2);
    endRequest();
    byte[] reply = getReply();
    red   = Tk.getUnsignedShort(reply, 8);
    green = Tk.getUnsignedShort(reply, 10);
    blue  = Tk.getUnsignedShort(reply, 12);
    int pixel = Tk.getInt(reply, 16);
    finishReply();
    return pixel;
  }

  final void freeColor(int colormapId, int pixel) {
    intBuffer[0] = pixel;
    freeColors(colormapId, intBuffer, 0, 1);
  } 

  final void freeColors(int colormapId, 
                        int[] pixels, int offset, int length) {
    if (X.DEBUG) Tk.assert(colormapId == frameWindowScreen.defaultColormapId 
                           || colormapIdsStore.present(colormapId));
    if (length > pixels.length) {
      throw new ArrayIndexOutOfBoundsException(length + " > " + pixels.length);
    }
    startRequest(X.FREE_COLORS_REQUEST, X.UNUSED_BYTE, length * X.INT_SIZE);
    output.writeInt(colormapId);
    output.writeInt(EMPTY_PLANE_MASK);
    output.writeInts(pixels, offset, length);
    endRequest();
  } 
  
  final int newFontId() {
    return fontIdsStore.newId();
  }

// Fonts support
  final void openFont(int fid, byte[] name) {
    if (X.DEBUG) Tk.assert(fontIdsStore.present(fid));
    int dataLength = name.length;
    int pad = Tk.pad4(dataLength);
    startRequest(X.OPEN_FONT_REQUEST, X.UNUSED_BYTE, dataLength + pad);
    output.writeInt(fid);
    output.writeShort(dataLength);
    output.writeShort(X.UNUSED_SHORT);
    output.write(name);
    output.skip(pad);
    endRequest();
  }

  final void openFont(int fid, byte[] foundry, byte[] faceName, byte[] style,
                      byte[] heightBuf, int heightBufLen, byte[] encoding) {
    if (X.DEBUG) Tk.assert(fontIdsStore.present(fid));
    Tk.dbg(new String(foundry, 0) + new String(faceName, 0)
            + new String(style, 0) + new String(heightBuf, 0, 0, heightBufLen)
            + new String(encoding, 0));

    int dataLength = foundry.length + faceName.length + style.length
                     + heightBufLen + encoding.length;
    int pad = Tk.pad4(dataLength);
    startRequest(X.OPEN_FONT_REQUEST, X.UNUSED_BYTE, dataLength + pad);
    output.writeInt(fid);
    output.writeShort(dataLength);
    output.writeShort(X.UNUSED_SHORT);
    output.write(foundry);
    output.write(faceName);
    output.write(style);
    output.write(heightBuf, 0, heightBufLen);
    output.write(encoding);
    output.skip(pad);
    endRequest();
  }

  public final void freeFont(int fid) {
    freeServerId(fid, X.CLOSE_FONT_REQUEST, fontIdsStore);
  }

  public final void freeFont(XFontInfo fontInfo) {
    freeFont(fontInfo.fid);
  }

  final XFontInfo queryFont(int fid) throws XError {
    if (X.DEBUG) Tk.assert(fontIdsStore.present(fid));
    startRequest(X.QUERY_FONT_REQUEST, X.UNUSED_BYTE, 0, X.WAIT_SERVER_ANSWER);
    output.writeInt(fid);
    endRequest();
    byte[] reply = getReply();
    XFontInfo info = new XFontInfo(fid, reply, dataInput);
    finishReply();
    return info;
  }

  public final XFontInfo loadFont(String family, int style, int height) {
    return FromSun.loadFont(this, family, style, height);
  }

// Cursors
  private void writeCursorColors(int rgbForeground, int rgbBackground) {
    int r, g, b;
    r = (rgbForeground >> 16) & 0xFF;
    g = (rgbForeground >> 8) & 0xFF;
    b = (rgbForeground >> 0) & 0xFF;
    output.writeShort((r << 8) | r);
    output.writeShort((g << 8) | g);
    output.writeShort((b << 8) | b);
    r = (rgbBackground >> 16) & 0xFF;
    g = (rgbBackground >> 8) & 0xFF;
    b = (rgbBackground >> 0) & 0xFF;
    output.writeShort((r << 8) | r);
    output.writeShort((g << 8) | g);
    output.writeShort((b << 8) | b);
  }

  public final int createBitmapCursor(int sourceBitmapId, int shapeBitmapId, 
                                      int rgbForeground, int rgbBackground,
                                      int x, int y) {
    if (X.DEBUG) {
      Tk.assert(pixmapIdsStore.present(sourceBitmapId));
      Tk.assert(shapeBitmapId == X.ID_NONE 
                || pixmapIdsStore.present(shapeBitmapId));
    }  
    int r, g, b;
    int cursorId = cursorIdsStore.newId();
    startRequest(X.CREATE_CURSOR_REQUEST, X.UNUSED_BYTE, 0);
    output.writeInt(cursorId);
    output.writeInt(sourceBitmapId);
    output.writeInt(shapeBitmapId);
    writeCursorColors(rgbForeground, rgbBackground);
    output.writeShort(x);
    output.writeShort(y);
    endRequest();
    return cursorId;
  }

  public final int createGlyphCursor(int sourceFontId, int shapeFontId,
                                     int sourceChar, int shapeChar,  
                                     int rgbForeground, int rgbBackground) {
    if (X.DEBUG) {
      Tk.assert(fontIdsStore.present(sourceFontId));
      Tk.assert(shapeFontId == X.ID_NONE 
                || fontIdsStore.present(shapeFontId));
    }  
    int r, g, b;
    int cursorId = cursorIdsStore.newId();
    startRequest(X.CREATE_GLYPH_CURSOR_REQUEST, X.UNUSED_BYTE, 0);
    output.writeInt(cursorId);
    output.writeInt(sourceFontId);
    output.writeInt(shapeFontId);
    output.writeShort(sourceChar);
    output.writeShort(shapeChar);
    writeCursorColors(rgbForeground, rgbBackground);
    endRequest();
    return cursorId;
  }

  private static final byte[] CURSOR_FONT_NAME = Tk.lowBytes("cursor");
  
  public final int createStandardCursor(int shape) {
    if (shape < 0 || shape > X.XC_NUM_GLYPHS - 2 || ((shape & 1) != 0)) {
      Tk.warning("Unknown cursor shape " + shape 
                 + " replaced by top left arrow");
      shape = X.XC_TOP_LEFT_ARROW;
    }
    if (cursorFontId == 0) {
      cursorFontId = newFontId();
      openFont(cursorFontId, CURSOR_FONT_NAME);
    }
    return createGlyphCursor(cursorFontId, cursorFontId, shape, shape + 1, 
                             X.RGB_BLACK, X.RGB_WHITE);
  }
  
  public final void freeCursor(int cursorId) {
    freeServerId(cursorId, X.FREE_CURSOR_REQUEST, cursorIdsStore);
  }
  
  public final void recolorCursor(int cursorId,
                                  int rgbForeground, int rgbBackground) {
    if (X.DEBUG) Tk.assert(cursorIdsStore.present(cursorId));
    int r, g, b;
    startRequest(X.RECOLOR_CURSOR_REQUEST, X.UNUSED_BYTE, 0);
    output.writeInt(cursorId);
    writeCursorColors(rgbForeground, rgbBackground);
    endRequest();
  }
  

// GC management
  final int bitmapImageGC() {
    if (bitmapImageGC == 0) {
      int bitmapId = createBitmap(frameRootWindowId, 1, 1);
      bitmapImageGC = createGC(bitmapId);
      freePixmap(bitmapId);
    }    
    return bitmapImageGC; 
  }
  
  private void startGCRequest(int gc, int request, int flags, 
                             int pixel, XFontInfo font, int function) {
    if (X.DEBUG) {
      Tk.assert(gcIdsStore.present(gc));
      Tk.assert(font == null || fontIdsStore.present(font.fid));
      Tk.assert(((flags & X.SET_GC_FUNCTION) == 0 && function == 0)
                || function == X.GC_FUNCTION_COPY
                || function == X.GC_FUNCTION_XOR);
    }            
    int length = Tk.maskLength(flags) * X.INT_SIZE;
    startRequest(request, 0, length);
    output.writeInt(gc);
  }  

  private void writeGCAttributes(int flags, 
                                 int pixel, XFontInfo font, int function) {
    output.writeInt(flags);
    if ((flags & X.SET_GC_FUNCTION) != 0) {
      output.writeInt(function);
    }
    if ((flags & X.SET_GC_FOREGROUND) != 0) {
      output.writeInt(pixel);
    }
    if ((flags & X.SET_GC_FONT) != 0) {
      output.writeInt(font == null ? 0 : font.fid);
    }
  }

  public final int createGC(int drawable) {
    return createGC(drawable, 0, 0, null, 0);
  }

  public final int createGC(int drawable, int flags,
                            int pixel, XFontInfo font, int function) {
    if (X.DEBUG) {
      Tk.assert(pixmapIdsStore.present(drawable)
                || windowIdsStore.present(drawable));
    }            
    int gc = gcIdsStore.newId();
    startGCRequest(gc, X.CREATE_GC_REQUEST, flags, pixel, font, function);
    output.writeInt(drawable);
    writeGCAttributes(flags, pixel, font, function);
    endRequest();
    return gc;
  }

  public final void freeGC(int gc) {
    freeServerId(gc, X.FREE_GC_REQUEST, gcIdsStore);
  }

  public final void changeGC(int gc, int flags,
                             int pixel, XFontInfo font, int function) {
    startGCRequest(gc, X.CHANGE_GC_REQUEST, flags, pixel, font, function);
    writeGCAttributes(flags, pixel, font, function);
    endRequest();
  }

  public final void setGCForeground(int gc, int pixel) {
    changeGC(gc, X.SET_GC_FOREGROUND, pixel, null, 0);
  }

  public final void setGCFont(int gc, XFontInfo font) {
    changeGC(gc, X.SET_GC_FONT, 0, font, 0);
  }

  public final void setGCClipBitmap(int gc, int bitmapId, int x, int y) {
    if (X.DEBUG) {
      Tk.assert(bitmapId == X.ID_NONE || pixmapIdsStore.present(bitmapId));
    }            
    startRequest(X.CHANGE_GC_REQUEST, 0, 3 * X.INT_SIZE);
    output.writeInt(gc);
    output.writeInt(X.SET_GC_CLIP_X_ORIGIN | X.SET_GC_CLIP_Y_ORIGIN 
                    | X.SET_GC_CLIP_BITMAP);
    output.writeInt(Tk.boundToShort(x));
    output.writeInt(Tk.boundToShort(y));
    output.writeInt(bitmapId);
    endRequest();
  }  

  private static final int UNSORETED_CLIP_RECTS = 0;
  private static final int ZERO_CLIP_X_ORIGIN   = 0;
  private static final int ZERO_CLIP_Y_ORIGIN   = 0;

  public final void setGCClipRect(int gc,
                                int clipX, int clipY,
                                int clipWidth, int clipHeight) {
    if (X.DEBUG) Tk.assert(gcIdsStore.present(gc));
    startRequest(X.SET_CLIP_RECTANGLES_REQUEST, UNSORETED_CLIP_RECTS, 8);
    output.writeInt(gc);
    output.writeShort(ZERO_CLIP_X_ORIGIN);
    output.writeShort(ZERO_CLIP_Y_ORIGIN);
    output.writeShort(clipX);
    output.writeShort(clipY);
    output.writeShort(clipWidth);
    output.writeShort(clipHeight);
    endRequest();
  }

  public final void clearGCClipping(int gc) {
    startRequest(X.CHANGE_GC_REQUEST, 0, X.INT_SIZE);
    output.writeInt(gc);
    output.writeInt(X.SET_GC_CLIP_BITMAP);
    output.writeInt(X.ID_NONE);
    endRequest();
  }

// Drawing output related functions
  public final void copyArea(int drawableSrc, int drawableDest, int gc,
                             int x, int y, int width, int height,
                             int destX, int destY) {
    if (X.DEBUG) {
      Tk.assert(pixmapIdsStore.present(drawableDest)
                || windowIdsStore.present(drawableDest));
      Tk.assert(gcIdsStore.present(gc));
    }  
    startRequest(X.COPY_AREA_REQUEST, 0, 0);
    output.writeInt(drawableSrc);
    output.writeInt(drawableDest);
    output.writeInt(gc);
    output.writeShort(x);
    output.writeShort(y);
    output.writeShort(destX);
    output.writeShort(destY);
    output.writeShort(width);
    output.writeShort(height);
    endRequest();
  }

  public final void copyArea(int drawable, int gc,
                             int x, int y, int width, int height,
                             int destX, int destY) {
    copyArea(drawable, drawable, gc, x, y, width, height, destX, destY);
  }

  long prevGraphRequest = -1;
  int prevRequestId = 0;
  int prevRequestDrawable = 0;
  int prevRequestGC = 0;

  private final void startGraphicsRequest(int request, int firstByte, int size,
                                          int drawable, int gc) {
// I check here for previous request to make Xlib optimization trick...
    if (X.DEBUG) Tk.assert(size % X.INT_SIZE == 0);
    if (X.DEBUG) Tk.assert(pixmapIdsStore.present(drawable)
                       || windowIdsStore.present(drawable));
    if (X.DEBUG) Tk.assert(gcIdsStore.present(gc));
    int shift;
    if (prevGraphRequest == requestCount && prevRequestId == request
        && prevRequestDrawable == drawable && prevRequestGC == gc
        && (shift = output.curRequestPos) != -1
        && output.cursor + size <= output.bufLength) {
      
      int curSize = getUnsignedShort(output.buffer, shift + 2);
      putShort(curSize + size / 4, output.buffer, shift + 2);
      if (X.DEBUG) {
        debugRequestBytesCount += size;
      }
    }
    else {
      startRequest(request, firstByte, size);
      output.writeInt(drawable);
      output.writeInt(gc);
      prevGraphRequest = requestCount;
      prevRequestId = request;
      prevRequestDrawable = drawable;
      prevRequestGC = gc;
    }
  }

  private static final int ORIGIN_COORDINATE_MODE = 0;

  public final void drawPoint(int drawable, int gc, int x, int y) {
    startGraphicsRequest(X.POLY_POINT_REQUEST, ORIGIN_COORDINATE_MODE, 4,
                         drawable, gc);
    output.writeShort(x);
    output.writeShort(y);

    endRequest();
  }

  public final void drawLine(int drawable, int gc,
                             int x, int y, int x2, int y2) {
    startGraphicsRequest(X.POLY_SEGMENT_REQUEST, X.UNUSED_BYTE, 8,
                         drawable, gc);
    output.writeShort(x);
    output.writeShort(y);
    output.writeShort(x2);
    output.writeShort(y2);

    endRequest();
  }

  public final void drawRectangle(int drawable, int gc,
                                  int x, int y, int width, int height) {
    if (width < 0 || height < 0) {
      return;
    }
    startGraphicsRequest(X.POLY_RECTANGLE_REQUEST, X.UNUSED_BYTE, 8,
                         drawable, gc);
    output.writeShort(x);
    output.writeShort(y);
    output.writeShort(width);
    output.writeShort(height);

    endRequest();
  }

  public final void drawArc(int drawable, int gc,
                            int x, int y, int width, int height,
                            int startAngle, int arcAngle) {
    if (width < 0 || height < 0) {
      return;
    }
    startAngle = (startAngle % 360) << 6; // << 6 == * 64
    arcAngle = Tk.bound(arcAngle, -360, 360) << 6;

    startGraphicsRequest(X.POLY_ARC_REQUEST, X.UNUSED_BYTE, 12,
                         drawable, gc);
    output.writeShort(x);
    output.writeShort(y);
    output.writeShort(width);
    output.writeShort(height);
    output.writeShort(startAngle);
    output.writeShort(arcAngle);

    endRequest();
  }

  public final void drawPolygon(int drawable, int gc,
                                int xOffset, int yOffset,
                                int xPoints[], int yPoints[], int nPoints) {
    if (nPoints > xPoints.length || nPoints > yPoints.length) {
      String s = nPoints + " > " + Math.min(xPoints.length, yPoints.length);
      throw new ArrayIndexOutOfBoundsException(s);
    }

    startGraphicsRequest(X.POLY_LINE_REQUEST, ORIGIN_COORDINATE_MODE, 
                         8 * nPoints, drawable, gc);
    int x, y, xFirst, yFirst;
    x = xFirst = xOffset + xPoints[0];
    y = yFirst = yOffset + yPoints[0];
    for (int i = 1; i < nPoints; ++i) {
      output.writeShort(x);
      output.writeShort(y);
      output.writeShort(x = xOffset + xPoints[i]);
      output.writeShort(y = yOffset + yPoints[i]);
    }
    output.writeShort(x);
    output.writeShort(y);
    output.writeShort(xFirst);
    output.writeShort(yFirst);

    endRequest();
  }

  public final void fillRectangle(int drawable, int gc,
                                  int x, int y, int width, int height) {
    if (width < 0 || height < 0) {
      return;
    }
    startGraphicsRequest(X.POLY_FILL_RECTANGLE_REQUEST, X.UNUSED_BYTE, 8,
                         drawable, gc);
    output.writeShort(x);
    output.writeShort(y);
    output.writeShort(width);
    output.writeShort(height);

    endRequest();
  }

  public final void fillArc(int drawable, int gc,
                            int x, int y, int width, int height,
                            int startAngle, int arcAngle) {
    if (width < 0 || height < 0) {
      return;
    }
    startAngle = (startAngle % 360) << 6; // << 6 == * 64
    arcAngle = Tk.bound(arcAngle, -360, 360) << 6;

    startGraphicsRequest(X.POLY_FILL_ARC_REQUEST, X.UNUSED_BYTE, 12,
                         drawable, gc);
    output.writeShort(x);
    output.writeShort(y);
    output.writeShort(width);
    output.writeShort(height);
    output.writeShort(startAngle);
    output.writeShort(arcAngle);

    endRequest();
  }

  private static final int COMPLEX_POLYGON_SHAPE = 0;

  public final void fillPolygon(int drawable, int gc,
                                int xOffset, int yOffset,
                                int xPoints[], int yPoints[], int nPoints) {
    if (nPoints > xPoints.length || nPoints > yPoints.length) {
      String s = nPoints + " > " + Math.min(xPoints.length, yPoints.length);
      throw new ArrayIndexOutOfBoundsException(s);
    }

    startGraphicsRequest(X.FILL_POLY_REQUEST, 0, 4 * nPoints,
                         drawable, gc);
    output.writeByte(COMPLEX_POLYGON_SHAPE);
    output.writeByte(ORIGIN_COORDINATE_MODE);
    output.skip(2);
    for (int i = 0; i < nPoints; ++i) {
      output.writeShort(xOffset + xPoints[i]);
      output.writeShort(yOffset + yPoints[i]);
    }

    endRequest();
  }

  static final int ZeroLeftPad = 0;

  final void putImageData(int drawable, int gc, int format, int depth,
                          int width, int height, int dstX, int dstY,
                          byte[] data, int offset, int length) {
    if (X.DEBUG) {
      Tk.assert(data.length >= offset + length);
      Tk.assert(pixmapIdsStore.present(drawable)
                || windowIdsStore.present(drawable));
      Tk.assert(gcIdsStore.present(gc));
      Tk.assert((length == 0 && height == 0) || length % height == 0);
    }  

    if (width <= 0 || height <= 0) { return; }

    int maxDataSize = maxRequestDataLength(X.PUT_IMAGE_REQUEST);
    int scanBytesCount = length / height;
    int blockHeight = maxDataSize / scanBytesCount;
    int h = height % blockHeight;
    int l = h * scanBytesCount;

    for (;;) {
      // I suppose that the maxDataSize is greater than one scan size
      // XIOError will be thrown otherwise by startRequest
      int pad = Tk.pad4(l);
      startRequest(X.PUT_IMAGE_REQUEST, format, l + pad);
      output.writeInt(drawable);
      output.writeInt(gc);
      output.writeShort(width);
      output.writeShort(h);
      output.writeShort(dstX);
      output.writeShort(dstY);
      output.writeByte(ZeroLeftPad);
      output.writeByte(depth);
      output.writeShort(X.UNUSED_SHORT);
      output.write(data, offset, l);
      output.skip(pad);
      endRequest();

      if (height == h) { break; }
      dstY += h;
      offset += l;
      height -= h;
      length -= l;
      h = blockHeight;
      l = h * scanBytesCount;
    }
  }

  private static final int MAX_TEXT_ITEM_LENGTH = 254;
  private static final int ZERO_TEXT_DELTA = 0;

  private boolean checkTextOutput(int drawable, int gc, 
                                  int arrayLength, int off, int len) {
    if (X.DEBUG) {
      Tk.assert(pixmapIdsStore.present(drawable)
                || windowIdsStore.present(drawable));
      Tk.assert(gcIdsStore.present(gc));
    }            
    if (arrayLength < off + len) {
      String s = (off + len) + " > " + arrayLength;
      throw new ArrayIndexOutOfBoundsException(s);
    }
    
    return len > 0;
  }                             

  private void startTextOutput(int request, int length, 
                              int drawable, int gc, int x, int y) {
    startRequest(request, X.UNUSED_BYTE, length);
    output.writeInt(drawable);
    output.writeInt(gc);
    output.writeShort(x);
    output.writeShort(y);
  }                                   

  public final void drawBytes(int drawable, int gc, int x, int y,
                              byte[] array, int off, int len) {
    if (!checkTextOutput(drawable, gc, array.length, off, len)) {
      return;
    }
    int itemCount = (len + MAX_TEXT_ITEM_LENGTH - 1) / MAX_TEXT_ITEM_LENGTH;
    int dataLength = itemCount * 2 * X.BYTE_SIZE + len * X.BYTE_SIZE;
    int pad = Tk.pad4(dataLength);
    startTextOutput(X.POLY_TEXT8_REQUEST, dataLength + pad,
                    drawable, gc, x, y);
    while (--itemCount > 0) {
      output.writeByte(MAX_TEXT_ITEM_LENGTH);
      output.writeByte(ZERO_TEXT_DELTA);
      output.write(array, off, MAX_TEXT_ITEM_LENGTH);
      off += MAX_TEXT_ITEM_LENGTH;
      len -= MAX_TEXT_ITEM_LENGTH;
    }
    output.writeByte(len);
    output.writeByte(ZERO_TEXT_DELTA);
    output.write(array, off, len);
    output.skip(pad);
    endRequest();
  }

  public final void drawChars(int drawable, int gc, int x, int y,
                              char[] array, int off, int len) {
    if (!checkTextOutput(drawable, gc, array.length, off, len)) {
      return;
    }

    int itemCount = (len + MAX_TEXT_ITEM_LENGTH - 1) / MAX_TEXT_ITEM_LENGTH;
    int dataLength = itemCount * 2 * X.BYTE_SIZE + len * X.CHAR_SIZE;
    int pad = Tk.pad4(dataLength);
    startTextOutput(X.POLY_TEXT16_REQUEST, dataLength + pad,
                    drawable, gc, x, y);
    while (--itemCount > 0) {
      output.writeByte(MAX_TEXT_ITEM_LENGTH);
      output.writeByte(ZERO_TEXT_DELTA);
      output.writeChars(array, off, MAX_TEXT_ITEM_LENGTH);
      off += MAX_TEXT_ITEM_LENGTH;
      len -= MAX_TEXT_ITEM_LENGTH;
    }
    output.writeByte(len);
    output.writeByte(ZERO_TEXT_DELTA);
    output.writeChars(array, off, len);
    output.skip(pad);
    endRequest();
  }

  public final void drawBytes(int drawable, int gc, int x, int y, byte[] b) {
    drawBytes(drawable, gc, x, y, b, 0, b.length);
  }

  public final void drawChars(int drawable, int gc, int x, int y, char[] c) {
    drawChars(drawable, gc, x, y, c, 0, c.length);
  }

  public final void drawString(int drawable, int gc, int x, int y, String s) {
    drawChars(drawable, gc, x, y, s.toCharArray());
  }

// The following functions are copied from Tk to permit inline optimization

  private static int getUnsignedShort(byte[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 2 == 0);
    return ((0xFF & array[offset]) << 8) | (0xFF & array[++offset]);
  }

  private static void putShort(int i, byte[] array, int offset) {
    if (X.DEBUG) Tk.assert(offset % 2 == 0);
    array[offset]   = (byte)(i >> 8);
    array[++offset] = (byte)(i);
  }

}

class XIdsStore {

  WindowSubsystem dpy;
  int length = 0;
  int capacity = 0;
  int[] ids = null;
  XIdsStore(WindowSubsystem dpy) {
    this.dpy = dpy;
  }
  
  final int newId() {
    if (length == capacity) {
      allocateMoreIds();
    }  
    return ids[length++];
  }
  
  final void freeId(int id) {
    if (X.DEBUG) Tk.assert(present(id));    
    int i = length - 1;
    if (ids[i] == id) {
      --length;
    }
    else {
      while (ids[--i] != id) { }
      System.arraycopy(ids, i + 1, ids, i, --length - i);
      ids[length] = id;
    }  
    if (length == 0) { 
      capacity = 0;
      ids = null; 
    }
  }
  
  boolean present(int id) {
    if (X.DEBUG) Tk.assert(dpy.isAllocatedId(id));    
    int i = length;
    while (i > 0) {
      if (ids[--i] == id) { return true; }
    }
    return false;
  }
  
  final boolean empty() { return length == 0; }

  private final void allocateMoreIds() {
    int curCapacity = capacity;
    if (curCapacity == 0) {
      ids = new int[capacity = X.ID_STORE_CAPACITY_INCREMENT];
    }
    else {
      int[] temp = new int[capacity += X.ID_STORE_CAPACITY_INCREMENT];
      System.arraycopy(ids, 0, temp, 0, curCapacity);
      ids = temp;
    }
    while (curCapacity < capacity) {
      ids[curCapacity++] = dpy.getNextResourceId();
    }
  }

}


class FreeRequestQueueNode {
  int id;
  int request;
  XIdsStore store;
  FreeRequestQueueNode next; 
  FreeRequestQueueNode(int id, int request, XIdsStore store) {
    this.id = id;
    this.request = request;
    this.store = store;
    this.next = null;
  }                     
}
