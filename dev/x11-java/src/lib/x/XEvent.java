package lib.x;

import java.io.*; 

//import lib.x.event.*; 

public class XEvent {

  public static final int KEY_PRESS = 2;
  public static final int KEY_RELEASE = 3;
  public static final int BUTTON_PRESS = 4;
  public static final int BUTTON_RELEASE = 5;
  public static final int MOTION_NOTIFY = 6;
  public static final int ENTER_NOTIFY = 7;
  public static final int LEAVE_NOTIFY = 8;
  public static final int FOCUS_IN = 9;
  public static final int FOCUS_OUT = 10;
  public static final int KEYMAP_NOTIFY = 11;
  public static final int EXPOSE = 12;
  public static final int GRAPHICS_EXPOSURE = 13;
  public static final int NO_EXPOSURE = 14;
  public static final int VISIBILITY_NOTIFY = 15;
  public static final int CREATE_NOTIFY = 16;
  public static final int DESTROY_NOTIFY = 17;
  public static final int UNMAP_NOTIFY = 18;
  public static final int MAP_NOTIFY = 19;
  public static final int MAP_REQUEST = 20;
  public static final int REPARENT_NOTIFY = 21;
  public static final int CONFIGURE_NOTIFY = 22;
  public static final int CONFIGURE_REQUEST = 23;
  public static final int GRAVITY_NOTIFY = 24;
  public static final int RESIZE_REQUEST = 25;
  public static final int CIRCULATE_NOTIFY = 26;
  public static final int CIRCULATE_REQUEST = 27;
  public static final int PROPERTY_NOTIFY = 28;
  public static final int SELECTION_CLEAR = 29;
  public static final int SELECTION_REQUEST = 30;
  public static final int SELECTION_NOTIFY = 31;
  public static final int COLORMAP_NOTIFY = 32;
  public static final int CLIENT_MESSAGE = 33;
  public static final int MAPPING_NOTIFY = 34;

  private static final String[] eventNames = {
    "KEY_PRESS",
    "KEY_RELEASE",
    "BUTTON_PRESS",
    "BUTTON_RELEASE",
    "MOTION_NOTIFY",
    "ENTER_NOTIFY",
    "LEAVE_NOTIFY",
    "FOCUS_IN",
    "FOCUS_OUT",
    "KEYMAP_NOTIFY",
    "EXPOSE",
    "GRAPHICS_EXPOSURE",
    "NO_EXPOSURE",
    "VISIBILITY_NOTIFY",
    "CREATE_NOTIFY",
    "DESTROY_NOTIFY",
    "UNMAP_NOTIFY",
    "MAP_NOTIFY",
    "MAP_REQUEST",
    "REPARENT_NOTIFY",
    "CONFIGURE_NOTIFY",
    "CONFIGURE_REQUEST",
    "GRAVITY_NOTIFY",
    "RESIZE_REQUEST",
    "CIRCULATE_NOTIFY",
    "CIRCULATE_REQUEST",
    "PROPERTY_NOTIFY",
    "SELECTION_CLEAR",
    "SELECTION_REQUEST",
    "SELECTION_NOTIFY",
    "COLORMAP_NOTIFY",
    "CLIENT_MESSAGE",
    "MAPPING_NOTIFY",
  };  
  
  private static final int[] windowIdOffsets = {
    12, // KEY_PRESS
    12, // KEY_RELEASE
    12, // BUTTON_PRESS
    12, // BUTTON_RELEASE
    12, // MOTION_NOTIFY
    12, // ENTER_NOTIFY
    12, // LEAVE_NOTIFY
     4, // FOCUS_IN
     4, // FOCUS_OUT
    -1, // KEYMAP_NOTIFY
     4, // EXPOSE
     4, // GRAPHICS_EXPOSURE
     4, // NO_EXPOSURE
     4, // VISIBILITY_NOTIFY
     8, // CREATE_NOTIFY
     8, // DESTROY_NOTIFY
     8, // UNMAP_NOTIFY
     8, // MAP_NOTIFY
     8, // MAP_REQUEST
     8, // REPARENT_NOTIFY
     8, // CONFIGURE_NOTIFY
     8, // CONFIGURE_REQUEST
     8, // GRAVITY_NOTIFY
     4, // RESIZE_REQUEST
     8, // CIRCULATE_NOTIFY
     8, // CIRCULATE_REQUEST
     4, // PROPERTY_NOTIFY
    -1, // SELECTION_CLEAR
    -1, // SELECTION_REQUEST
    -1, // SELECTION_NOTIFY
     4, // COLORMAP_NOTIFY
     4, // CLIENT_MESSAGE
    -1, // MAPPING_NOTIFY
  };

  public static String eventName(int opcode) {
    if (opcode < KEY_PRESS || MAPPING_NOTIFY < opcode) {
      return "Unknown(" + opcode + ')';
    }
    return eventNames[opcode - KEY_PRESS];
  }

  public static int windowIdFromEvent(byte[] data, int shift, int opcode) {
    if (opcode < KEY_PRESS || MAPPING_NOTIFY < opcode) {
      return 0;
    }
    int idOffset = windowIdOffsets[opcode - KEY_PRESS];
    if (idOffset == -1) {
      return 0;
    }
    return Tk.getInt(data, shift + idOffset);
  }

}


