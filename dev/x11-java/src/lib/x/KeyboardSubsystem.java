package lib.x;

// Important note:
// The methods of this class are thread safe although they do not use 
// any synchronization. 
// This cames from the fact that assignment in setupKeyboardSupport()
// is atomic operation.

public class KeyboardSubsystem extends WindowSubsystem {

  KeyboardMapState keyboardMapState = null;
  
  KeyboardSubsystem(String displayName) {
    super(displayName);
  }
  
  public void setupKeyboardSupport() {
    try {
      keyboardMapState = new KeyboardMapState(this);
    }
    catch (XError e) {
      System.err.println("Initialization of keyboard support failed");
    }  
  }

  int translateKeycodeToKeysym(int keycode, int modifiers) {
    if (keyboardMapState == null) {
      return 0;
    }  
    return keyboardMapState.keycodeToKeysym(keycode, modifiers);
  }
  
  int mapKeyboardModifiers(int shift, int control, int alt, int meta,
                           int modifiers) {
    if (keyboardMapState == null) {
      return 0;
    }  
    return keyboardMapState.mapModifiers(shift, control, alt, meta, modifiers);
  }

  public final int awtModifiers(int modifiers) {
    int ret;
    if (keyboardMapState == null) {
      ret = 0;
    }
    else {  
      ret = keyboardMapState.mapModifiers(java.awt.Event.SHIFT_MASK, 
                                          java.awt.Event.CTRL_MASK,
                                          java.awt.Event.ALT_MASK,
                                          java.awt.Event.META_MASK,
                                          modifiers);
    }
    
    if ((modifiers & X.BUTTON2_MASK) != 0) {
      ret |= java.awt.Event.ALT_MASK;
    }
    if ((modifiers & X.BUTTON3_MASK) != 0) {
      ret |= java.awt.Event.META_MASK;
    }
    return ret;
  }

  public final int awtKey(int keycode, int modifiers) {
    if (keyboardMapState == null) {
      return 0;
    }
    
    int key = keyboardMapState.keycodeToKeysym(keycode, modifiers);

    if (key < 256) {
      // I got symbol from Latin1
      if (0 != (modifiers & X.CONTROL_KEY_MASK)) {
        // Translation to ASCII control char 
        switch (key) {
        case '[': case ']': case '\\': case '_':
          key -= 64;
          break;
        default:
          if (key >= X.XK_A && key <= X.XK_Z) {
            key -= X.XK_A - 1;
          }
          else if (key >= X.XK_a && key <= X.XK_z) {
            key -= X.XK_a - 1;
          }
        }
      }
    }
    else {
      switch (key) {
      case X.XK_BackSpace:
        key = '\010';
        break;
      case X.XK_Delete:
      case X.XK_KP_Delete:
        key = '\177';
        break;
      case X.XK_Tab:
        key = '\t';
        break;
      case X.XK_Return:
      case X.XK_Linefeed:
      case X.XK_KP_Enter:
        key = '\n';
        break;
      case X.XK_F27:
      case X.XK_Home:
        /* Home */
        key = java.awt.Event.HOME;
        break;
      case X.XK_F1:
        key = java.awt.Event.F1;
        break;
      case X.XK_F2:
        key = java.awt.Event.F2;
        break;
      case X.XK_F3:
        key = java.awt.Event.F3;
        break;
      case X.XK_F4:
        key = java.awt.Event.F4;
        break;
      case X.XK_F5:
        key = java.awt.Event.F5;
        break;
      case X.XK_F6:
        key = java.awt.Event.F6;
        break;
      case X.XK_F7:
        key = java.awt.Event.F7;
        break;
      case X.XK_F8:
        key = java.awt.Event.F8;
        break;
      case X.XK_F9:
        key = java.awt.Event.F9;
        break;
      case X.XK_F10:
        key = java.awt.Event.F10;
        break;
      case X.XK_F11:
        key = java.awt.Event.F11;
        break;
      case X.XK_F12:
        key = java.awt.Event.F12;
        break;
      case X.XK_R13:
      case X.XK_End:
        /* End */
        key = java.awt.Event.END;
        break;
      case X.XK_F29:
      case X.XK_Page_Up:
        /* PgUp */
        key = java.awt.Event.PGUP;
        break;
      case X.XK_F35:
      case X.XK_Page_Down:
        /* PgDn */
        key = java.awt.Event.PGDN;
        break;
      case X.XK_Up:
        /* Up */
        key = java.awt.Event.UP;
        break;
      case X.XK_Down:
        /* Down */
        key = java.awt.Event.DOWN;
        break;
      case X.XK_Left:
        /* Left */
        key = java.awt.Event.LEFT;
        break;
      case X.XK_Right:
        /* Right */
        key = java.awt.Event.RIGHT;
        break;
      case X.XK_Escape:
        key = 27;
        break;
      case X.XK_KP_Decimal:
        key = '.';
        break;
      case X.XK_KP_Add:
        key = '+';
        break;
      case X.XK_KP_Subtract:
        key = '-';
        break;
      case X.XK_KP_Divide:
        key = '/';
        break;
      case X.XK_KP_Multiply:
        key = '*';
        break;
      case X.XK_KP_0:
        key = '0';
        break;
      case X.XK_KP_1:
        key = '1';
        break;
      case X.XK_KP_2:
        key = '2';
        break;
      case X.XK_KP_3:
        key = '3';
        break;
      case X.XK_KP_4:
        key = '4';
        break;
      case X.XK_KP_5:
        key = '5';
        break;
      case X.XK_KP_6:
        key = '6';
        break;
      case X.XK_KP_7:
        key = '7';
        break;
      case X.XK_KP_8:
        key = '8';
        break;
      case X.XK_KP_9:
        key = '9';
        break;
      default:
        key = 0;
      }
    }
    return key;
  }

}

final class KeyboardMapState {
  KeyboardSubsystem dpy;
  int               keycodeMin;
  int               keycodeCount;
  int               maxMapShift;
  boolean           xLockIsCaps         = false;
  int               xLockMask           = 0x1;
  int               xAltMask            = 0;
  int               xMetaMask           = 0;
  int               xModeSwitchMask     = 0;
  int               xNumLockMask        = 0;

  int               keycodeMap[]        = null;
  byte              modifierMap[]       = null;
  int               numKeyPerModifier   = 1;

  KeyboardMapState(KeyboardSubsystem dpy) throws XError {
    this.dpy     = dpy;
    keycodeMin   = dpy.minKeycode;
    keycodeCount = dpy.maxKeycode - keycodeMin + 1;
    maxMapShift  = keycodeCount * X.KEYSYMS_PER_KEYCODE;

    XInput input = dpy.dataInput;

    dpy.startRequest(X.GET_MODIFIER_MAPPING_REQUEST, 0, 0, 
                     X.WAIT_SERVER_ANSWER);
    dpy.endRequest();
    byte[] reply = dpy.getReply();
    numKeyPerModifier = 0xFF & reply[1];
    modifierMap = new byte[X.KEYBOARD_MODIFIERS_COUNT * numKeyPerModifier];
    input.read(modifierMap);
    dpy.finishReply();
    
    dpy.startRequest(X.GET_KEYBOARD_MAPPING_REQUEST, 0, 0, 
                     X.WAIT_SERVER_ANSWER);
    dpy.output.writeByte(dpy.minKeycode);
    dpy.output.writeByte(keycodeCount);
    dpy.output.pad4();
    dpy.endRequest();

    reply = dpy.getReply();
    int serverKeysymPerKeycode = reply[1];

    int min = Math.min(X.KEYSYMS_PER_KEYCODE, serverKeysymPerKeycode);
    keycodeMap = new int[keycodeCount * X.KEYSYMS_PER_KEYCODE];

    for (int shift = 0; shift < maxMapShift; shift += X.KEYSYMS_PER_KEYCODE) {
      int j;
      for (j = 0; j < min; ++j) {
        keycodeMap[shift + j] = input.readInt();
      }
      if (j < serverKeysymPerKeycode) { 
        input.skip(X.INT_SIZE * (serverKeysymPerKeycode - j));
      }  
    }
    dpy.finishReply();

    setupMaps();
  }
  
  private int findKeysymForModifier(int modifier) {
    int shift = (0xFF & modifierMap[modifier] - keycodeMin) 
                * X.KEYSYMS_PER_KEYCODE;
    if (shift >= 0 && shift < maxMapShift) {
      int j = X.KEYSYMS_PER_KEYCODE;
      while (j > 0) {
        --j;
        int keysym = keycodeMap[shift++];
        if (X.NoSymbol != keysym) { return keysym; }
      }
    }
    return X.NoSymbol;
  }

  private void setupMaps() {  
    
    for (int shift = 0; shift < maxMapShift; shift += X.KEYSYMS_PER_KEYCODE) {
      long result;
      int k0 = keycodeMap[shift + 0];
      int k1 = keycodeMap[shift + 1];
      if (k1 == X.NoSymbol) {
        result = convertKeysymCase(k0);
        k0 = keycodeMap[shift + 0] = Tk.lowPart(result);
        k1 = keycodeMap[shift + 1] = Tk.upperPart(result);
      }
      int k2 = keycodeMap[shift + 2];
      int k3 = keycodeMap[shift + 3];
      if (k3 == X.NoSymbol) {
        if (k2 == X.NoSymbol) {
          keycodeMap[shift + 2] = k0;
          keycodeMap[shift + 3] = k1;
        }
        else {
          result = convertKeysymCase(k2);
          keycodeMap[shift + 2] = Tk.lowPart(result);
          keycodeMap[shift + 3] = Tk.upperPart(result);
        }
      }
    }
    
    // Now I can look for the Lock meaning
    xLockIsCaps = false;
    boolean lockIsShift = false;

    xLockMask = X.LOCK_KEY_MASK;

    int iMin = numKeyPerModifier * X.LOCK_KEY_INDEX;
    int iMax = numKeyPerModifier * (X.LOCK_KEY_INDEX + 1);
    loop: 
    for (int i = iMin; i < iMax; ++i) {
      int keysym = findKeysymForModifier(i);
      switch (keysym) {
      case X.XK_ISO_Lock: case X.XK_Caps_Lock: xLockIsCaps = true; break loop;
      case X.XK_Shift_Lock: lockIsShift = true; break;
      }
    }
    if (!xLockIsCaps && !lockIsShift) {
      xLockMask = 0;
    }

    // It is time to get mode, numlock, alt and meta switch
    xModeSwitchMask = xNumLockMask = xAltMask = xMetaMask = 0;

    iMin = numKeyPerModifier * X.MOD1_KEY_INDEX;
    iMax = numKeyPerModifier * (X.MOD5_KEY_INDEX + 1);
    for (int i = iMin; i < iMax; ++i) {
      int keysym = findKeysymForModifier(i);
      switch (keysym) {
      case X.XK_Mode_switch:
        xModeSwitchMask |= 1 << (i / numKeyPerModifier);
        break;
      case X.XK_Num_Lock:
        xNumLockMask |= 1 << (i / numKeyPerModifier);
        break;
      case X.XK_Alt_R: case X.XK_Alt_L:
        xAltMask |= 1 << (i / numKeyPerModifier);
        break;
      case X.XK_Meta_R: case X.XK_Meta_L:
        xMetaMask |= 1 << (i / numKeyPerModifier);
        break;
      }
    }
  }

  int mapModifiers(int shift, int control, int alt, int meta,int modifiers) {
    int ret = 0;
    if (0 != (modifiers & X.SHIFT_KEY_MASK)) { ret |= shift; }
    if (0 != (modifiers & X.CONTROL_KEY_MASK)) { ret |= control; }
    if (0 != (modifiers & xAltMask)) { ret |= alt; }
    if (0 != (modifiers & xMetaMask)) { ret |= meta; }
    return ret;
  }
  
  int keycodeToKeysym(int keycode, int modifiers) {
    int symShift = (keycode - keycodeMin) * X.KEYSYMS_PER_KEYCODE;
    if (symShift < 0 || symShift >= maxMapShift) {
      return X.NoSymbol;
    }

    if (0 != (modifiers & xModeSwitchMask)) {
      symShift += 2;
    }
    int keySym0 = keycodeMap[symShift + 0];
    int keySym1 = keycodeMap[symShift + 1];

    int sym;

    boolean isLock = (0 != (modifiers & xLockMask));
    boolean isShift =
      (0 != (modifiers & X.SHIFT_KEY_MASK)) || (isLock && !xLockIsCaps);
    boolean isCaps = isLock && xLockIsCaps;
    long tmp;
    if (0 != (modifiers & xNumLockMask) && keypadKey(keySym1)) {
      sym = isShift ? keySym0 : keySym1;
    }
    else if (!isShift) {
      sym = keySym0;
      if (isCaps && sym == Tk.lowPart(tmp = convertKeysymCase(sym))) {
        sym = Tk.upperPart(tmp);
      }
    }
    else {
      sym = keySym1;
      if (isCaps && sym == Tk.upperPart(tmp = convertKeysymCase(sym))) {
        sym = Tk.lowPart(tmp);
      }
    }
    return (sym == X.VoidSymbol) ? X.NoSymbol : sym;
  }

  private static boolean keypadKey(int keysym) {
    return  (keysym >= X.XK_KP_Space && keysym <= X.XK_KP_Equal)
            || (keysym >= 0x11000000 && keysym <= 0x1100FFFF);
  }

  private static long convertKeysymCase(int sym) {
    switch(sym >>> 8) {
    case 0: // Latin 1
      if (sym >= X.XK_A && sym <= X.XK_Z) {
        return Tk.makeLong(sym, sym + (X.XK_a - X.XK_A));
      }
      else if (sym >= X.XK_a && sym <= X.XK_z) {
        return Tk.makeLong(sym - (X.XK_a - X.XK_A), sym);
      }
      else if (sym >= X.XK_Agrave && sym <= X.XK_Odiaeresis) {
        return Tk.makeLong(sym, sym + (X.XK_agrave - X.XK_Agrave));
      }
      else if (sym >= X.XK_agrave && sym <= X.XK_odiaeresis) {
        return Tk.makeLong(sym - (X.XK_agrave - X.XK_Agrave), sym);
      }
      else if (sym >= X.XK_Ooblique && sym <= X.XK_Thorn) {
        return Tk.makeLong(sym, sym + (X.XK_oslash - X.XK_Ooblique));
      }
      else if (sym >= X.XK_oslash && sym <= X.XK_thorn) {
        return Tk.makeLong(sym - (X.XK_oslash - X.XK_Ooblique), sym);
      }

    case 1: // Latin 2
      /* Assume the KeySym is a legal value (ignore discontinuities) */
      if (sym == X.XK_Aogonek || sym == X.XK_aogonek) {
        return Tk.makeLong(X.XK_Aogonek, X.XK_aogonek);
      }
      else if (sym >= X.XK_Lstroke && sym <= X.XK_Sacute) {
        return Tk.makeLong(sym, sym + (X.XK_lstroke - X.XK_Lstroke));
      }
      else if (sym >= X.XK_Scaron && sym <= X.XK_Zacute) {
        return Tk.makeLong(sym, sym + (X.XK_scaron - X.XK_Scaron));
      }
      else if (sym >= X.XK_Zcaron && sym <= X.XK_Zabovedot) {
        return Tk.makeLong(sym, sym + (X.XK_zcaron - X.XK_Zcaron));
      }
      else if (sym >= X.XK_lstroke && sym <= X.XK_sacute) {
        return Tk.makeLong(sym - (X.XK_lstroke - X.XK_Lstroke), sym);
      }
      else if (sym >= X.XK_scaron && sym <= X.XK_zacute) {
        return Tk.makeLong(sym - (X.XK_scaron - X.XK_Scaron), sym);
      }
      else if (sym >= X.XK_zcaron && sym <= X.XK_zabovedot) {
        return Tk.makeLong(sym - (X.XK_zcaron - X.XK_Zcaron), sym);
      }
      else if (sym >= X.XK_Racute && sym <= X.XK_Tcedilla) {
        return Tk.makeLong(sym, sym + (X.XK_racute - X.XK_Racute));
      }
      else if (sym >= X.XK_racute && sym <= X.XK_tcedilla) {
        return Tk.makeLong(sym - (X.XK_racute - X.XK_Racute), sym);
      }

    case 2: // Latin 3
      /* Assume the KeySym is a legal value (ignore discontinuities) */
      if (sym >= X.XK_Hstroke && sym <= X.XK_Hcircumflex) {
        return Tk.makeLong(sym, sym + (X.XK_hstroke - X.XK_Hstroke));
      }
      else if (sym >= X.XK_Gbreve && sym <= X.XK_Jcircumflex) {
        return Tk.makeLong(sym, sym + (X.XK_gbreve - X.XK_Gbreve));
      }
      else if (sym >= X.XK_hstroke && sym <= X.XK_hcircumflex) {
        return Tk.makeLong(sym - (X.XK_hstroke - X.XK_Hstroke), sym);
      }
      else if (sym >= X.XK_gbreve && sym <= X.XK_jcircumflex) {
        return Tk.makeLong(sym - (X.XK_gbreve - X.XK_Gbreve), sym);
      }
      else if (sym >= X.XK_Cabovedot && sym <= X.XK_Scircumflex) {
        return Tk.makeLong(sym, sym + (X.XK_cabovedot - X.XK_Cabovedot));
      }
      else if (sym >= X.XK_cabovedot && sym <= X.XK_scircumflex) {
        return Tk.makeLong(sym - (X.XK_cabovedot - X.XK_Cabovedot), sym);
      }

    case 3: // Latin 4
      /* Assume the KeySym is a legal value (ignore discontinuities) */
      if (sym >= X.XK_Rcedilla && sym <= X.XK_Tslash) {
        return Tk.makeLong(sym, sym + (X.XK_rcedilla - X.XK_Rcedilla));
      }
      else if (sym >= X.XK_rcedilla && sym <= X.XK_tslash) {
        return Tk.makeLong(sym - (X.XK_rcedilla - X.XK_Rcedilla), sym);
      }
      else if (sym == X.XK_ENG || sym == X.XK_eng) {
        return Tk.makeLong(X.XK_ENG, X.XK_eng);
      }
      else if (sym >= X.XK_Amacron && sym <= X.XK_Umacron) {
        return Tk.makeLong(sym, sym + (X.XK_amacron - X.XK_Amacron));
      }
      else if (sym >= X.XK_amacron && sym <= X.XK_umacron) {
        return Tk.makeLong(sym - (X.XK_amacron - X.XK_Amacron), sym);
      }

    case 6: // Cyrillic
      /* Assume the KeySym is a legal value (ignore discontinuities) */
      if (sym >= X.XK_Serbian_DJE && sym <= X.XK_Serbian_DZE) {
        return Tk.makeLong(sym, sym - (X.XK_Serbian_DJE - X.XK_Serbian_dje));
      }
      else if (sym >= X.XK_Serbian_dje && sym <= X.XK_Serbian_dze) {
        return Tk.makeLong(sym + (X.XK_Serbian_DJE - X.XK_Serbian_dje), sym);
      }
      else if (sym >= X.XK_Cyrillic_YU && sym <= X.XK_Cyrillic_HARDSIGN) {
        return Tk.makeLong(sym, sym - (X.XK_Cyrillic_YU - X.XK_Cyrillic_yu));
      }
      else if (sym >= X.XK_Cyrillic_yu && sym <= X.XK_Cyrillic_hardsign) {
        return Tk.makeLong(sym + (X.XK_Cyrillic_YU - X.XK_Cyrillic_yu), sym);
      }
    }
    return Tk.makeLong(sym, sym);
  }
}
