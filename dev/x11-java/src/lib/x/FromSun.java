/*
 * Copyright (c) 1997 Igor Boukanov, igor.boukanov@fi.uib.no
 *
 * Copyright (c) 1993-1996 Sun Microsystems, Inc. All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for NON-COMMERCIAL purposes and without
 * fee is hereby granted provided that this copyright notice
 * appears in all copies. 
 *
 * The Java source code is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You shall
 * not disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sun.

 * SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
*/

package lib.x;

class FromSun {

  static final byte[] X_FOUNDARY_ADOBE      = Tk.lowBytes("-adobe");
  static final byte[] X_FOUNDARY_B_AND_H    = Tk.lowBytes("-b&h");
  static final byte[] X_FOUNDARY_ITC        = Tk.lowBytes("-itc");
  static final byte[] X_FOUNDRY_DEFAULT     = Tk.lowBytes("-misc");
  static final byte[] X_FOUNDRY_ANY         = Tk.lowBytes("-*");

  static final byte[] X_FASENAME_HELVETICA  = Tk.lowBytes("-helvetica");
  static final byte[] X_FASENAME_TIMES      = Tk.lowBytes("-times");
  static final byte[] X_FASENAME_COURIER    = Tk.lowBytes("-courier");
  static final byte[] X_FASENAME_LUCIDA     = Tk.lowBytes("-lucida");
  static final byte[] X_FASENAME_TPEWRITER  = Tk.lowBytes("-lucidatypewriter");
  static final byte[] X_FASENAME_ZAPF_DINGBATS = Tk.lowBytes("-zapfdingbats");
  static final byte[] X_FASENAME_DEFAULT    = Tk.lowBytes("-fixed");

  static final byte[] X_STYLE_ITALIC        = Tk.lowBytes("-medium-i-*-*-");
  static final byte[] X_STYLE_BOLD          = Tk.lowBytes("-bold-r-*-*-");
  static final byte[] X_STYLE_BOLD_ITALIC   = Tk.lowBytes("-bold-i-*-*-");
  static final byte[] X_STYLE_PLAIN         = Tk.lowBytes("-medium-r-*-*-");
  static final byte[] X_STYLE_ANY           = Tk.lowBytes("-*-*-*-*-");

  static final byte[] X_ENCODING_LATIN1 = Tk.lowBytes("-*-*-*-*-*-iso8859-1");
  static final byte[] X_ENCODING_ANY    = Tk.lowBytes("-*-*-*-*-*-*-*");


  static final String HELVETICA     = "Helvetica";
  static final String TIMESROMAN    = "TimesRoman";
  static final String COURIER       = "Courier";
  static final String DIALOG        = "Dialog";
  static final String DIALOG_INPUT  = "DialogInput";
  static final String ZAPF_DINGBATS = "ZapfDingbats";

  static final XFontInfo loadFont(WindowSubsystem dpy, 
                                  String family, int style, int height) {

    if (height <= 0) { return null; }

    byte[] foundry;
    byte[] faceName;
    byte[] encoding;
    
    if (family.equals(HELVETICA)) {
      foundry  = X_FOUNDARY_ADOBE;
      faceName = X_FASENAME_HELVETICA;
      encoding = X_ENCODING_LATIN1;
    } 
    else if (family.equals(TIMESROMAN)) {
      foundry  = X_FOUNDARY_ADOBE;
      faceName = X_FASENAME_TIMES;
      encoding = X_ENCODING_LATIN1;
    } 
    else if (family.equals(COURIER)) {
      foundry  = X_FOUNDARY_ADOBE;
      faceName = X_FASENAME_COURIER;
      encoding = X_ENCODING_LATIN1;
    } 
    else if (family.equals(DIALOG)) {
      foundry  = X_FOUNDARY_B_AND_H;
      faceName = X_FASENAME_LUCIDA;
      encoding = X_ENCODING_LATIN1;
    } 
    else if (family.equals(DIALOG_INPUT)) {
      foundry  = X_FOUNDARY_B_AND_H;
      faceName = X_FASENAME_TPEWRITER;
      encoding = X_ENCODING_LATIN1;
    } 
    else if (family.equals(ZAPF_DINGBATS)) {
      foundry  = X_FOUNDARY_ITC;
      faceName = X_FASENAME_ZAPF_DINGBATS;
      encoding = X_ENCODING_ANY;
    } 
    else {
      foundry  = X_FOUNDRY_DEFAULT;
      faceName = X_FASENAME_DEFAULT;
      encoding = X_ENCODING_LATIN1;
    }

    byte[] styleStr;
    
    switch (style) {
    case java.awt.Font.ITALIC:
      styleStr = X_STYLE_ITALIC;
      break;
    case java.awt.Font.BOLD:
      styleStr = X_STYLE_BOLD;
      break;
    case java.awt.Font.BOLD+java.awt.Font.ITALIC:
      styleStr = X_STYLE_BOLD_ITALIC;
      break;
    case java.awt.Font.PLAIN:
    default:
      styleStr = X_STYLE_PLAIN;
    }

    byte[] heightBuf = new byte[11];

    int above = 0; /* tries above height */
    int below = 0; /* tries below height */
    int oheight = height; 

    int fid = dpy.newFontId();

    for (;;) {
      dpy.openFont(fid, foundry, faceName, styleStr, 
                   heightBuf, Tk.printInt(height, heightBuf), 
                   encoding);
      try {
        /* XXX: sometimes queryFont returns a bogus font structure */
        /* with negative ascent. */
        XFontInfo fontInfo = dpy.queryFont(fid);
        if (fontInfo.ascent >= 0) {
          return fontInfo;
        }
        dpy.freeFont(fontInfo);
        fid = dpy.newFontId();
      }  
      catch (XError e) {
      }
      if (foundry != X_FOUNDRY_ANY) {
        /* Try any other foundry before messing with the sizes */
        foundry = X_FOUNDRY_ANY;
      }
      else {
        /* We couldn't find the font. We'll try to find an */
        /* alternate by searching for heights above and below our */
        /* preferred height. We try for 4 heights above and below. */
        /* If we still can't find a font we repeat the algorithm */
        /* using misc-fixed as the font. If we then fail, then we */
        /* give up and signal an error. */
        if (above == below) {
          height = oheight + ++above;
        } else {
          below++;
          if (below > 4) {
            if (faceName != X_FASENAME_DEFAULT || styleStr != X_STYLE_ANY) {
              faceName = X_FASENAME_DEFAULT;
              foundry  = X_FOUNDRY_DEFAULT;
              height   = oheight;
              styleStr = X_STYLE_ANY;
              encoding = X_ENCODING_LATIN1;
              above = below = 0;
            } 
            else {
              dpy.fontIdsStore.freeId(fid);
              return null;
            }
          }
          height = oheight - below;
        }
      }  
    }
  }
  
  static void preallocateSomeColors(IndexColorTranslator ict) {

    ict.allocColor(0, 0, 0);
    ict.allocColor(255, 0, 0);
    ict.allocColor(0, 255, 0);
    ict.allocColor(0, 0, 255);
    ict.allocColor(255, 255, 0);
    ict.allocColor(255, 0, 255);
    ict.allocColor(0, 255, 255);
    ict.allocColor(235, 235, 235);
    ict.allocColor(224, 224, 224);
    ict.allocColor(214, 214, 214);
    ict.allocColor(192, 192, 192);
    ict.allocColor(162, 162, 162);
    ict.allocColor(128, 128, 128);
    ict.allocColor(105, 105, 105);
    ict.allocColor(64, 64, 64);
    ict.allocColor(32, 32, 32);
    ict.allocColor(255, 128, 128);
    ict.allocColor(128, 255, 128);
    ict.allocColor(128, 128, 255);
    ict.allocColor(255, 255, 128);
    ict.allocColor(255, 128, 255);
    ict.allocColor(128, 255, 255);
    ict.allocColor(255, 255, 255);

  } 
  
  static int awtCursorShapeToX(int awtCursor) {
    switch (awtCursor) {
    case java.awt.Frame.CROSSHAIR_CURSOR:
      return X.XC_CROSSHAIR;
    case java.awt.Frame.TEXT_CURSOR:
      return X.XC_XTERM;
    case java.awt.Frame.WAIT_CURSOR:
      return X.XC_WATCH;
    case java.awt.Frame.SW_RESIZE_CURSOR:
      return X.XC_BOTTOM_LEFT_CORNER;
    case java.awt.Frame.NW_RESIZE_CURSOR:
      return X.XC_TOP_LEFT_CORNER;
    case java.awt.Frame.SE_RESIZE_CURSOR:
      return X.XC_BOTTOM_RIGHT_CORNER;
    case java.awt.Frame.NE_RESIZE_CURSOR:
      return X.XC_TOP_RIGHT_CORNER;
    case java.awt.Frame.S_RESIZE_CURSOR:
      return X.XC_BOTTOM_SIDE;
    case java.awt.Frame.N_RESIZE_CURSOR:
      return X.XC_TOP_SIDE;
    case java.awt.Frame.W_RESIZE_CURSOR:
      return X.XC_LEFT_SIDE;
    case java.awt.Frame.E_RESIZE_CURSOR:
      return X.XC_RIGHT_SIDE;
    case java.awt.Frame.HAND_CURSOR:
      return X.XC_HAND2;
    case java.awt.Frame.MOVE_CURSOR:
      return X.XC_FLEUR;
    case java.awt.Frame.DEFAULT_CURSOR:
    default:
      return -1;
    }  
  } 
}
