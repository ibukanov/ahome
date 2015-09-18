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
/*
 * @(#)ImageRepresentation.java    1.40 96/03/30 Jim Graham
 *
 */

package lib.x;

import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageProducer;
import java.util.Hashtable;

public class ImageRepresentation implements ImageConsumer {

  int srcW;
  int srcH;
  int width;
  int height;
  int hints;

  int state;
  public static final int STATE_ZERO               = 0;
  public static final int STATE_WITH_SIZE          = 1;
  public static final int STATE_WITH_SIZE_AND_BITS = 2;
  public static final int STATE_COMPLETE           = 3;
  public static final int STATE_ABORTED            = 4;
  public static final int STATE_ERROR              = 5;
  
  ImageProducer producer;

  /**
   * Create an ImageRepresentation for the given Image scaled
   * to the given width and height and dithered or converted to
   * a ColorModel appropriate for the given image tag.
   */
  public ImageRepresentation(ImageProducer producer, int w, int h) {
    if (w < 0 || h < 0) {
      throw new IllegalArgumentException();
    }
    width = w;
    height = h;
    state = STATE_ZERO;
    this.producer = producer;
    producer.startProduction(this);
  }

  public void setDimensions(int w, int h) {
    synchronized(this) {
      srcW = w;
      srcH = h;
      Tk.dbg("Got image size:" + w + "x" + h);
      if (state == STATE_ZERO) {
        state = STATE_WITH_SIZE;
      }
      else {
        state = STATE_ERROR;
        Tk.dbg("Attempt to set image size in wrong state");
      }  
    }  
  }

  public void setProperties(Hashtable props) {
  }

  public void setColorModel(ColorModel model) {
  }

  public void setHints(int h) {
    synchronized (this) {
      hints = h;
    }  
  }

  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        byte pix[], int off, int scansize) {
    synchronized(this) {
      if (state == STATE_WITH_SIZE) {
        Tk.dbg("Got pixels size:" + x + "x" + y + ", " + w + "x" + h);
        state = STATE_WITH_SIZE_AND_BITS;
      }
      else if (state != STATE_WITH_SIZE_AND_BITS) {
        state = STATE_ERROR;
        Tk.dbg("Attempt to set pixels size in wrong state");
      }  
    }  
  }

  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        int pix[], int off, int scansize) {
    synchronized(this) {
      if (state == STATE_WITH_SIZE) {
        Tk.dbg("Got pixels size:" + x + "x" + y + ", " + w + "x" + h);
        state = STATE_WITH_SIZE_AND_BITS;
      }
      else if (state != STATE_WITH_SIZE_AND_BITS) {
        state = STATE_ERROR;
        Tk.dbg("Attempt to set pixels size in wrong state");
      }  
    }  
  }

  public void imageComplete(int status) {
    int newState;
    switch (status) {
    case ImageConsumer.IMAGEABORTED:
      newState = STATE_ABORTED;
      break;
    case ImageConsumer.IMAGEERROR:
      newState = STATE_ERROR;
//      dispose();
      break;
    case ImageConsumer.STATICIMAGEDONE:
      newState = STATE_COMPLETE;
      break;
    case ImageConsumer.SINGLEFRAMEDONE:
      newState = STATE_COMPLETE;
      break;
    default:
      newState = STATE_ABORTED;
    }
    producer.removeConsumer(this);
    producer = null;
    Tk.dbg("Image done is state " + state);
    synchronized (this) {
      state = newState;
//      notifyAll();
    }
    Tk.dbg("Image done is state " + state);
  }

/*
  public boolean drawImage(Graphics g, int x, int y, Color c,
               ImageObserver iw) {
    if (src != null) {
      src.checkSecurity(null, false);
    }
    if ((availinfo & ImageObserver.ERROR) != 0) {
      if (iw != null) {
        iw.imageUpdate(image, ImageObserver.ERROR|ImageObserver.ABORT,
                 -1, -1, -1, -1);
      }
      return false;
    }
    boolean done = ((availinfo & ImageObserver.ALLBITS) != 0);
    if (!done) {
      addWatcher(iw);
      startProduction();
    }
    imageDraw(g, x, y, c);
    return done;
  }

  void abort() {
    ImageProducer producer = this.producer;
    if (producer != null) {
      producer.removeConsumer(this);
    }
//    disposeImage();
  }

  public void finalize() {
//    disposeImage();
  }

*/
}
