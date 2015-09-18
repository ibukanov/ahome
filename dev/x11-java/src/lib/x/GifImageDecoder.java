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
 * @(#)GifImageDecoder.java    1.24 96/03/26 Patrick Naughton, Arthur van Hoff
 *
 */

/**
 * The algorithm is copyright of CompuServe.
 */

package lib.x;

import java.io.*;

/**
 * Gif Image converter
 * 
 * @version 1.24 03/26/96
 * @author Arthur van Hoff
 */
public class GifImageDecoder {
  private final boolean verbose = false;

  private static final int IMAGESEP         = 0x2c;
  private static final int EXBLOCK         = 0x21;
  private static final int EX_GRAPHICS_CONTROL= 0xf9;
  private static final int EX_COMMENT     = 0xfe;
  private static final int EX_APPLICATION     = 0xff;
  private static final int TERMINATOR     = 0x3b;
  private static final int INTERLACEMASK     = 0x40;
  private static final int COLORMAPMASK     = 0x80;

  int num_colors;
  byte[] colormap;
  int[] pixelMap;
  
  XImageData pixmapData;
  XBitmapData transPositions;

  public final XImageData pixmapData() { return pixmapData; }
  public final XBitmapData transPositions() { return transPositions; }
 
  
  Display dpy;
  

  /**
   * An error has occurred. Throw an exception.
   */
  private static void error(String s1) throws Exception {
    throw new Exception(s1);
  }

  /**
   * Read a number of bytes into a buffer.
   */
  void readBytes(InputStream input, byte buf[], int off, int len) 
    throws IOException {

    while (len > 0) {
      int n = input.read(buf, off, len);
      if (n == -1) {
        throw new EOFException(); 
      }
      off += n;
      len -= n;
    }
  }

  /**
   * produce an image from the stream.
   */
   
  int trans_pixel = -1;
   
  public GifImageDecoder(InputStream is, Display dpy, ColorTranslator ct) 
                         throws IOException, Exception {

    this.dpy = dpy;
    // Create a buffer
    byte byteBuf[] = new byte[13];

    // Read the header
    readBytes(is, byteBuf, 0, 13);

    // Check header
    if ((byteBuf[0] != 'G') || (byteBuf[1] != 'I') || (byteBuf[2] != 'F')) {
      error("not a GIF file.");
    }

    // colormap info
    int ch = byteBuf[10] & 0xFF;
    if ((ch & COLORMAPMASK) == 0) {
      error("no global colormap in GIF file.");
    }
    num_colors = 1 << ((ch & 0x7) + 1);

    // supposed to be NULL
    if (byteBuf[12] != 0) {
//      props.put("aspectratio", ""+(((byteBuf[12] & 0xff) + 15) / 64.0));
    }

    // Read colors
    colormap = new byte[num_colors * 3];
    readBytes(is, colormap, 0, num_colors * 3);
    pixelMap = new int[num_colors];
    int i, j;
    i = num_colors;
    j = i * 3;
    while (i > 0) {
      int b = 0xFF & colormap[--j];
      int g = 0xFF & colormap[--j];
      int r = 0xFF & colormap[--j];
      pixelMap[--i] = ct.closeDevicePixel(r, g, b); 
    }

    while (true) {
      int code;

      switch (code = is.read()) {
      case EXBLOCK:
        switch (code = is.read()) {
        case EX_GRAPHICS_CONTROL: 
          try { readBytes(is, byteBuf, 0, 6); } 
          catch (IOException e) { return; }
          if ((byteBuf[0] != 4) || (byteBuf[5] != 0)) {
            return;//error("corrupt GIF file (GCE size)");
          }
          // Get the index of the transparent color
          trans_pixel = byteBuf[4] & 0xFF;
          break;

          case EX_COMMENT:
          case EX_APPLICATION:
          default:
          String comment = "";
          while (true) {
            int n = is.read();
            if (n == 0) {
              break;
            }
            byte buf[] = new byte[n];
            try { readBytes(is, buf, 0, n); } catch (IOException e) { return; }
            if (code == EX_COMMENT) {
              comment += new String(buf, 0);
            }
          }
          if (code == EX_COMMENT) {
//            props.put("comment", comment);
          }
          break;
        }
        break;

        case IMAGESEP:
          if (readImage(is, dpy)) {
            imageComplete();
          }
          return;

        case TERMINATOR:
        return;

        case -1:
        return;

        default:
        return;//error("corrupt GIF file (parse) [" + code + "].");
        //break;
      }
    }
  }


  /**
   * The ImageConsumer hints flag for a non-interlaced GIF image.
   */
//  private static final int normalflags =
//    ImageConsumer.TOPDOWNLEFTRIGHT | ImageConsumer.COMPLETESCANLINES |
//    ImageConsumer.SINGLEPASS | ImageConsumer.SINGLEFRAME;

  /**
   * The ImageConsumer hints flag for an interlaced GIF image.
   */
//  private static final int interlaceflags =
//    ImageConsumer.RANDOMPIXELORDER | ImageConsumer.COMPLETESCANLINES |
//    ImageConsumer.SINGLEPASS | ImageConsumer.SINGLEFRAME;

  final void sendPixels(int y, int width, int rasline[], int off) {
    pixmapData.putDevicePixels(0, y, width, 1, rasline, 0, width);
    if (transPositions != null) {
      transPositions.putDevicePixels(0, y, width, 1, rasline, off, width);
    }
  }

  public void imageComplete() {
  }

  /**
   * Read Image data
   */
  private boolean readImage(InputStream is, Display dpy) throws IOException {
    long tm = System.currentTimeMillis();

    // Allocate the buffer
    byte block[] = new byte[256 + 3];

    // Read the image descriptor
    readBytes(is, block, 0, 10);
    int width = (block[4] & 0xFF) | ((block[5] & 0xFF) << 8);
    int height = (block[6] & 0xFF) | ((block[7] & 0xFF) << 8);
    
    pixmapData = PixmapData.create(dpy, width, height);
    if (trans_pixel != -1) {
      pixelMap[trans_pixel] = pixmapData.transparentMask();
      transPositions = new XBitmapData(dpy, 
                                       pixmapData.width(), pixmapData.height(),
                                       pixmapData.transparentMask());
    }  
    
    boolean interlace = (block[8] & INTERLACEMASK) != 0;

    int initCodeSize = block[9] & 0xFF;

//    setDimensions(width, height);
    
//    store.setColorModel(model);

//    int hints = (interlace ? interlaceflags : normalflags);
//    source.setHints(hints);

    // allocate the raster data
    int rasline[] = new int[width];

    boolean ret;
    try {
      parseImage(is, width, height, interlace, initCodeSize,
                               block, rasline);
      ret = true;                         
    }
    catch (IOException e) {
      ret = false;                         
    }                           
    Tk.dbg("Image Parsed In " + (System.currentTimeMillis() - tm) + " ms");
    
    return ret;
  }

  private static final int OUTCODELENGTH = 1025;


  private void parseImage(
    InputStream is, int width, int height, boolean interlace, int initCodeSize,
    byte block[], int rasline[]) throws IOException {

    /* Patrick Naughton:
     * Note that I ignore the possible existence of a local color map.
     * I'm told there aren't many files around that use them, and the
     * spec says it's defined for future use.  This could lead to an
     * error reading some files.
     *
     * Start reading the image data. First we get the intial code size
     * and compute decompressor constant values, based on this code
     * size.
     *
     * The GIF spec has it that the code size is the code size used to
     * compute the above values is the code size given in the file,
     * but the code size used in compression/decompression is the code
     * size given in the file plus one. (thus the ++).
     *
     * Arthur van Hoff:
     * The following narly code reads LZW compressed data blocks and
     * dumps it into the image data. The input stream is broken up into
     * blocks of 1-255 characters, each preceded by a length byte.
     * 3-12 bit codes are read from these blocks. The codes correspond to
     * entry is the hashtable (the prefix, suffix stuff), and the appropriate
     * pixels are written to the image.
     */

    int clearCode = (1 << initCodeSize);
    int eofCode = clearCode + 1;
    int bitMask;
    int curCode;
    int outCount;

    /* Variables used to form reading data */
    boolean blockEnd = false;
    int remain = 0;
    int byteoff = 0;
    int accumbits = 0;
    int accumdata = 0;

    /* Variables used to decompress the data */
    int codeSize = initCodeSize + 1;
    int maxCode = 1 << codeSize;
    int codeMask = maxCode - 1;
    int freeCode = clearCode + 2;
    int code = 0;
    int oldCode = 0;
    int prevChar = 0;
    

    /* Temproray storage for decompression */
    int[] prefixAndSuffix = new int[4096];
    int[] outCode = new int[OUTCODELENGTH];
    int tmp;

    int blockLength = 0;

    /* Variables used for writing pixels */
    int x = width;
    int y = 0;
    int off = 0;
    int yShift = 8;
    int yInit = 8;
    int len;

    bitMask = num_colors - 1;
    
    /* Read codes until the eofCode is encountered */
    for (;;) {
      if (accumbits < codeSize) {
        /* fill the buffer if needed */
        remain -= 2;
        while (remain < 0 && !blockEnd) {
          /* move remaining bytes to the beginning of the buffer */
          block[0] = block[byteoff];
          byteoff = 0;

          try {
            readBytes(is, block, remain + 2, blockLength + 1);
          }
          catch (EOFException e) {
            sendPixels(y, off, rasline, 0);
            throw e; 
          }  

          remain += blockLength;
          blockLength = 0xFF & block[remain + 2];
          if (blockLength == 0) {
            blockEnd = true;
          }
        }

        /* 2 bytes at a time saves checking for accumbits < codeSize.
         * We know we'll get enough and also that we can't overflow
         * since codeSize <= 12.
         */
        accumdata += (block[byteoff++] & 0xff) << accumbits;
        accumbits += 8;
        accumdata += (block[byteoff++] & 0xff) << accumbits;
        accumbits += 8;
      }

      /* Compute the code */
      code = accumdata & codeMask;
      accumdata >>= codeSize;
      accumbits -= codeSize;

      /*
       * Interpret the code
       */
      if (code == clearCode) {
        /* Clear code sets everything back to its initial value, then
         * reads the immediately subsequent code as uncompressed data.
         */
        /* Note that freeCode is one less than it is supposed to be,
         * this is because it will be incremented next time round the loop
         */
        freeCode = clearCode + 1;
        codeSize = initCodeSize + 1;
        maxCode = 1 << codeSize;
        codeMask = maxCode - 1;

        /* Continue if we've NOT reached the end, some Gif images
         * contain bogus codes after the last clear code.
         */
        if (y < height) {
          continue;
        }

        /* pretend we've reached the end of the data */
        code = eofCode;
      }

      if (code == eofCode) {
        /* make sure we read the whole block of pixels. */
        if (!blockEnd) {
          is.read();
        }
        return;
      } 

      /* It must be data: save code in CurCode */
      curCode = code;
      outCount = OUTCODELENGTH;

      /* If greater or equal to freeCode, not in the hash table
       * yet; repeat the last character decoded
       */
      if (curCode >= freeCode) {
        curCode = oldCode;
        outCode[--outCount] = pixelMap[prevChar];
      }

      /* Unless this code is raw data, pursue the chain pointed
       * to by curCode through the hash table to its end; each
       * code in the chain puts its associated output code on
       * the output queue.
       */
       while (curCode > bitMask) {
         tmp = prefixAndSuffix[curCode];
         outCode[--outCount] = pixelMap[0xFF & tmp];
         curCode = tmp >>> 16;
       }

      /* The last code in the chain is treated as raw data. */
      prevChar = 0xFF & curCode;
      rasline[off++] = pixelMap[prevChar];

      /* Now we put the data out to the Output routine. It's
       * been stacked LIFO, so deal with it that way...
       */
      len = OUTCODELENGTH - outCount;
      
      int rest = width - off;
      if (len < rest) {
        System.arraycopy(outCode, outCount, rasline, off, len);
        off += len;
      }
      else {
        if (rest > 0) {
          System.arraycopy(outCode, outCount, rasline, off, rest);
          outCount += rest;
          len -= rest;
        }  
        sendPixels(y, width, rasline, 0);
        for (;;) {
          if (interlace) {
            y += yShift;
            if (y >= height) {
              yShift = yInit;
              yInit >>= 1;
              y = yInit;
              // Some files overrun the end 
              if (yInit == 0) { return; }
            }
          } 
          else {
            y++;
            // Some files overrun the end 
            if (y >= height) { return; }
          }
          
          if (len < width) {
            break;
          }

          sendPixels(y, width, outCode, outCount);
          outCount += width;
          len -= width;
        }
        
        if (len > 0) {
          System.arraycopy(outCode, outCount, rasline, 0, len);
        }
        
        off = len;
      }

      /* Build the hash table on-the-fly. No table is stored in the file. */
      prefixAndSuffix[freeCode] = (oldCode << 16) | prevChar;
      oldCode = code;

      /* Point to the next slot in the table.  If we exceed the
       * maxCode, increment the code size unless
       * it's already 12.  If it is, do nothing: the next code
       * decompressed better be CLEAR
       */
      if (++freeCode >= maxCode) {
        if (codeSize < 12) {
          codeSize++;
          maxCode <<= 1;
          codeMask = maxCode - 1;
        }
      }
    }
  }

}
