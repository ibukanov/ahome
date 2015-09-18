package lib.x;

class asm {

  static int matchBestIndex(int red, int green, int blue, 
                            int i, int[] rgbData) {
    int c, tmp, best = 0, dist2 = 196608;
    while (i > 0) {
      c = rgbData[--i];
      tmp = (tmp = (c >> 16) - red) * tmp
            + (tmp = ((c >> 8) & 0xFF) - green) * tmp 
            + (tmp = (c & 0xFF) - blue) * tmp;
      if (tmp == 0) { return i; }
      if (tmp < dist2) { 
        dist2 = tmp;
        best = i;
      }
    }
    return best;
  }                          

}

