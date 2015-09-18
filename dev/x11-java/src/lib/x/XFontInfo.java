package lib.x;

class XFontProp
{
	public int nameAtomId;
	public int value;

	XFontProp(XInput input)
	{
		nameAtomId = input.readInt();
		value      = input.readInt();
	}
}

public final class XFontInfo {

	public int fid;
	public boolean drawLeftToRight;
	public int minCharOrByte2;
	public int maxCharOrByte2;
	public int minByte1;
	public int maxByte1;
	public boolean allCharsExist;
	public int defaultChar;
	public XCharStruct minBounds;
	public XCharStruct maxBounds;
	public int ascent;
	public int descent;
	public XFontProp[] properties;
	public XCharStruct[] charsInfo;

	public int charsInRowCount;
	public boolean fixedFont;
	public int maxWidth;
	public int defaultCharWidth;
	public int[] widthsFor256;


	XFontInfo(int fid, byte[] reply, XInput input) 
	{
		int i;
		this.fid = fid;
		minBounds = new XCharStruct(reply);
		maxBounds = new XCharStruct(reply, input);
		input.skip(X.INT_SIZE);
		minCharOrByte2  = input.readUnsignedShort();
		maxCharOrByte2  = input.readUnsignedShort();
		defaultChar     = input.readUnsignedShort();
		properties      = new XFontProp[input.readUnsignedShort()];
		drawLeftToRight = (0 == input.readUnsignedByte());
		minByte1        = input.readUnsignedByte();
		maxByte1        = input.readUnsignedByte();
		allCharsExist   = input.readBoolean();
		ascent          = input.readShort();
		descent         = input.readShort();
		charsInfo       = new XCharStruct[input.readInt()];
		for (i = 0; i < properties.length; ++i) {
			properties[i] = new XFontProp(input);
		}
		for (i = 0; i < charsInfo.length; ++i) {
			charsInfo[i] = new XCharStruct(input);
		}

// Currently more than one row in 2 bytes X fonts are NOT VISIBLE...
//  twoByteFont = (minByte1 != 0) && (maxByte1 != 0);

		charsInRowCount = maxCharOrByte2 - minCharOrByte2 + 1;
		fixedFont = (charsInfo.length == 0);
		maxWidth = maxBounds.width;
		defaultCharWidth = 0;
		int index = defaultChar - minCharOrByte2;
		if (index >= 0 && index < charsInRowCount) {
			defaultCharWidth = fixedFont ? maxWidth : charsInfo[index].width;
		}

		if (!fixedFont) {
			widthsFor256 = new int[256];
			int last = minCharOrByte2 > 256 ? 256 : minCharOrByte2;
			for (i = 0; i < last; ++i) { widthsFor256[i] = defaultCharWidth; }
			last = maxCharOrByte2 > 255 ? 255 : maxCharOrByte2;
			index = 0;
			for (; i <= last; ++i) { widthsFor256[i] = charsInfo[index++].width; }
			for (; i < 256; ++i) { widthsFor256[i] = defaultCharWidth; }
		}
	}

	public int bytesWidth(byte[] arr, int off, int len) 
	{
		byte c;
		int w = 0;
		if (fixedFont) {
			if (defaultCharWidth != 0) { return maxWidth * len; }
			for (; off < len; ++off) {
				c = arr[off];
				if (minCharOrByte2 <= c && c <= maxCharOrByte2) {
					w += maxWidth;
				}
			}

		}
		else {
			for (; off < len; ++off) {
				w += widthsFor256[arr[off]];
			}
		}
		return w;
	}

	public int charWidth(char c) 
	{
		if (0 <= (c -= minCharOrByte2) && c < charsInRowCount) {
				if (fixedFont) {
						return maxWidth;
				}
				else {
						return charsInfo[c].width;
				}
		}
		return defaultCharWidth;
	}

	public int charsWidth(char[] arr, int off, int len) 
	{
		char c;
		int w = 0;
		if (fixedFont) {
			if (defaultCharWidth != 0) { return maxWidth * len; }
			for (; off < len; ++off) {
				c = arr[off];
				if (minCharOrByte2 <= c && c <= maxCharOrByte2) {
					w += maxWidth;
				}
			}

		}
		else {
			for (; off < len; ++off) {
				c = arr[off];
				if (c < 256) {
					w += widthsFor256[c];
				}
				else {
					w += (0 <= (c -= minCharOrByte2) && c < charsInRowCount)
							 ? charsInfo[c].width : defaultCharWidth;
				}
			}
		}
		return w;
	}

	public int stringWidth(String s) 
	{
		char c;
		int i = s.length();
		int w = 0;
		if (fixedFont) {
			if (defaultCharWidth != 0) { return maxWidth * i; }
			while (i > 0) {
				--i;
				c = s.charAt(i);
				if (minCharOrByte2 <= c && c <= maxCharOrByte2) {
					w += maxWidth;
				}
			}

		}
		else {
			while (i > 0) {
				--i;
				c = s.charAt(i);
				if (c < 256) {
					w += widthsFor256[c];
				}
				else {
					w += (0 <= (c -= minCharOrByte2) && c < charsInRowCount)
							 ? charsInfo[c].width : defaultCharWidth;
				}
			}
		}
		return w;
	}

}

