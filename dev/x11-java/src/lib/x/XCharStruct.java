package lib.x;

public class XCharStruct 
{
	public int lbearing; 
	public int rbearing; 
	public int width;    
	public int ascent;   
	public int descent;  
	
// Very unfortunately I have to write this 3 constructors...
	XCharStruct(byte[] reply) 
	{  
		// for minBounds
		lbearing = Tk.getShort(reply, 8 + 0);
		rbearing = Tk.getShort(reply, 8 + 2); 
		width    = Tk.getShort(reply, 8 + 4);    
		ascent   = Tk.getShort(reply, 8 + 6);   
		descent  = Tk.getShort(reply, 8 + 8); 
	}
	
	XCharStruct(byte[] reply, XInput input) 
	{
		// for maxBounds
		lbearing = Tk.getShort(reply, 24 + 0);
		rbearing = Tk.getShort(reply, 24 + 2); 
		width    = Tk.getShort(reply, 24 + 4);    
		ascent   = Tk.getShort(reply, 24 + 6);   
		descent  = input.readShort();
		input.skip(X.SHORT_SIZE); // atttributes
	}

	XCharStruct(XInput input) 
	{
		// for per char data
		lbearing = input.readShort();
		rbearing = input.readShort(); 
		width    = input.readShort();    
		ascent   = input.readShort();   
		descent  = input.readShort();
		input.skip(X.SHORT_SIZE); // atttributes
	}
}

