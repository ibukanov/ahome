package lib.x;

public class Display extends KeyboardSubsystem {
  public Display() {
    super(null);
  }
  public Display(String displayName) {
    super(displayName);
  }
  public Display(String[] args) {
    this(displayNameFromArgs(args));
  }
}
