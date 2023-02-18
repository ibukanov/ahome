import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig (additionalKeys)
 
main =
   xmonad $ gnomeConfig {
     -- add manage hooks while still ignoring panels and using default manageHooks
     --  manageHook = myManageHook <+> manageHook desktopConfig ,
 
     -- add a fullscreen tabbed layout that does not avoid covering
     -- up desktop panels before the desktop layouts
     layoutHook = simpleTabbed ||| layoutHook gnomeConfig ,
     terminal = "u-term",
     modMask = mod4Mask,
     borderWidth = 0
     }
     -- add a screenshot key to the default desktop bindings
     `additionalKeys` [ 
         ((mod4Mask .|. shiftMask , xK_q), spawn "gnome-session-quit --logout"), 
         ((mod4Mask .|. shiftMask , xK_z), spawn "gnome-session-quit --power-off") 
     ]
