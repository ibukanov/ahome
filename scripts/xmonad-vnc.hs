import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myModMask = mod3Mask

myStartup = do
          spawn "u-fix-vnc-xmodmap"

main = do
    xmonad $ defaultConfig
        { layoutHook = simpleTabbed ||| layoutHook defaultConfig
        , terminal = "u-term"
        , startupHook = myStartup
        , modMask = myModMask
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
