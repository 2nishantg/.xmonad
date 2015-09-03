--import XMonad.Layout.LayoutHints
import Data.Ratio ((%))
import System.Exit
import XMonad hiding ((|||))
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W

main :: IO ()
main = do
--  d <- spawnPipe "dzen2 -p -y 1060 -x 0 -ta l -e 'onstart=lower'"
  xmonad $ ewmh $  defaultConfig
        { modMask = myModMask
        , terminal = myTerminal
        , focusFollowsMouse = True
--        , logHook = myLogHook d
        , layoutHook = myLayoutHook
        , manageHook = manageDocks
                   <+> manageHook defaultConfig
                   <+> myManageHook
                   <+> scratchpadManageHookDefault
        , handleEventHook = XMonad.Hooks.EwmhDesktops.fullscreenEventHook
        , normalBorderColor = "#2a2b2f"
        , focusedBorderColor = "Green"
        , borderWidth = 1
        , workspaces = myWorkspaces
        , startupHook = myStartupHook
        } `additionalKeysP`
        [ ("<XF86AudioRaiseVolume>", spawn "/home/nis/.xmonad/dvol up 10")   -- volume up
        , ("<XF86AudioLowerVolume>", spawn "/home/nis/.xmonad/dvol down 10") -- volume down
        , ("<XF86AudioMute>", spawn "/home/nis/.xmonad/dvol toggle") -- mute
        , ("<XF86MonBrightnessDown>", spawn "/home/nis/.xmonad/dbright -d 5") -- decrease Brightness
        , ("<XF86MonBrightnessUp>", spawn "/home/nis/.xmonad/dbright -i 5") -- decrease Brightness
        , ("<XF86Launch1>", spawn "dmenu_run")                             -- dmenu
        , ("<XF68ScreenSaver>", spawn "xlock")                                 -- lock screen
         ] `additionalKeys`
        [ ((mod4Mask, xK_b), runOrRaise "firefox" (className =? "Firefox"))
        , ((mod4Mask, xK_e), spawn "emacsclient -c")
        , ((mod4Mask, xK_u), scratchpad)
        , ((mod4Mask, xK_y), focusUrgent)
        , ((mod4Mask .|. shiftMask, xK_space), myLayoutPrompt)
        , ((mod4Mask, xK_p), shellPrompt myXPConfig)
        , ( (mod4Mask .|. shiftMask, xK_p)
          , shellPrompt
                greenXPConfig
                { font = "xft:PragmataPro:size=9"
                })
        , ((mod4Mask .|. shiftMask, xK_q), kill) -- %! Close the focused window
        , ((mod4Mask .|. shiftMask, xK_c), io exitSuccess)
        , ((mod4Mask, xK_g), gotoMenu)]
  where
    myStartupHook = spawn "feh --bg-scale /home/nis/.xmonad/animal.jpg"
                <+> spawn "udiskie --tray"
                <+> spawn "nm-applet"
                <+> spawn "emacs -daemon"
                <+> spawn "urxvtd -q -f"
                <+> setWMName "LG3D">> takeTopFocus
                <+> setDefaultCursor xC_left_ptr
    myTerminal = "urxvtc"
    myModMask = mod4Mask
    myWorkspaces    = ["T","E","F","U","M","B","A","P","R"]
    scratchpad = scratchpadSpawnActionTerminal "urxvt"
    -- myLogHook h = dynamicLogWithPP $ defaultPP
    --     -- display current workspace as darkgrey on light grey (opposite of 
    --     -- default colors)
    --     { ppCurrent         = dzenColor "#303030" "#909090" . pad 

    --     -- display other workspaces which contain windows as a brighter grey
    --     , ppHidden          = dzenColor "#909090" "" . pad 

    --     -- display other workspaces with no windows as a normal grey
    --     , ppHiddenNoWindows = dzenColor "#606060" "" . pad 

    --     -- display the current layout as a brighter grey
    --     , ppLayout          = dzenColor "#909090" "" . pad 

    --     -- if a window on a hidden workspace needs my attention, color it so
    --     , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

    --     -- shorten if it goes over 100 characters
    --     , ppTitle           = shorten 100

    --     -- no separator between workspaces
    --     , ppWsSep           = ""

    --     -- put a few spaces between each object
    --     , ppSep             = "  "

    --     -- output to the handle we were given as an argument
    --     , ppOutput          = hPutStrLn h
    --     }
    myLayoutHook =
        avoidStrutsOn [U,L,R,D] $
        smartBorders $
        (tall ||| wide ||| full ||| circle ||| sTabbed ||| acc)
    tall = renamed [Replace "tall"] $ Tall 1 3.0e-2 0.5
    wide = renamed [Replace "wide"] $ Mirror tall
    full = renamed [Replace "full"] Full
    circle = renamed [Replace "circle"] circleSimpleDefaultResizable
    sTabbed = renamed [Replace "tabbed"]  simpleTabbed
    acc = renamed [Replace "accordion"] Accordion
    imLayout = withIM (1 % 7) pidginRoster Grid
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"
    -- layout prompt (w/ auto-completion and all layouts)
    myLayoutPrompt =
        inputPromptWithCompl
            myXPConfig
            "Layout"
            (mkComplFunFromList' allLayouts) ?+
        (sendMessage . JumpToLayout)
    myXPConfig =
        greenXPConfig
        { autoComplete = Just 1000
        , font         = myFont
        , promptBorderWidth = 0
        , position = Top
        }
        where
         myFont = "xft:PragmataPro:size=9"
    allLayouts = ["tall", "wide", "circle", "full", "tabbed", "accordion"]
    myManageHook = (composeAll . concat $
          [   [isDialog --> doFloat]
            , [className =? c --> doFloat | c <- myCFloats]
            , [title =? t --> doFloat | t <- myTFloats]
            , [resource =? i --> doIgnore | i <- myIgnores]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "M" | x <- my1Shifts]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "F" | x <- my2Shifts]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "B" | x <- my3Shifts]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "E" | x <- my4Shifts]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "U" | x <- my5Shifts]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "T" | x <- my6Shifts]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "P" | x <- my7Shifts]
            , [(className =? x <||> title =? x <||> resource =? x) --> doShift "R" | x <- my8Shifts]
         ])
         where
        myCFloats = ["mplayer","vlc","Smplayer","Xmessage", "Toplevel"]
        myTFloats = ["Save As..."]
        myIgnores = ["desktop_window","kdesktop"]
        my1Shifts = []
        my2Shifts = ["Firefox","Chrome"]
        my3Shifts = ["irssi*","irssi"]
        my4Shifts = []
        my5Shifts = ["Pidgin"]
        my6Shifts = []
        my7Shifts = []
        my8Shifts = []
        my9Shifts = []

