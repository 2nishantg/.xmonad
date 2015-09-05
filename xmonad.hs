--import XMonad.Layout.LayoutHints
import Data.Ratio ((%))
import System.Exit
import XMonad hiding ((|||))
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Actions.Search
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
import XMonad.Prompt.XMonad
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet as W
import qualified Data.Map as M



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
        , ((mod4Mask .|. shiftMask, xK_p)
          , shellPrompt
                greenXPConfig
                { font = "xft:PragmataPro:size=9"
                })
        , ((mod4Mask .|. shiftMask, xK_q), kill) -- %! Close the focused window
        , ((mod4Mask, xK_s), SM.submap $ searchEngineMap $ promptSearchBrowser
                             greenXPConfig {
                               font = "xft:PragmataPro:size=9"
                               } "firefox")
        , ((mod4Mask .|. shiftMask, xK_s), selectSearchBrowser "firefox" google ) 
        , ((mod4Mask .|. controlMask, xK_l  ), spawn "xscreensaver-command -lock")
        , ((mod4Mask .|. shiftMask, xK_c), io exitSuccess)
        , ((mod4Mask , xK_m),   nextWS)
        , ((mod4Mask .|. shiftMask, xK_m), shiftToNext >> nextWS)
        , ((mod4Mask, xK_n),   prevWS)
        , ((mod4Mask .|. shiftMask, xK_n), shiftToPrev >> prevWS)
        , ((mod4Mask, xK_z),     toggleWS)
        , ((mod4Mask .|. controlMask, xK_x), xmonadPrompt myXPConfig)
        , ((mod4Mask, xK_d), runOrRaisePrompt myXPConfig)
        , ((mod4Mask, xK_f), sendMessage $ ToggleStruts)
--        , ((mod4Mask, xK_u), appendFilePrompt myXPConfig "/home/nis/NOTES")
--        , ((mod4Mask, xK_i), appendFilePrompt myXPConfig "/home/nis/IDEAS")
        , ((mod4Mask, xK_g), gotoMenu)
         ] `additionalMouseBindings`
        [ ((mod4Mask, button4), (\_ -> nextWS))
        , ((mod4Mask, button5), (\_ -> prevWS))]
  where
    myStartupHook = spawn "feh --randomize --bg-scale /home/nis/.xmonad/wallpaper/* "
                <+> spawn "udiskie --tray"
                <+> spawn "nm-applet"
                <+> spawn "emacs -daemon"
                <+> spawn "urxvtd -q -f"
                <+> spawn "xscreensaver -nosplash"
                <+> setWMName "LG3D">> takeTopFocus
                <+> setDefaultCursor xC_left_ptr
    myTerminal = "urxvtc"
    myModMask = mod4Mask
    myWorkspaces    = ["T","E","F","U","M","B","A","P","R"]
    scratchpad = scratchpadSpawnActionTerminal "urxvt"
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
    searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google )
       , ((0, xK_y), method S.youtube )
       , ((0, xK_m), method S.maps )
       , ((0, xK_d), method S.dictionary )
       , ((0, xK_w), method S.wikipedia )
       , ((0, xK_a), method S.amazon )
       ]
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

