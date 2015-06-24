--
-- File     : ~/.xmonad/xmonad.hs
-- Author   : Yiannis Tsiouris (yiannist)
-- Desc     : A clean and well-documented xmonad configuration file (based on
--            the $HOME/.cabal/share/xmonad-0.10.1/man/xmonad.hs template file).
--
--            It uses:
--              * a ScratchPad (for a hidden terminal),
--              * an IM layout for Pidgin,
--              * a layout prompt (with auto-complete).
--
import XMonad hiding ((|||))
import XMonad.Hooks.ManageDocks
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
import XMonad.Actions.WindowBringer
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import System.Exit
import Data.Ratio ((%))

main :: IO ()
main =
    xmonad $ ewmh myConfig
    where
    myConfig  = defaultConfig {
    modMask            = myModMask
  , terminal           = myTerminal
  , focusFollowsMouse  = True
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook <+> manageHook defaultConfig <+> manageDocks
  , normalBorderColor  = "#2a2b2f"
  , focusedBorderColor = "Green"
  , borderWidth        = 1
  , workspaces         = myWorkSpaces
--  , startupHook        = spawn "taffybar"
  } `additionalKeysP`
        [ ("<XF86AudioRaiseVolume>", spawn "/home/nis/bin/volume_level.sh up")   -- volume up
        , ("<XF86AudioLowerVolume>", spawn "/home/nis/bin/volume_level.sh down") -- volume down
        , ("<XF86AudioMute>"       , spawn "/home/nis/bin/volume_level.sh mute") -- mute
        , ("<XF86Launch1>"         , spawn "dmenu_run")                             -- dmenu
        , ("<XF68ScreenSaver>"     , spawn "xlock")                                 -- lock screen
        ] `additionalKeys`
        [ ((mod4Mask, xK_b), spawn "google-chrome-beta")
        , ((mod4Mask, xK_r), spawn "emacs")
        , ((mod4Mask, xK_u), scratchpad)
        , ((mod4Mask, xK_y), focusUrgent)
        , ((mod4Mask .|. controlMask, xK_space), myLayoutPrompt)
        , ((mod4Mask, xK_p), shellPrompt myXPConfig)
        , ((mod4Mask .|. shiftMask, xK_q     ), kill) -- %! Close the focused window
        , ((mod4Mask .|. shiftMask, xK_c     ), io exitSuccess)
        , ((mod4Mask, xK_g     ), gotoMenu)
        ]
    myTerminal = "urxvt"
    myModMask = mod4Mask
    myWorkSpaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι"]
    scratchpad = scratchpadSpawnActionTerminal "urxvt"
--    scratchpad = scratchpadSpawnActionCustom "xterm -e ps aux | percol | awk '{print $2}' | xargs kill $*  -name scratchpad"

       -- layouts
    myLayoutHook =
        avoidStrutsOn [D] $
        smartBorders $
        onWorkspace "8" imLayout $
        tall ||| wide ||| full ||| circle ||| sTabbed ||| acc
    tall = renamed [Replace "tall"] $ Tall 1 3.0e-2 0.5
    wide = renamed [Replace "wide"] $ Mirror tall
    full = renamed [Replace "full"] $ Full
    circle = renamed [Replace "circle"] $ circleSimpleDefaultResizable
    sTabbed = renamed [Replace "tabbed"] $ simpleTabbed
    acc = renamed [Replace "accordion"] $ Accordion
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
        -- , bgColor     = backgroundColor
        -- , fgColor     = textColor
        -- , fgHLight    = lightTextColor
        -- , bgHLight    = lightBackgroundColor
        -- , borderColor = lightBackgroundColor
        , promptBorderWidth = 0
        , position = Top
        }
        where
         myFont = "xft:Input:size=9"
         -- focusColor = "#535d6c"
         -- textColor = "#ff4500"
         -- lightTextColor = "#222222"
         -- backgroundColor = "#222222"
         -- lightBackgroundColor = "#ff4500"
         -- urgentColor = "#ffc000"
    allLayouts = ["tall", "wide", "circle", "full", "tabbed", "accordion"]
   -- manageHook
    myManageHook =
       manageDocks <+>
       floatHook <+> fullscreenManageHook <+> scratchpadManageHookDefault
   -- Inspect with xprop: appName is the first element in WM_CLASS, while
   -- className is the second.
    floatHook =
       composeAll
           [ appName =? "gimp-2.8" --> doFloat
           , className =? "wpa_gui" --> doFloat
           , appName =? "google-chrome-beta" --> doF (W.shift "3")
           , appName =? "xfrun4" --> doFloat]
