import XMonad

-- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
-- Layouts
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat
import XMonad.Layout.GridVariants
import XMonad.Layout.Tabbed
-- Layout Modifiers
import XMonad.Layout.LimitWindows
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders

-- Variable definitions

myWorkspaces = ["dev", "www", "sys", "vid", "doc"]

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBorderWidth :: Dimension
myBorderWidth = 2

-- Startup Hook
myStartupHook :: X()
myStartupHook = do
  spawnOnce "~/.fehbg &"
  spawnOnce "picom -b --config ~/.config/picom/picom.conf"
  spawn "emacs --daemon"

-- Layout Hook

-- Add spacing between
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- layouts
tall = renamed [Replace "tall"]
       $ mySpacing 8
       $ ResizableTall 1 (3/100) (1/2) []

monocle = renamed [Replace "monocle"]
          $ mySpacing 2
          $ Full

floats = renamed [Replace "floats"]
         $ simplestFloat
grid = renamed [Replace "grid"]
       $ limitWindows 9
       $ windowNavigation
       $ mySpacing 8
       $ Grid (16/10)

-- tabs = renamed [Replace "tabbed"]
   --       $ Tabbed

myLayoutHook = avoidStruts $ myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                                           ||| noBorders monocle
                                           ||| floats
                                           ||| grid
                                           ||| simpleTabbed

-- colors
colorBack = "#3D3F42"
colorFore = "#EBD999"

color01 = "#92A0A1" --
color02 = "#818F75"
color03 = "#8db95c"-- "#9DCE66"
color04 = "#c2975a"
color05 = "#72BBDB" -- dirty powder blue

main :: IO ()
main = do
  xmproc <- spawnPipe ("xmobar -d ~/.xmobarrc")
  xmonad $ ewmhFullscreen $ ewmh $ xmobarProp $ def
    { modMask     = myModMask
    , layoutHook  = myLayoutHook
    , manageHook  = manageDocks <+> manageHook def
    , workspaces  = myWorkspaces
    , startupHook = myStartupHook
    , normalBorderColor = colorBack
    , focusedBorderColor = colorFore
    , logHook     = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor color05 "" . wrap
                    "<fn=2>[" "]</fn>"
                   -- ("<box type=Bottom width=2 mb=2 color=" ++ color05 ++ ">") "</box>"
      , ppHidden = xmobarColor color03 "" . wrap
                   ("<box type=Bottom width=1 mb=1 color=" ++ color03 ++ ">") "</box>"
      , ppHiddenNoWindows = xmobarColor color02 ""
      , ppLayout = xmobarColor color05 ""
      , ppTitle = xmobarColor colorFore "" . shorten 60
      , ppSep = "<fc=" ++ colorFore ++ "> | </fc>"
      , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      }
    }
