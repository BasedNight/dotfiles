import XMonad
import XMonad.Prelude

-- Actions
import XMonad.Actions.Submap
import XMonad.Actions.Search
import XMonad.Actions.CycleWS

-- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks

-- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed

-- Layout Modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.LimitWindows
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.OrgMode
import XMonad.Prompt.RunOrRaise

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Variable definitions

myWorkspaces = ["dev", "www", "sys", "vid", "doc"]

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

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
		  $ noBorders

floats = renamed [Replace "floats"]
         $ simplestFloat

tabbed = renamed [Replace "tabbed"]
         $ shrinkText
		 $ myTabConfig
		 $ simpleTabbed

twopane = renamed [Replace "twopane"]
          $ mySpacing 3
		  $ TwoPane (3/100) (1/2)
		  
myTabConfig = def { activeColor       = colorFore
                  , inactiveColor     = colorBack
				  , activeTextColor   = color05
				  , inactiveTextColor = color02
				  }

myLayoutHook = avoidStruts $ myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                                           ||| monocle
                                           ||| twopane
										   ||| tabbed
										   ||| floats

myXPConfig :: XPConfig
myXPConfig = def {font = "xft:terminus:pixelsize:14"
                 , height = 33
                 , position = Top}

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm standardManage
                , NS "repl-guile" spawnGuile findGuile standardManage
				, NS "repl-haskell" spawnGhci findGhci standardManage
				, NS "htop" spawnHtop findHtop standardManage
                ]
  where
    standardManage = customFloating $ W.RationalRect l t w h
                   where
				   h = 0.9
				   w = 0.9
				   t = 0.95 -h
				   l = 0.95 -w
	
	spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"

    spawnGuile = myTerminal ++ " -t guile-repl -e guix repl"
    findGuile = title =? "guile-repl"

	spawnGhci = myTerminal ++ " -t haskell-repl -e ghci"
	findGhci = title =? "haskell-repl"
	
	spawnHtop = myTerminal ++ " -t htop -e htop"
	findHtop = title =? "htop"


-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys =
    [ ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
      , ((myModMask, xK_s), submap . M.fromList $
      [
        ((0, xK_t), namedScratchpadAction myScratchPads "terminal")
        , ((0, xK_g), namedScratchpadAction myScratchPads "repl-guile")
		, ((0, xK_h), namedScratchpadAction myScratchPads "repl-haskell")
		, ((0, xK_p), namedScratchpadAction myScratchPads "htop")
      ])
      , ((myModMask, xK_p), submap . M.fromList $
      [ ((0, xK_m),  manPrompt myXPConfig)
          , ((0, xK_o),  orgPrompt myXPConfig "TODO" "~/org/inbox.org")
          , ((0, xK_r),  runOrRaisePrompt myXPConfig)
          , ((0, xK_s),  submap . M.fromList $
                               [ ((0, xK_g),  promptSearchBrowser myXPConfig myBrowser google)
                               , ((0, xK_i),  promptSearchBrowser myXPConfig myBrowser images)
                               , ((0, xK_d),  promptSearchBrowser myXPConfig myBrowser dictionary)
                               , ((0, xK_t),  promptSearchBrowser myXPConfig myBrowser thesaurus)
                               , ((0, xK_y),  promptSearchBrowser myXPConfig myBrowser youtube)
                               , ((0, xK_w),  promptSearchBrowser myXPConfig myBrowser wikipedia)
                               , ((0, xK_a),  promptSearchBrowser myXPConfig myBrowser amazon)
                               , ((0, xK_s),  promptSearchBrowser myXPConfig myBrowser scholar)
                               ])
      ])
    ]
      where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
            nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))


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
    , manageHook  = manageDocks <+> manageHook def <+> namedScratchpadManageHook myScratchPads
    , workspaces  = myWorkspaces
    , startupHook = myStartupHook
    , normalBorderColor = colorBack
    , focusedBorderColor = colorFore
    , logHook     = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor color05 "" . wrap
                    "<fn=2>[" "]</fn>"
                   -- ("<box type=Bottom width=2 mb=2 color=" ++ color05 ++ ">") "</box>"
      , ppHidden = xmobarColor color03 "" . filterNSP . wrap 
                   ("<box type=Bottom width=1 mb=1 color=" ++ color03 ++ ">") "</box>"
      , ppHiddenNoWindows = xmobarColor color02 "" . filterNSP
      , ppLayout = xmobarColor color05 ""
      , ppTitle = xmobarColor colorFore "" . shorten 60
      , ppSep = "<fc=" ++ colorFore ++ "> | </fc>"
      , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      } where
	      filterNSP ws = if ws /= "NSP" then ws else ""
    } `additionalKeys` myKeys
