import XMonad

-- Actions
import XMonad.Actions.Submap
import XMonad.Actions.Search
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
-- Prompts
import XMonad.Prompt.Man
import XMonad.Prompt.OrgMode
import XMonad.Prompt.RunOrRaise

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

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { font = "xft:terminus:pixelsize:14"
                             , height = 35

										   
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	[ ((modm, xK_p), visualSubmap . M.fromList $
      [ ((0, "Man Pages", xK_m),  manPrompt def)
	  , ((0, "Org Prompt", xK_o), orgPrompt def "TODO" "~/org/inbox.org")
	  , ((0, "Run", xK_r),        runOrRaisePrompt def)
	  , ((0, "Search", xK_s),     visualSubmap . M.fromList $
	                              [ ((0, "Google", xK_g),         promptSearchBrowser myXPConfig myBrowser google)
	  							  , ((0, "Google Images", xK_i),  promptSearchBrowser myXPConfig myBrowser images)
	  							  , ((0, "Dictionary", xK_d),     promptSearchBrowser myXPConfig myBrowser dictionary)
	  							  , ((0, "Thesaurus", xK_t),      promptSearchBrowser myXPConfig myBrowser thesaurus)
								  , ((0, "YouTube", xK_y),        promptSearchBrowser myXPConfig myBrowser youtube)
	  							  , ((0, "Wikipedia", xK_w),      promptSearchBrowser myXPConfig myBrowser wikipedia)
								  , ((0, "Amazon", xK_a),         promptSearchBrowser myXPConfig myBrowser amazon)
	  							  , ((0, "Google Scholar", xK_s), promptSearchBrowser myXPConfig myBrowser scholar)
	  							  ])
	  ])
    ]

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
