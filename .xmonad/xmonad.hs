-- Quelle:
-- The xmonad configuration of Derek Taylor (DistroTube)
-- My YouTube: http://www.youtube.com/c/DistroTube
-- My GitLab:  http://www.gitlab.com/dwt1/
-- For more information on Xmonad, visit: https://xmonad.org

------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Custom
import           System.Exit
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog)
import           XMonad.Hooks.UrgencyHook
import           XMonad.Actions.CycleWS (prevWS, nextWS, swapNextScreen)
import           XMonad.Actions.RotSlaves
import           XMonad.Layout.Reflect
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.OneBig
import           Control.Arrow (first)
import           Control.Monad (liftM2)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D


------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:pixelsize=13"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "firefox"       -- Sets default browser

myTextEditor :: String
myTextEditor = "code"       -- Sets default text editor

myFileManager :: String
myFileManager = "thunar"    -- Sets default text editor
--myFileManager = "pcmanfm"   -- Sets default text editor

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: String
myNormColor = "#4c566a"     -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#5e81ac"    -- Border color of focused windows

myGaps :: Int
myGaps = 5                  -- Sets layout gaps and window spacing

myLargeSpacing :: Int
myLargeSpacing = 30

-- Colours
fg = "#DEE3E0"
bg = "#282c34"
gray = "#a89984"
bg1 = "#3c3836"
bg2 = "#505050"
bg3 = "#665c54"
bg4 = "#7c6f64"

green = "#b8bb26"
darkgreen = "#98971a"
red = "#fb4934"
darkred = "#cc241d"
yellow = "#fabd2f"
blue = "#83a598"
purple = "#d3869b"
aqua = "#8ec07c"
white = "#eeeeee"
pur2 = "#5b51c9"
blue2 = "#2266d0"

fore :: String
fore = fg

back :: String
back = bg

winType :: String
winType = "#c678dd"

altMask :: KeyMask
altMask = mod1Mask   -- Setting this for use in xprompts

workDir :: String
workDir = "$WORK_DIR"

shellCmd :: String
shellCmd = myTerminal ++ " -t 'OneTimeConsole' --working-directory " ++ workDir

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawn "sh $HOME/Scripts/autostart.sh"
  setWMName "LG3D"

------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x29,0x2d,0x3e) -- lowest inactive bg
                  (0x29,0x2d,0x3e) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x29,0x2d,0x3e) -- active fg
                  
-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer =
  (buildDefaultGSConfig
     myColorizer) { gs_cellheight = 30
                  , gs_cellwidth = 240
                  , gs_cellpadding = 8
                  , gs_originFractY = 0.7
                  , gs_font = myFont
                  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where
    conf = def { gs_font = myFont, gs_cellheight = 40, gs_cellwidth = 240 }

------------------------------------------------------------------------
---MOUSE BINDINGS
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))

    ]

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
myKeys =
  -- Xmonad
  [
  ("M-c", spawn $ "conky-toggle" )
  , ("M-r", spawn $ "rofi-theme-selector" )
  , ("M-t", spawn $ myTerminal )
  , ("M-v", spawn $ "pavucontrol" )
  , ("M-y", spawn $ "polybar-msg cmd toggle" )
  , ("M-x", spawn $ "arcolinux-logout" )
  , ("M-<Escape>", spawn $ "xkill" )
  , ("M-<Return>", spawn $ myTerminal)
  , ("M-F1", spawn $ myBrowser )
  , ("M-F7", spawn $ "virtualbox" )
  , ("M-F8", spawn $ myFileManager )

  -- SUPER + SHIFT KEYS
  , ("M-S-<Return>", spawn $ myFileManager)
  , ("M-S-r", spawn $ "xmonad --recompile; xmonad --restart")
  , ("M-C-q", io (exitWith ExitSuccess))

  -- SUPER + CONTROL KEYS
  , ("M-C-t", spawn "sh ./Scripts/picom-toggle.sh")

  -- CONTROL + ALT KEYS
  , ("M1-C-Next", spawn $ "conky-rotate -n")
  , ("M1-C-Prior", spawn $ "conky-rotate -p")
  , ("M1-C-b", spawn $ myFileManager)
  , ("M1-C-c", spawn $ "catfish")
  , ("M1-C-e", spawn $ "arcolinux-tweak-tool")
  , ("M1-C-f", spawn $ myBrowser)
  , ("M1-C-g", spawn $ "chromium -no-default-browser-check")
  , ("M1-C-i", spawn $ "nitrogen")
  , ("M1-C-k", spawn $ "arcolinux-logout")
  , ("M1-C-l", spawn $ "arcolinux-logout")
  , ("M1-C-m", spawn $ "xfce4-settings-manager")
  , ("M1-C-p", spawn $ "pamac-manager")
  , ("M1-C-r", spawn $ "rofi-theme-selector")
  , ("M1-C-t", spawn $ myTerminal)
  , ("M1-C-u", spawn $ "pavucontrol")
  , ("M1-C-w", spawn $ "arcolinux-welcome-app")
  , ("M1-C-<Return>", spawn $ myTerminal)

    -- ALT + ... KEYS
  , ("M1-f", spawn $ "variety -f" )
  , ("M1-n", spawn $ "variety -n" )
  , ("M1-p", spawn $ "variety -p" )
  , ("M1-r", spawn $ "xmonad --restart" )
  , ("M1-t", spawn $ "variety -t" )
  , ("M1-<Up>", spawn $ "variety --pause" )
  , ("M1-<Down>", spawn $ "variety --resume" )
  , ("M1-<Left>", spawn $ "variety -p" )
  , ("M1-<Right>", spawn $ "variety -n" )

  --VARIETY KEYS WITH PYWAL
  , ("M1-S-f", spawn $ "variety -f && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ("M1-S-n", spawn $ "variety -n && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ("M1-S-p", spawn $ "variety -p && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ("M1-S-t", spawn $ "variety -t && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ("M1-S-u", spawn $ "wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")

  --CONTROL + SHIFT KEYS
  , ("C-S-<Escape>", spawn $ "xfce4-taskmanager")

  --SCREENSHOTS
  , ("<Print>", spawn $ "spectacle")
  , ("C-<Print>", spawn $ "spectacle" )
  , ("C-S-<Print>", spawn $ "spectacle")

  --MULTIMEDIA KEYS

  -- Mute volume
  , ("<XF86XK_AudioMute>", spawn $ "amixer -q set Master toggle")

  -- Decrease volume
  , ("<XF86XK_AudioLowerVolume>", spawn $ "amixer -q set Master 5%-")

  -- Increase volume
  , ("<XF86XK_AudioRaiseVolume>", spawn $ "amixer -q set Master 5%+")

  -- Increase brightness
  , ("<XF86XK_MonBrightnessUp>",  spawn $ "xbacklight -inc 5")

  -- Decrease brightness
  , ("<XF86XK_MonBrightnessDown>", spawn $ "xbacklight -dec 5")

      -- Multimedia Keys
--  , ("<XF86AudioPlay>", spawn "mpc toggle")
--  , ("<XF86AudioNext>", spawn "mpc next")
--  , ("<XF86AudioPrev>", spawn "mpc prev")
--  , ("<XF86AudioStop>", spawn "mpc stop")

  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("<XF86AudioNext>", spawn "playerctl next")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioStop>", spawn "playerctl stop")

  , ("<XF86HomePage>", spawn myBrowser)
  , ("<XF86Search>", safeSpawn myBrowser ["https://www.google.com/"])
  , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
  , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
  , ("<XF86Eject>", spawn "toggleeject")

  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ("M-<Space>", sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ("M-S-<Space>", sendMessage FirstLayout)

  -- Swap the focused window with the next window.
  , ("M-C-<Down>", windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ("M-C-<Up>", windows W.swapUp  )

    -- Windows
  , ("M-q", kill1)                                    -- Kill the currently focused client
  , ("M-S-a", killAll)                                -- Kill all the windows on current workspace

      -- Prompts
  , ("M-S-m", manPrompt dtXPConfig)                   -- Manpage Prompt

    -- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)    -- Push floating window back to tile.
  , ("M-S-<Delete>", sinkAll)                         -- Push ALL floating windows back to tile.
  , ("M-S-f", sendMessage (T.Toggle "floats"))        -- Toggles my 'floats' layout

    -- Grid Select
      -- Goto running
  , ("M-S-g", goToSelected $ mygridConfig myColorizer)
      -- Pull running
  , ("M-S-b", bringSelected $ mygridConfig myColorizer)

    -- Workspaces navigation
  , ("M-,", nextScreen)            -- View next screen
  , ("M-.", swapNextScreen)        -- Swap current screen with next screen

  , ("M-S-<Right>", nextWS)        -- Swap the focused window with the next window
  , ("M-S-<Left>", prevWS)         -- Swap the focused window with the prev window
  , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next workspace
  , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to previous workspace

    -- Scratchpads
  , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-k", namedScratchpadAction myScratchPads "krusader")
  , ("M-C-s", namedScratchpadAction myScratchPads "spotify")
  , ("M-C-m", namedScratchpadAction myScratchPads "matrix")

    -- Windows navigation
  , ("M-<Right>", windows W.focusDown)        -- Swap the focused window with the next window
  , ("M-<Left>", windows W.focusUp)           -- Swap the focused window with the prev window
  , ("M-m", windows W.focusMaster)            -- Move focus to the master window
  , ("M-<Backspace>", promote)                -- Moves focused window to master, all others maintain order
  , ("M-S-<Return>", promote)                 -- Moves focused window to master, all others maintain order
  , ("M-<Down>", rotSlavesDown)               -- Rotate all windows except master and keep focus in place
  , ("M-<Up>", rotSlavesUp)                   -- Rotate all windows except master and keep focus in place
  , ("M-S-<Down>", rotAllDown)                -- Rotate all the windows in the current stack
  , ("M-S-<Up>", rotAllUp)                    -- Rotate all the windows in the current stack

    -- Layouts
  , ("M-<Tab>", sendMessage NextLayout)                               -- Switch to next layout
  , ("M-S-<Tab>", sendMessage FirstLayout)                            -- Switch to next layout
  , ("M-S-<Space>", sendMessage ToggleStruts)                         -- Toggles struts
  , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)                         -- Toggles noborder
  , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  , ("M-S-f", sendMessage (T.Toggle "float"))
  , ("M-S-x", sendMessage $ MT.Toggle REFLECTX)
  , ("M-S-y", sendMessage $ MT.Toggle REFLECTY)
  , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
  , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
  , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
  , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

    --- Rofi Menu
  , ("M-z", spawn "rofi -show combi")
  , ("M-S-z", spawn "xfce4-appfinder")
  , ("M-w", spawn "bwmenu -- -location 2")
  , ("M-F2", spawn $ "gmrun" )

    --- Menu Scripts (Super+Alt+Key)
  , ("M-M1-a", spawn "./Scripts/menu/app-menu.sh")
  , ("M-M1-d", spawn "./Scripts/menu/develop-menu.sh")
  , ("M-d", spawn "./Scripts/menu/develop-menu.sh")
  , ("M-M1-e", spawn "./Scripts/menu/edit-configs.sh")
  , ("M-M1-m", spawn "./Scripts/menu/system-monitor.sh")
  , ("M-M1-q", spawn "./Scripts/menu/system-menu.sh")
  ]
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))

    nonEmptyNonNSP = WSIs
      (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

------------------------------------------------------------------------
-- XPROMPT KEYMAP (emacs-like key bindings)
------------------------------------------------------------------------
dtXPKeymap :: M.Map (KeyMask, KeySym) (XP ())
dtXPKeymap = M.fromList
  $ map
    (first $ (,) controlMask)       -- control + <key>
    [ (xK_z, killBefore)            -- kill line backwards
    , (xK_k, killAfter)             -- kill line fowards
    , (xK_a, startOfLine)           -- move to the beginning of the line
    , (xK_e, endOfLine)             -- move to the end of the line
    , (xK_m, deleteString Next)     -- delete a character foward
    , (xK_b, moveCursor Prev)       -- move cursor forward
    , (xK_f, moveCursor Next)       -- move cursor backward
    , (xK_BackSpace, killWord Prev) -- kill the previous word
    , (xK_y, pasteString)           -- paste a string
    , (xK_g, quit)                  -- quit out of prompt
    , (xK_bracketleft, quit)]
  ++ map
    (first $ (,) altMask)       -- meta key + <key>
    [ (xK_BackSpace, killWord Prev) -- kill the prev word
    , (xK_f, moveWord Next)         -- move a word forward
    , (xK_b, moveWord Prev)         -- move a word backward
    , (xK_d, killWord Next)         -- kill the next word
    , (xK_n, moveHistory W.focusUp')   -- move up thru history
    , (xK_p, moveHistory W.focusDown') -- move down thru history
    ]
  ++ map
    (first $ (,) 0) -- <key>
    [ (xK_Return, setSuccess True >> setDone True)
    , (xK_KP_Enter, setSuccess True >> setDone True)
    , (xK_BackSpace, deleteString Prev)
    , (xK_Delete, deleteString Next)
    , (xK_Left, moveCursor Prev)
    , (xK_Right, moveCursor Next)
    , (xK_Home, startOfLine)
    , (xK_End, endOfLine)
    , (xK_Down, moveHistory W.focusUp')
    , (xK_Up, moveHistory W.focusDown')
    , (xK_Escape, quit)]

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
dtXPConfig :: XPConfig
dtXPConfig =
  def { font = "xft:Mononoki Nerd Font:size=9"
      , bgColor = "#292d3e"
      , fgColor = "#d0d0d0"
      , bgHLight = "#c792ea"
      , fgHLight = "#000000"
      , borderColor = "#535974"
      , promptBorderWidth = 0
      , promptKeymap = dtXPKeymap
      , position = Top
        --    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height = 20
      , historySize = 256
      , historyFilter = id
      , defaultText = []
      , autoComplete = Just 100000    -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate = isPrefixOf
      , alwaysHighlight = True
      , maxComplRows = Nothing        -- set to Just 5 for 5 rows
      }

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = inputPrompt c (trim ans)
  ?+ \input -> liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where
        f = reverse . dropWhile isSpace

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool.
tagDev = "wsDev"
tagDevCon = "wsDevCon"
tagGit = "wsGit"
tagTeams = "wsTeams"
tagVM = "wsVM"
tagWeb = "wsWeb"
tagMedia = "wsMedia"
tagAdmin = "wsAdmin"
tagStatus = "wsStatus"
tagScratch = "wsScratch"

myWorkspaces :: [String]
myWorkspaces =
  [ tagDev
  , tagDevCon
  , tagGit
  , tagTeams
  , tagVM
  , tagWeb
  , tagMedia
  , tagStatus
  , tagAdmin]

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool.
-- You need the className or title of the program. Use xprop to get this info.
-- window manipulations
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat
  $ [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagDev | x <- myDevShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift tagDevCon | x <- myDevConShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift tagGit | x <- myGitShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift tagTeams | x <- myTeamsShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift tagVM | x <- myVmShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift tagWeb | x <- myWebShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift tagMedia | x <- myMediaShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagAdmin | x <- myAdminShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagStatus | x <- myStatusShifts]
    , [namedScratchpadManageHook myScratchPads]]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift

    myCFloats =
      [
        "Arandr",
        "Arandr",
        "Arcolinux-tweak-tool.py",
        "Arcolinux-welcome-app.py",
        "copyq",
        "feh",
        "Galculator",
        "imagewriter",
        "JetBrains Toolbox",
        "Libfm-pref-apps",
        "Lxappearance",
        "Nitrogen",
        "mpv",
        "Nemo-terminal-prefs",
        "org.remmina.Remmina",
        "Pavucontrol",
        "qt5ct",
        "smb4k",
        "Viewnior",
        "VirtualBox Manager",
        "Xfce4-appfinder",
        "Xfce4-taskmanager",
        "Xfce4-terminal"
        ]

    myTFloats = [
      "bmenu",
      "Downloads",
      "JetBrains Toolbox",
      "Microsoft Teams-Benachrichtigung",
      "Save As...",
      "wetter",
      "xmonad-errors",
      "xsession-errors"
      ]

    myRFloats = []

    myIgnores = ["desktop_window"]

    -- Shifts
    myDevShifts = ["JetBrains Toolbox"]
    myDevConShifts = ["Chromium", "Google-chrome", "OneTimeConsole"]
    myGitShifts = ["SmartGit"]
    myTeamsShifts = ["Microsoft Teams - Preview"]
    myVmShifts = ["Virtualbox", "VirtualBox Manager", "org.remmina.Remmina"]
    myWebShifts = ["firefox", "Vivaldi-stable", "Firefox"]
    myMediaShifts = ["vlc", "mpv", "Gimp", "feh", "Inkscape"]
    myAdminShifts = []

    myStatusShifts =
      [ "s-tui"
      , "bashtop"
      , "glances"
      , "gtop"
      , "htop"
      , "iftop"
      , "iotop"
      , "iptraf-ng"]

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

noSpacing = mySpacing 0

smallSpacing = mySpacing 3

defaultSpacing = mySpacing 5

largeSpacing = mySpacing 8

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []

magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ limitWindows 20
           $ Full

floats   = renamed [Replace "floats"]
           $ limitWindows 20
           $ simplestFloat

grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)

spirals  = renamed [Replace "spirals"]
           $ mySpacing' 8
           $ spiral (6/7)

threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)

threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)

tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }

mosaic   = renamed [Replace "mosaic"]
           $ limitWindows 12
           $ mySpacing' 4
           $ MosaicAlt M.empty

oneBig   = renamed [Replace "oneBig"] 
           $ limitWindows 20 
           $ mySpacing' 4
           $ OneBig 0.75 0.65

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats 
               $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
    myDefaultLayout =  tall
      ||| oneBig
      ||| magnify
      ||| smartBorders mosaic
      ||| smartBorders grid
      ||| noBorders monocle
      ||| noBorders tabs
      ||| smartBorders spirals
      -- ||| smartBorders threeCol
      -- ||| smartBorders threeRow
      -- ||| floats

-----------------------------------------------------------------------------
-- LOGHOOK
-----------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus =
  def { ppOutput = dbusOutput dbus
      , ppCurrent = wrap ("C:%{F" ++ blue2 ++ "} ") " %{F-}"
      , ppVisible = wrap ("V:%{F" ++ blue ++ "} ") " %{F-}"
      , ppUrgent = wrap ("U:%{F" ++ red ++ "} ") " %{F-}"
      , ppHidden = wrap " " " "
      , ppWsSep = ""
      , ppSep = " | "
      , ppTitle = myAddSpaces 25
      }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal
           objectPath
           interfaceName
           memberName) { 
                          D.signalBody = [D.toVariant $ UTF8.decodeString str]
                       }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "krusader" spawnKrus findKrus manageKrus
                , NS "spotify" spawnSpoty findSpoty manageSpoty
                , NS "matrix" spawnMatrix findMatrix manageMatrix
                ]
  where
    spawnTerm = myTerminal ++ " --class scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.9
        t = 0.03
        l = (1 - w) / 2

    spawnKrus = "krusader"
    findKrus = resource =? "krusader"
    manageKrus = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.9
        t = 0.2
        l = (1 - w) / 2

    spawnSpoty = "spotify"
    findSpoty = className =? "Spotify"
    manageSpoty = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.9
        t = 0.03
        l = (1 - w) / 2

    spawnMatrix = myTerminal ++ " --hold -t matrix -e cmatrix -B -C blue"
    findMatrix = title =? "matrix"
    manageMatrix = customFloating $ W.RationalRect l t w h
      where
        h = 1
        w = 1
        t = 0
        l = 0

mySpotifyHook :: Event -> X All
mySpotifyHook =
  dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
  where
    floating = customFloating $ W.RationalRect l t w h
      where
        h = 0.8
        w = 0.9
        t = 0.03
        l = (1 - w) / 2

------------------------------------------------------------------------
---MAIN
------------------------------------------------------------------------
myBaseConfig = desktopConfig

main :: IO ()
main = do

  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ myBaseConfig { manageHook = (isFullscreen --> doFullFloat)
                       <+> myManageHook
                       <+> manageDocks
                   , handleEventHook = handleEventHook myBaseConfig
                       <+> fullscreenEventHook
                       <+> mySpotifyHook
                       <+> docksEventHook
                   , modMask = myModMask
                   , terminal = myTerminal
                   , startupHook = myStartupHook
                   , layoutHook = myLayoutHook
                   , workspaces = myWorkspaces
                   , borderWidth = myBorderWidth
                   , normalBorderColor = myNormColor
                   , focusedBorderColor = myFocusColor
                   , focusFollowsMouse = False
                   , clickJustFocuses = True
                   , mouseBindings = myMouseBindings
                   , logHook = dynamicLogWithPP (myLogHook dbus)
                   }
        -- , keys               = myKeys
    `additionalKeysP` myKeys
