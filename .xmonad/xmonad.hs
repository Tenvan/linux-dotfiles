-- IMPORTS

------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
    -- Base
import           Data.Char
import           Data.Maybe            (isJust)
import qualified Data.Map              as M
import           Data.Monoid
import           System.Exit           (exitSuccess)
import           System.IO             (hPutStrLn)
import           XMonad
import           XMonad.Config.Desktop
import qualified XMonad.StackSet       as W

    -- Utilities
import XMonad.Util.EZConfig        (additionalKeysP, additionalMouseBindings)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run             (runInTerm, safeSpawn, spawnPipe, unsafeSpawn)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog    (PP (..), defaultPP, dynamicLogWithPP, pad, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks   (ToggleStruts (..), avoidStruts, docks, docksStartupHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.Place         (placeHook, smart, withGaps)
import XMonad.Hooks.SetWMName

    -- Actions
import qualified XMonad.Actions.ConstrainedResize as Sqr
import           XMonad.Actions.CopyWindow        (copyToAll, kill1, killAllOtherCopies, runOrCopy)
import           XMonad.Actions.CycleWS           (WSType (..), moveTo, nextScreen, prevScreen, shiftNextScreen,
                                                   shiftPrevScreen, shiftTo)
import           XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Minimize          (minimizeWindow)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves         (rotAllDown, rotSlavesDown)
import           XMonad.Actions.WindowGo          (raiseMaybe, runOrRaise)
import           XMonad.Actions.WithAll           (killAll, sinkAll)

    -- Layouts modifiers
import           XMonad.Layout.LimitWindows          (decreaseLimit, increaseLimit, limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT), Toggle (..), mkToggle, single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace          (onWorkspace)
import           XMonad.Layout.Reflect               (REFLECTX (..), REFLECTY (..), reflectHoriz, reflectVert)
import           XMonad.Layout.Renamed               (Rename (CutWordsLeft, Replace), renamed)
import           XMonad.Layout.Spacing               (spacing)
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle), toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..), windowArrange)
import           XMonad.Layout.WorkspaceDir

    -- Layouts
import XMonad.Layout.GridVariants  (Grid (Grid))
import XMonad.Layout.IM            (Property (Role), withIM)
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ZoomRow       (ZoomMessage (ZoomFullToggle), zoomIn, zoomOut, zoomReset, zoomRow)

    -- Prompts
import XMonad.Prompt (Direction1D (..), XPConfig (..), XPPosition (Top), defaultXPConfig)

-- rest
import System.Environment

------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------
myFont          = "xft:Mononoki Nerd Font:regular:pixelsize=12"
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "alacritty"      -- Sets default terminal
myTextEditor    = "nvim"     -- Sets default text editor
-- Width of the window border in pixels.
--
myBorderWidth   = 5         -- Sets border width for windows
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

workDir         = "/media/WORKSPACE/Node/OneTime" -- os.getenv("WORK_DIR")
shellCmd        = myTerminal ++ " --title='OneTimeConsole' --working-directory " ++ workDir

------------------------------------------------------------------------
---MAIN
------------------------------------------------------------------------
main = do
  -- Launching three instances of xmobar on their monitors.
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc1"
  xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc2"

  xmonad $ ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x
            , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
            , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
            , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
            , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
            , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
            , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
            , ppExtras  = [windowCount]                           -- # of windows current workspace
            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
            }
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = "#292d3e"
        , focusedBorderColor = "#bbc5ff"
        , focusFollowsMouse  = False
        , clickJustFocuses   = True
        } `additionalKeysP` myKeys

------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom --config ~/.config/compton-awesome.conf &"
  spawnOnce "copyq &"
  spawnOnce "nm-applet &"
  spawnOnce "volumeicon &"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --iconspacing 4 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x292d3e --height 18 &"
  -- setWMName "LG3D"

------------------------------------------------------------------------
---GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

-- gridSelect menu layout
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

myExitGrid = [
                         ("Sperren", "sh ./Scripts/session_lock.sh")
                         , ("Bildschirm sperren", "./Scripts/session_lock_screen.sh")
                         , ("Abmelden", "./Scripts/session_logout.sh")
                         , ("Benutzerwechsel", "./Scripts/session_switch_user.sh")
                         , ("Bereitschaft", "./Scripts/session_suspend.sh")
                         , ("Hibernate", "./Scripts/session_hibernate.sh")
                         , ("Neustart", "./Scripts/session_reboot.sh")
                         , ("Runterfahren", "./Scripts/session_shutdown.sh")
                     ]

------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", spawnSelected' myExitGrid)                  -- Quits xmonad
        , ("M-C-q", io exitSuccess)                  -- Quits xmonad

    -- Windows
        , ("M-S-c", kill1)                           -- Kill the currently focused client
        , ("M-S-a", killAll)                         -- Kill all the windows on current workspace

    -- Floating windows
        , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
        , ("M-S-<Delete>", sinkAll)                  -- Push ALL floating windows back to tile.

    -- Grid Select
        -- Applications
        , ("M-S-t", spawnSelected' [
            ("Krusader", "krusader")
            , ("Audacity", "audacity")
            , ("Deadbeef", "deadbeef")
            , ("Firefox", "firefox")
            , ("Geany", "geany")
            , ("Geary", "geary")
            , ("Gimp", "gimp")
            , ("LibreOffice Impress", "loimpress")
            , ("LibreOffice Writer", "lowriter")
            , ("OBS", "obs")
            , ("Xonotic", "xonotic-glx")
        ])

        -- Develop Processes
        , ("M-d", spawnSelected'
            [("Shell",  shellCmd ++ " --hold")
            , ("yarn", shellCmd ++ " --hold -e yarn")
            , ("Generate", shellCmd ++ " --hold -e yarn generate")
            , ("Check updates", shellCmd ++ " --hold -e yarn outdated")
            , ("Start Server", shellCmd ++ " --hold -e yarn server:dev")
            , ("Pug watch", shellCmd ++ "/src/client --hold -e yarn pug:watch")
            , ("\x00e753 Start", shellCmd ++ "/src/client --hold -e yarn start")
            , ("\x00e753 Start hmr", shellCmd ++ "/src/client --hold -e yarn start:client:hmr --port 4201")
            , ("\x00e753 Start AOT", shellCmd ++ "/src/client --hold -e yarn start:client:dev --aot --port 4202")
            , ("Yarn install", shellCmd ++ " --hold -e yarn install --ignore-scripts")
            , ("Yarn update", shellCmd ++ " --hold -e yarn run update:all")
            ])

        , ("M-S-g", goToSelected $ mygridConfig myColorizer)
        , ("M-S-b", bringSelected $ mygridConfig myColorizer)

    -- Windows navigation
        , ("M-<Right>", windows W.focusDown)         -- Swap the focused window with the next window
        , ("M-<Left>", windows W.focusUp)            -- Swap the focused window with the prev window
        , ("M-<Down>", windows W.swapMaster)         -- Swap the focused window and the master window

        , ("M-m", windows W.focusMaster)             -- Move focus to the master window
        , ("M-<Backspace>", promote)                 -- Moves focused window to master, all others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
        , ("M-S-s", windows copyToAll)
        , ("M-C-s", killAllOtherCopies)

        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Up>", sendMessage (MoveUp 10))             --  Move focused window to up
        , ("M-<Down>", sendMessage (MoveDown 10))         --  Move focused window to down
        , ("M-S-<Up>", sendMessage (IncreaseUp 10))       --  Increase size of focused window up
        , ("M-S-<Down>", sendMessage (IncreaseDown 10))   --  Increase size of focused window down
        , ("M-S-<Right>", sendMessage (IncreaseRight 10)) --  Increase size of focused window right
        , ("M-S-<Left>", sendMessage (IncreaseLeft 10))   --  Increase size of focused window left
        , ("M-C-<Up>", sendMessage (DecreaseUp 10))       --  Decrease size of focused window up
        , ("M-C-<Down>", sendMessage (DecreaseDown 10))   --  Decrease size of focused window down
        , ("M-C-<Right>", sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
        , ("M-C-<Left>", sendMessage (DecreaseLeft 10))   --  Decrease size of focused window left

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                              -- Switch to next layout

        , ("M-S-<Space>", sendMessage ToggleStruts)                          -- Toggles struts
        , ("M-S-n", sendMessage $ Toggle NOBORDERS)                          -- Toggles noborder
        , ("M-f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-f", sendMessage (T.Toggle "float"))
        , ("M-S-x", sendMessage $ Toggle REFLECTX)
        , ("M-S-y", sendMessage $ Toggle REFLECTY)
        , ("M-S-m", sendMessage $ Toggle MIRROR)
        , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
        , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-C-j", sendMessage MirrorShrink)
        , ("M-C-k", sendMessage MirrorExpand)
        , ("M-S-ö", sendMessage zoomReset)
        , ("M-ö", sendMessage ZoomFullToggle)

    -- Workspaces
        , ("M-.", nextScreen)                           -- Switch focus to next monitor
        , ("M-,", prevScreen)                           -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next workspace
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to previous workspace

    -- Scratchpads
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
        , ("M-C-c", namedScratchpadAction myScratchPads "cmus")

    -- Open My Preferred Terminal. I also run the FISH shell. Setting FISH as my default shell
    -- breaks some things so I prefer to just launch "fish" when I open a terminal.
        , ("M-<Return>", spawn myTerminal)

    --- Dmenu Scripts (Alt+Ctr+Key)
        , ("M-z", spawn "rofi -show combi")

    --- My Applications (Super+Alt+Key)
        , ("M-M1-a", spawn (myTerminal ++ " -e ncpamixer"))
        , ("M-M1-b", spawn "surf www.youtube.com/c/DistroTube/")
        , ("M-M1-c", spawn (myTerminal ++ " -e cmus"))
        , ("M-M1-e", spawn (myTerminal ++ " -e neomutt"))
        , ("M-M1-f", spawn (myTerminal ++ " -e sh ./.config/vifm/scripts/vifmrun"))
        , ("M-M1-i", spawn (myTerminal ++ " -e irssi"))
        , ("M-M1-j", spawn (myTerminal ++ " -e joplin"))
        , ("M-M1-l", spawn (myTerminal ++ " -e lynx -cfg=~/.lynx/lynx.cfg -lss=~/.lynx/lynx.lss gopher://distro.tube"))
        , ("M-M1-m", spawn (myTerminal ++ " -e toot curses"))
        , ("M-M1-n", spawn (myTerminal ++ " -e newsboat"))
        , ("M-M1-p", spawn (myTerminal ++ " -e pianobar"))
        , ("M-M1-r", spawn (myTerminal ++ " -e rtv"))
        , ("M-M1-w", spawn (myTerminal ++ " -e wopr report.xml"))
        , ("M-M1-y", spawn (myTerminal ++ " -e youtube-viewer"))

    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn "cmus toggle")
        , ("<XF86AudioPrev>", spawn "cmus prev")
        , ("<XF86AudioNext>", spawn "cmus next")
        -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
        , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool.

xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ [
               "1:\x00e753 Dev",
               "2:\x00e795 DevCon",
               "3:\x00f0c3 Scratch",
               "4:\x00f0c0 Teams",
               "5:\x00e62b VM",
               "6:\x00e745 Web",
               "7:\x00f001 Media",
               "8:\x00e20f Admin",
               "9:\x00f080 Status"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool.
-- You need the className or title of the program. Use xprop to get this info.

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [  className =? "firefox"     --> doShift "<action=xdotool key super+2>www</action>"
      , className =? "Opera"     --> doShift "<action=xdotool key super+2>www</action>"
      , title =? "qutebrowser"     --> doShift "<action=xdotool key super+2>www</action>"
      , title =? "irssi"           --> doShift "<action=xdotool key super+6>chat</action>"
      , className =? "mpv"         --> doShift "<action=xdotool key super+8>vid</action>"
      , className =? "vlc"         --> doShift "<action=xdotool key super+8>vid</action>"
      , className =? "VirtualBox Manager"  --> doFloat
      , className =? "VirtualBox Manager"  --> doShift "<action=xdotool key super+5>vbox</action>"
      , className =? "Gimp"        --> doFloat
      , className =? "Gimp"        --> doShift "<action=xdotool key super+9>gfx</action>"
      , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
                 myDefaultLayout = tall ||| grid ||| threeCol ||| threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats


tall       = renamed [Replace "tall"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
grid       = renamed [Replace "grid"]     $ limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol   = renamed [Replace "threeCol"] $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2)
threeRow   = renamed [Replace "threeRow"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig     = renamed [Replace "oneBig"]   $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle    = renamed [Replace "monocle"]  $ limitWindows 20 Full
space      = renamed [Replace "space"]    $ limitWindows 4  $ spacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats     = renamed [Replace "floats"]   $ limitWindows 20 simplestFloat

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
myScratchPads = [
                    NS "terminal" spawnTerm findTerm manageTerm
                ]
    where
    spawnTerm  = myTerminal ++  " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
