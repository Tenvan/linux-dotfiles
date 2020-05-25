-- IMPORTS

------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
import System.Environment
import System.Exit
import System.IO

import qualified Data.ByteString as B
import           Data.Char
import qualified Data.Map        as M
import           Data.Maybe      (isJust)
import           Data.Monoid

import           Control.Monad (liftM2)
import qualified DBus          as D
import qualified DBus.Client   as D

import Graphics.X11.ExtraTypes.XF86

import           XMonad
import qualified XMonad.Actions.ConstrainedResize    as Sqr
import           XMonad.Actions.CopyWindow           (copyToAll, kill1, killAllOtherCopies, runOrCopy)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces    (addWorkspacePrompt, removeEmptyWorkspace)
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Minimize             (minimizeWindow)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves            (rotAllDown, rotSlavesDown)
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.WindowGo             (raiseMaybe, runOrRaise)
import           XMonad.Actions.WithAll              (killAll, sinkAll)
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog             (PP (..), defaultPP, dynamicLogWithPP, pad, shorten, wrap, xmobarColor, xmobarPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks            (ToggleStruts (..), avoidStruts, docks, docksStartupHook, manageDocks)
import           XMonad.Hooks.ManageHelpers          (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import           XMonad.Hooks.Place                  (placeHook, smart, withGaps)
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Cross                 (simpleCross)
import           XMonad.Layout.Fullscreen            (fullscreenFull)
import           XMonad.Layout.Gaps
import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.IM                    (Property (Role), withIM)
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LimitWindows          (decreaseLimit, increaseLimit, limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT), Toggle (..), mkToggle, single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace          (onWorkspace)
import           XMonad.Layout.Reflect               (REFLECTX (..), REFLECTY (..), reflectHoriz, reflectVert)
import           XMonad.Layout.Renamed               (Rename (CutWordsLeft, Replace), renamed)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle), toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..), windowArrange)
import           XMonad.Layout.WorkspaceDir
import           XMonad.Layout.ZoomRow               (ZoomMessage (ZoomFullToggle), zoomIn, zoomOut, zoomReset, zoomRow)
import           XMonad.Prompt                       (Direction1D (..), XPConfig (..), XPPosition (Top), defaultXPConfig)
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig                (additionalKeysP, additionalMouseBindings)
import           XMonad.Util.Loggers
import           XMonad.Util.Run                     (runInTerm, safeSpawn, spawnPipe, unsafeSpawn)
import           XMonad.Util.SpawnOnce

------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------

-- colours
normBord = "#4c566a"
focdBord = "#5e81ac"
fore     = "#DEE3E0"
back     = "#282c34"
winType  = "#c678dd"

myFont          = "xft:MononokiNerdFont:sizeregular:pixelsize=16"
--myFont          = "xft:NotoSans:size=15"
--myFont            = "xft:NotoEmoji:scale=9"

-- xft:NotoSans:size=15,xft:NotoEmoji:scale=9

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "termite" -- Sets default terminal
myTextEditor    = "kate"   -- Sets default text editor
-- Width of the window border in pixels.
--

myBorderWidth   = 5         -- Sets border width for windows
myGapsWidth     = 5         -- Sets border width for windows

workDir         = "$WORK_DIR" -- os.getenv("WORK_DIR")
shellCmd        = myTerminal ++ " --title='OneTimeConsole' --directory " ++ workDir

------------------------------------------------------------------------
---MAIN
------------------------------------------------------------------------
myBaseConfig = desktopConfig

main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad . ewmh $
        myBaseConfig
            {
            startupHook        = myStartupHook
            , layoutHook         = gaps [(U,(30 + myGapsWidth)), (D,myGapsWidth), (R,myGapsWidth), (L,myGapsWidth)] $ archoLayout ||| layoutHook myBaseConfig
            , manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
            , modMask            = myModMask
            , borderWidth        = myBorderWidth
            , handleEventHook    = handleEventHook myBaseConfig <+> fullscreenEventHook
            , focusFollowsMouse  = False
            , clickJustFocuses   = True
            , workspaces         = myWorkspaces
            , focusedBorderColor = focdBord
            , normalBorderColor  = normBord
            -- , keys               = myKeys
            , terminal           = myTerminal
            }  `additionalKeysP` myKeys

------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
  spawn "sh $HOME/Scripts/autostart.sh"
  setWMName "LG3D"

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

spawnGridConfig = defaultGSConfig {
                gs_font         = myFont
            }
            
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect spawnGridConfig lst >>= flip whenJust spawn

myPowerGrid = [
                 ("Sperren", "sh ./Scripts/session_lock.sh")
                 , ("Bildschirm sperren", "./Scripts/session_lock_screen.sh")
                 , ("Abmelden", "./Scripts/session_logout.sh")
                 , ("Benutzerwechsel", "./Scripts/session_switch_user.sh")
                 , ("Bereitschaft", "./Scripts/session_suspend.sh")
                 , ("Hibernate", "./Scripts/session_hibernate.sh")
                 , ("Neustart", "./Scripts/session_reboot.sh")
                 , ("Runterfahren", "./Scripts/session_shutdown.sh")
             ]

myApplicationGrid = [
                        ("JetBrains Toolsbox", "jetbrains-toolbox")
                        , ("SmartGit", "/opt/smartgit/bin/smartgit.sh")
                        , ("Krusader", "krusader")
                        , ("\x00e745 Firefox", "firefox")
                        , ("Teams", "teams")
                    ]

yarnChar    = "\x00e71a"
angularChar = "\x00e753"
tsChar      = "\x00fbe4"
                    
myDevelopGrid = [
                    ("\x00e795 Shell",  shellCmd ++ " --hold")
                    , (yarnChar ++ " yarn", shellCmd ++ " --hold -e yarn")
                    , (yarnChar ++ " Generate", shellCmd ++ " --hold -e yarn generate")
                    , (yarnChar ++ " Check updates", shellCmd ++ " --hold -e 'yarn outdated'")
                    , ("\x00f1c0 Start Server", shellCmd ++ " --hold -e 'yarn server:dev'")
                    , ("\x00e718 Pug watch", shellCmd ++ "/src/client --hold -e 'yarn pug:watch'")
                    , (angularChar ++ " Start", shellCmd ++ "/src/client --hold -e 'yarn start'")
                    , (angularChar ++ " Start hmr", shellCmd ++ "/src/client --hold -e 'yarn start:client:hmr --port 4201'")
                    , (angularChar ++ " Start AOT", shellCmd ++ "/src/client --hold -e 'yarn start:client:dev --aot --port 4202'")
                    , (yarnChar ++ " Yarn install", shellCmd ++ " --hold -e 'yarn install --ignore-scripts'")
                    , (yarnChar ++ " Yarn update", shellCmd ++ " --hold -e 'yarn run update:all'")
                ]

------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys =
    -- Xmonad
        [ ("M-q", spawn "xmonad --recompile; xmonad --restart;")    -- Recompiles and Restarts xmonad
        , ("M-C-r", spawn "xmonad --recompile")                     -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart;")                      -- Restarts xmonad
        , ("M-S-q", spawnSelected' myPowerGrid)                     -- Quits xmonad
        , ("M-C-x", spawn "xkill")
        , ("M-C-c", spawn (myTextEditor ++ " $HOME/.xmonad/xmonad.hs"))

    -- System
        , ("C-<Escape>", spawn "xfce4-taskmanager")
        , ("M-C-t", spawn "sh ./Scripts/picom-toggle.sh")

    -- Windows
        , ("M-S-c", kill1)                                  -- Kill the currently focused client
        , ("M-S-a", killAll)                                -- Kill all the windows on current workspace

    -- Floating windows
        , ("M-<Delete>", withFocused $ windows . W.sink)    -- Push floating window back to tile.
        , ("M-S-<Delete>", sinkAll)                         -- Push ALL floating windows back to tile.

    -- Grid Select
        -- Applications
        , ("M-S-t", spawnSelected' myApplicationGrid )

        -- Develop Processes
        , ("M-d", spawnSelected' myDevelopGrid)

        -- Goto running
        , ("M-S-g", goToSelected $ mygridConfig myColorizer)
        -- Pull running
        , ("M-S-b", bringSelected $ mygridConfig myColorizer)

    -- Windows navigation
        , ("M-<Right>", windows W.focusDown)        -- Swap the focused window with the next window
        , ("M-<Left>", windows W.focusUp)           -- Swap the focused window with the prev window

        , ("M-S-<Right>", nextWS)        -- Swap the focused window with the next window
        , ("M-S-<Left>", prevWS)           -- Swap the focused window with the prev window

        , ("M-m", windows W.focusMaster)            -- Move focus to the master window

        , ("M-<Backspace>", promote)                -- Moves focused window to master, all others maintain order
        , ("M-S-<Return>", promote)                 -- Moves focused window to master, all others maintain order

        , ("M-S-<Down>", rotAllDown)                -- Rotate all the windows in the current stack
        , ("M-<Down>", rotSlavesDown)               -- Rotate all windows except master and keep focus in place

        , ("M-S-s", windows copyToAll)
        , ("M-C-s", killAllOtherCopies)

        , ("M-C-<Up>", sendMessage Arrange)
        , ("M-C-<Down>", sendMessage DeArrange)

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

        , ("M-<Return>", spawn myTerminal)

    --- Rofi Menu
        , ("M-z", spawn "rofi -show combi")

    --- My Applications (Super+Alt+Key)
        , ("M-M1-a", spawn (myTerminal ++ " -e ncpamixer"))
        , ("M-M1-b", spawn "surf www.youtube.com/c/DistroTube/")

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

tagDev      = "\x1F170" -- "\x1F170 Dev"
tagDevCon   = "\x1F5A5" -- "\x1F5A5 DevCon"
tagGit      = "\x1F9E0" -- "\x1F9E0 Git"
tagTeams    = "\x1F4E8" -- "\x1F4E8 Teams"
tagVM       = "\x1F4FA" -- "\x1F4FA VM"
tagWeb      = "\x1F30D" -- "\x1F30D Web"
tagMedia    = "\x1F3B5" -- "\x1F4FD Media"
tagAdmin    = "\x02699" -- "\x02699 Admin"
tagStatus   = "\x1F4C8" -- "\x1F4C8 Status"
tagScratch  = "\x1F4DD" -- "\x1F4DD Scratch"

myWorkspaces :: [String]
myWorkspaces =  [
                   tagDev,
                   tagDevCon,
                   tagGit,
                   tagTeams,
                   tagVM,
                   tagWeb,
                   tagMedia,
                   tagStatus,
                   tagAdmin
                 ]

arcWorkspaces    = ["\61612","\61899","\61947","\61635","\61502","\61501","\61705","\61564","\62150","\61872"]


------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool.
-- You need the className or title of the program. Use xprop to get this info.

-- window manipulations
-- myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagDev | x <- myDevShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagDevCon | x <- myDevConShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagGit | x <- myGitShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagTeams | x <- myTeamsShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagVM | x <- myVmShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagWeb | x <- myWebShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagMedia | x <- myMediaShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagAdmin | x <- myAdminShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagStatus | x <- myStatusShifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo tagScratch | x <- myScratchShifts]
    ]
    where
        doShiftAndGo    = doF . liftM2 (.) W.greedyView W.shift
        myCFloats       = [
            "JetBrains Toolbox",
            "Xfce4-taskmanager",
            "Viewnior",
            "Libfm-pref-apps",
            "Arandr",
            "Galculator",
            "feh",
            "mpv",
            "VirtualBox Manager",
            "Pavucontrol"
            ]
        myTFloats       = [
            "JetBrains Toolbox",
            "Downloads", "Save As...",
            -- XFCE Panels
            "Wrapper-2.0", "Xfce4-panel", "Xfwm4-workspace-settings", "Xfce4-panel-profiles.py"
            ]
        myRFloats       = []
        myIgnores       = ["desktop_window"]
        -- Shifts
        myDevShifts     = ["JetBrains Toolbox"]
        myDevConShifts  = []
        myGitShifts     = ["SmartGit"]
        myTeamsShifts   = ["Microsoft Teams - Preview"]
        myVmShifts      = ["Virtualbox", "VirtualBox Manager"]
        myWebShifts     = ["firefox", "Chromium", "Vivaldi-stable", "Firefox"]
        myMediaShifts   = ["vlc", "mpv", "Gimp", "feh", "Inkscape"]
        myAdminShifts   = []
        myStatusShifts  = []
        myScratchShifts = []

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
archoLayout = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| spiral (6/7)  ||| ThreeColMid 1 (3/100) (1/2) ||| Full
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
                 myDefaultLayout = tall ||| grid ||| threeCol ||| threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats

-- tiled       = Tall nmaster delta tiled_ratio
tall       = renamed [Replace "tall"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
grid       = renamed [Replace "grid"]     $ limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol   = renamed [Replace "threeCol"] $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2)
threeRow   = renamed [Replace "threeRow"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig     = renamed [Replace "oneBig"]   $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle    = renamed [Replace "monocle"]  $ limitWindows 20 Full
space      = renamed [Replace "space"]    $ limitWindows 4  $ spacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats     = renamed [Replace "floats"]   $ limitWindows 20 simplestFloat
