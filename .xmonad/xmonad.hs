-- IMPORTS

------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
import System.Environment
import System.Exit
import System.IO

import qualified Data.ByteString as B
import           Data.Char
import           Data.List
import qualified Data.Map        as M
import           Data.Maybe      (isJust)
import           Data.Monoid

import           Control.Arrow (first)
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
import           XMonad.Prompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell (shellPrompt)
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig                (additionalKeysP, additionalMouseBindings)
import           XMonad.Util.Loggers
import           XMonad.Util.Run                     (runInTerm, safeSpawn, spawnPipe, unsafeSpawn)
import           XMonad.Util.SpawnOnce

------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------

myFont :: [Char]
myFont = "xft:MononokiNerdFont:sizeregular:pixelsize=16"

myModMask :: KeyMask
myModMask       = mod4Mask  -- Sets modkey to super/windows key

myTerminal :: [Char]
myTerminal      = "termite" -- Sets default terminal

myTextEditor :: [Char]
myTextEditor    = "kate"   -- Sets default text editor

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: [Char]
myNormColor   = "#4c566a"  -- Border color of normal windows

myFocusColor :: [Char]
myFocusColor  = "#5e81ac"  -- Border color of focused windows

fore :: [Char]
fore     = "#DEE3E0"

back :: [Char]
back     = "#282c34"

winType :: [Char]
winType  = "#c678dd"

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts


workDir :: [Char]
workDir         = "$WORK_DIR" -- os.getenv("WORK_DIR")

shellCmd :: [Char]
shellCmd        = myTerminal ++ " --title='OneTimeConsole' --directory " ++ workDir

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

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
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 30
    , gs_cellwidth    = 240
    , gs_cellpadding  = 8
    , gs_originFractY = 0.7
    , gs_font         = myFont
    }

spawnGridConfig = defaultGSConfig {
                gs_font         = myFont
                , gs_cellheight   = 40
                , gs_cellwidth  = 240
            }
            
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect spawnGridConfig lst >>= flip whenJust spawn

myPowerGrid = [
                 ("Abmelden", "sh ./Scripts/session_logout.sh")
                 , ("Light Theme", "sh ./Scripts/lighttheme.sh")
                 , ("Sperren", "sh ./Scripts/session_lock.sh")
                 , ("Bildschirm sperren", "sh ./Scripts/session_lock_screen.sh")
                 , ("Benutzerwechsel", "sh ./Scripts/session_switch_user.sh")
                 , ("Bereitschaft", "sh ./Scripts/session_suspend.sh")
                 , ("Hibernate", "sh ./Scripts/session_hibernate.sh")
                 , ("Neustart", "sh ./Scripts/session_reboot.sh")
                 , ("Runterfahren", "sh ./Scripts/session_shutdown.sh")
             ]

myApplicationGrid = [
                        ("JetBrains Toolsbox", "jetbrains-toolbox")
                        , (gitChar ++ "SmartGit", "/opt/smartgit/bin/smartgit.sh")
                        , ("Krusader", "krusader")
                        , ("\x00e745 Firefox", "firefox")
                        , ("Teams", "teams")
                        , ("Emoji Test", shellCmd ++ " --hold -e 'curl https://unicode.org/Public/emoji/5.0/emoji-test.txt'")
                        , ("UTF8 Test", shellCmd ++ " --hold -e 'curl https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html'")
                    ]

gitChar     = "\x00e725"                    
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

        -- Prompts
        , ("M-C-<Return>", shellPrompt dtXPConfig)   -- Shell Prompt
        , ("M-S-o", xmonadPrompt dtXPConfig)         -- Xmonad Prompt
        , ("M-S-s", sshPrompt dtXPConfig)            -- Ssh Prompt
        , ("M-S-m", manPrompt dtXPConfig)            -- Manpage Prompt
    
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
        , ("M-M1-d", spawn "firefox www.youtube.com/c/DistroTube/")

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
-- XPROMPT KEYMAP (emacs-like key bindings)
------------------------------------------------------------------------
dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
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
     , (xK_bracketleft, quit)
     ] 
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
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
     , (xK_Escape, quit)
     ]

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
dtXPConfig :: XPConfig
dtXPConfig = def
      { font                  = "xft:Mononoki Nerd Font:size=9"
      , bgColor             = "#292d3e"
      , fgColor             = "#d0d0d0"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 1
      , promptKeymap        = dtXPKeymap
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000    -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = isPrefixOf
      , alwaysHighlight     = True
      , maxComplRows        = Nothing        -- set to Just 5 for 5 rows
      }
                
------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool.

tagDev      = "\x00e753" -- "\x1F170 Dev"
tagDevCon   = "\x00e23c" -- "\x1F5A5 DevCon"
tagGit      = gitChar -- "\x1F9E0 Git"
tagTeams    = "\x00e70f" -- "\x1F4E8 Teams"
tagVM       = "\x00e62b" -- "\x1F4FA VM"
tagWeb      = "\x00f484" -- "\x1F30D Web"
tagMedia    = "\x00f447" -- "\x1F4FD Media"
tagAdmin    = "\x00e615" -- "\x02699 Admin"
tagStatus   = "\x00f626" -- "\x1F4C8 Status"
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
            "Microsoft Teams-Benachrichtigung",
            "Downloads", 
            "Save As...",
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
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
                 myDefaultLayout = tall ||| grid ||| threeCol ||| threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats

tall     = renamed [Replace "tall"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
grid     = renamed [Replace "grid"]     $ limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol = renamed [Replace "threeCol"] $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2) 
threeRow = renamed [Replace "threeRow"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig   = renamed [Replace "oneBig"]   $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle  = renamed [Replace "monocle"]  $ limitWindows 20 $ Full
space    = renamed [Replace "space"]    $ limitWindows 4  $ spacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats   = renamed [Replace "floats"]   $ limitWindows 20 $ simplestFloat

------------------------------------------------------------------------
---MAIN
------------------------------------------------------------------------
myBaseConfig = desktopConfig

main :: IO ()
main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad . ewmh $
        myBaseConfig
            { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
            , modMask            = myModMask
            , terminal           = myTerminal
            , startupHook        = myStartupHook
            , layoutHook         = myLayoutHook
            , workspaces         = myWorkspaces
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myNormColor
            , focusedBorderColor = myFocusColor
            , handleEventHook    = handleEventHook myBaseConfig <+> fullscreenEventHook
            , focusFollowsMouse  = False
            , clickJustFocuses   = True
            -- , keys               = myKeys
            }  `additionalKeysP` myKeys
