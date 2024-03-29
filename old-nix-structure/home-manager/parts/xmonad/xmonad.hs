    -- Base
import XMonad
  ( xmonad
  , X
  , KeyMask
  , Dimension
  , KeySym
  , Query
  , WindowSet
  , manageHook
  , handleEventHook
  , modMask
  , terminal
  , startupHook
  , layoutHook
  , workspaces
  , borderWidth
  , normalBorderColor
  , focusedBorderColor
  , focusFollowsMouse
  , logHook
  , mod4Mask
  , mod1Mask
  , gets
  , windowset
  , liftIO
  , controlMask
  , xK_z
  , xK_k
  , xK_a
  , xK_e
  , xK_m
  , xK_b
  , xK_f
  , xK_y
  , xK_BackSpace
  , xK_g
  , xK_bracketleft
  , xK_d
  , xK_n
  , xK_p
  , xK_Return
  , xK_KP_Enter
  , xK_Delete
  , xK_Left
  , xK_Right
  , xK_Home
  , xK_End
  , xK_Down
  , xK_Up
  , xK_Escape
  , (|||)
  , composeAll
  , title
  , stringProperty
  , (=?)
  , (-->)
  , doShift
  , className
  , (<&&>)
  , resource
  , doFloat
  , spawn
  , io
  , withFocused
  , windows
  , sendMessage
  , ChangeLayout(NextLayout)
  , IncMasterN(IncMasterN)
  , Resize(Shrink, Expand)
  , (<+>)
  )
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.DynamicProjects (Project(Project), projectName, projectDirectory, projectStartHook, switchProjectPrompt, dynamicProjects, currentProject)
import qualified XMonad.Actions.DynamicWorkspaces as DynWs
import qualified XMonad.Actions.DynamicWorkspaceOrder as DynWsOrd
import XMonad.Actions.WorkspaceNames (getCurrentWorkspaceName)
import qualified XMonad.Actions.TreeSelect as TS

    -- Data
import Data.Char (isSpace, toUpper)
import qualified Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Tree

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops (ewmh) -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)
import XMonad.Hooks.ServerMode (serverModeEventHook, serverModeEventHookF, serverModeEventHookCmd)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

    -- Layouts
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.ResizableTile
  ( ResizableTall(ResizableTall)
  , MirrorResize(MirrorShrink, MirrorExpand)
  )
import XMonad.Layout.Tabbed
  ( tabbed
  , fontName
  , activeColor
  , inactiveColor
  , activeBorderColor
  , inactiveBorderColor
  , activeTextColor
  , inactiveTextColor
  , shrinkText
  , addTabs
  )

    -- Layouts modifiers
import qualified XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders, hasBorder, smartBorders)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Simplest (Simplest(Simplest))
import XMonad.Layout.Spacing (Spacing, spacingRaw, Border(Border), incWindowSpacing, decWindowSpacing, incScreenSpacing, decScreenSpacing)
import XMonad.Layout.SubLayouts (subLayout, pullGroup, GroupMsg(UnMerge, MergeAll, UnMergeAll), onGroup)
import XMonad.Layout.WindowNavigation (windowNavigation, Direction2D(L, R, U, D))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(Arrange, DeArrange))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(Toggle))

    -- Prompt
import XMonad.Prompt
  ( XPConfig
  , font
  , bgColor
  , fgColor
  , bgHLight
  , fgHLight
  , borderColor
  , promptBorderWidth
  , promptKeymap
  , position
  , XPPosition(CenteredAt)
  , xpCenterY
  , xpWidth
  , height
  , historySize
  , historyFilter
  , defaultText
  , autoComplete
  , showCompletionOnTab
  , searchPredicate
  , defaultPrompter
  , alwaysHighlight
  , maxComplRows
  , XP
  , def
  , killBefore
  , killAfter
  , startOfLine
  , endOfLine
  , deleteString
  , Direction1D(Next, Prev)
  , moveCursor
  , killWord
  , pasteString
  , quit
  , moveWord
  , moveHistory
  , setSuccess
  , setDone
  )
import XMonad.Prompt.Input (inputPrompt, (?+))
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)
import Control.Arrow (first)

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.ExclusiveScratchpads (customFloating)
import Data.Ratio ((%))

-- imports for polybar
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

myFont :: String
myFont = "xft:Hack Nerd Font:bold:size=9:antialias=true:hinting=true"

myPromptFont :: String
myPromptFont = "xft:Hack Nerd Font:bold:size=24:anialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty "

terminalRunning :: String -> String
terminalRunning cmd = myTerminal ++ " -e '" ++ cmd ++ "'"

myBrowser :: String
myBrowser = "firefox "

myEditor :: String
myEditor = terminalRunning " -e nvim "

myBorderWidth :: Dimension
myBorderWidth = 5

-- Border color of normal windows
myNormColor :: String
myNormColor   = "#222436"

-- Border color of focused windows
myFocusColor :: String
myFocusColor  = "#c3e88d"

-- Setting this for use in xprompts
altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          setWMName "LG3D"
          spawnOnce "sleep 2 && nitrogen --restore &"
          spawnOnce "sleep 2 && picom &"
          spawnOnce "sleep 2 && systemctl --user restart polybar.service"

jeyj0XPConfig :: XPConfig
jeyj0XPConfig = def
      { font                = myPromptFont
      , bgColor             = "#282828"
      , fgColor             = "#ebdbb2"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#8ec07c"
      , promptBorderWidth   = 0
      , promptKeymap        = jeyj0XPKeymap
      , position            = CenteredAt { xpCenterY = 0.1, xpWidth = 0.7 }
      , height              = 64
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id $ map toUpper
      , alwaysHighlight     = False
      , maxComplRows        = Just 10
      }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             , ("p", switchProjectPrompt)
             ]

jeyj0XPKeymap :: M.Map (KeyMask,KeySym) (XP ())
jeyj0XPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
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

nixpkgs, nixopts :: S.SearchEngine
nixpkgs  = S.searchEngine "nixpkgs" "https://search.nixos.org/packages?channel=unstable&from=0&size=30&sort=relevance&query="
nixopts  = S.searchEngine "nixopts" "https://search.nixos.org/options?channel=unstable&from=0&size=30&sort=relevance&query="

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("d", S.duckduckgo)
             , ("p", nixpkgs)
             , ("o", nixopts)
             , ("h", S.hoogle)
             , ("y", S.youtube)
             , ("s", S.stackage)
             ]

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] Simplest
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (5/8) []
equal    = renamed [Replace "equal"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] Simplest
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#222436"
                 , inactiveColor       = "#1b1d2b"
                 , activeBorderColor   = "#222436"
                 , inactiveBorderColor = "#1b1d2b"
                 , activeTextColor     = "#c8d3f5"
                 , inactiveTextColor   = "#444a73"
                 }

myLayoutHook =
  smartBorders $
  avoidStruts $
  mouseResize $
  windowArrange $
  mkToggle (NBFULL ?? EOT) $
  ifWider 1920 myWideLayout myThinLayout
  where
    myWideLayout = tall
      ||| noBorders tabs
      ||| equal

    myThinLayout = (renamed [Replace "tall"] $ reflectHoriz tall)
      ||| (renamed [Replace "tabs"] $ reflectHoriz $ noBorders tabs)
      ||| (renamed [Replace "equal"] $ reflectHoriz equal)

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces, and the names would very long if using clickable workspaces.
     [ title =? "Mozilla Firefox"     --> doShift "web" -- ( myWorkspaces !! 1 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , (className =? "Firefox" <&&> resource =? "Toolkit") --> (doRectFloat $ W.RationalRect (3%4) (3%4) (1%4) (1%4)) -- float and place picture-in-picture videos
     -- , title =? "Spotify" --> doShift ( myWorkspaces !! 3 )
     , title =? "Slack" --> doShift "chat" -- ( myWorkspaces !! 2 )
     , title =? "Zoom" --> doShift "chat" -- ( myWorkspaces !! 2 )
     , title =? "emacs-notepad" --> (customFloating $ W.RationalRect (1%6) (1%8) (4%6) (2%3))
     , className =? "discord" --> doShift "chat" -- ( myWorkspaces !! 2 )
     , className =? "Thunar" --> (doRectFloat $ W.RationalRect (1%4) (1%4) (2%4) (2%4))
     , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> (doRectFloat $ W.RationalRect (1%4) (1%4) (2%4) (2%4))
     , title =? "Error" --> (doRectFloat $ W.RationalRect (1%4) (1%4) (2%4) (2%4))
     , className =? "nnn" --> (doRectFloat $ W.RationalRect (1%4) (1%4) (2%4) (2%4))
     , className =? "Rofi" --> composeAll
        [ doRectFloat $ W.RationalRect (1%4) (1%4) (2%4) (2%4)
        , hasBorder False
        ]
     ]

data EmacsOpenAction
  = Eval String
  | File String
  | NoEmacsAction

launchEmacsclient servername mFrameName openAction = do
  let baseCmd = "emacsclient \
                \--alternate-editor=\"\" \
                \--socket-name=\"" ++ servername ++ "\" \
                \--create-frame"

  let baseCmd' = case mFrameName of
        Nothing -> baseCmd
        Just name -> baseCmd ++ " --frame-parameters=\"(quote (name . \\\"" ++ name ++ "\\\"))\""

  case openAction of
    NoEmacsAction -> spawn baseCmd'
    Eval eval -> spawn $ baseCmd' ++ " -e \"" ++ eval ++ "\""
    File file -> spawn $ baseCmd' ++ " \"" ++ file ++ "\""
  
launchEmacsclient' servername = launchEmacsclient servername Nothing NoEmacsAction

launchEmacsclientForProject = do
  project <- currentProject
  launchEmacsclient' $ projectName project
  
-- rbg not rgb
myTreeSelectConf = TS.TSConfig
  { TS.ts_hidechildren = True
  , TS.ts_background = 0x00000000
  -- , TS.ts_font = "xft:Sans-16"
  , TS.ts_font = "xft:Atkinson Hyperlegible-16"
  , TS.ts_node = (0xffebb2db, 0xff1d2021)
  , TS.ts_nodealt = (0xffebb2db, 0xff282828)
  , TS.ts_highlight = (0xff282828, 0xff98971a)
  , TS.ts_extra = 0xff66545c
  , TS.ts_node_width = 200
  , TS.ts_node_height = 30
  , TS.ts_originX = 0
  , TS.ts_originY = 0
  , TS.ts_indent = 50
  , TS.ts_navigate = TS.defaultNavigation
  }

treeSelectOpenActions =
  [ Node (TS.TSNode "Terminal" "" (spawn myTerminal)) []
  , Node (TS.TSNode "Browser" "" (spawn myBrowser)) []
  , Node (TS.TSNode "Files" "Launches nnn in current project directory" $ spawn $ myTerminal ++ "--class Alacritty,nnn --command nnn") []
  ]

-- M (GUI)   : Do something with xmonad
--             Common actions should only require this key to be held. Everything in this config should require it to be held though.
-- M1 (Alt)  : Windows
--             Manipulating windows should be done with Alt, as it's generally a fairly frequent action, but not always fully frequent
-- C (Ctrl)  : Workspaces/Projects and screens
--             Workspaces are less common to manipulate than windows, so they should be accessed with ctrl (which is not as nice to access for me)
-- S (Shift) : Unusual actions
--             This is really not nice to access. Don't put normal stuff here.
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-S-r", spawn "xmonad --restart")   -- Restarts xmonad
        , ("M-S-l", spawn "xfce4-session-logout &")

    -- Run Prompt
        , ("M-o", spawn "rofi -show run -normal-window")
        , ("M-S-o", TS.treeselectAction myTreeSelectConf treeSelectOpenActions) -- Open action tree select

    -- Useful programs to have a keybinding for launch
        , ("M-t", spawn (myTerminal))
        , ("M-v", spawn (myEditor))

    -- Kill windows
        , ("M-c", kill1)                         -- Kill the currently focused client
        , ("M-M1-c", killAll)                       -- Kill all windows on current workspace

    -- Workspaces
        , ("M-m", nextScreen)  -- Switch focus to next monitor
        , ("M-S-m", prevScreen)  -- Switch focus to prev monitor
        , ("M-,", moveTo Prev nonNSP)
        , ("M-.", moveTo Next nonNSP)
        , ("M-i", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-a", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
        , ("M-w c", DynWs.removeEmptyWorkspace)

    -- Floating windows
        , ("M-f", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-f", sinkAll)                       -- Push ALL floating windows to tile

    -- Windows navigation
        -- , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-n", windows W.focusDown)    -- Move focus to the next window
        , ("M-S-n", windows W.focusUp)      -- Move focus to the prev window
        , ("M-M1-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-M1-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-M1-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Space>", promote)     -- Moves focused window to master, others maintain order
        , ("M-C-S-n", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-n", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Esc>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase number of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease number of windows

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-<Left>", sendMessage $ pullGroup L)
        , ("M-C-<Down>", sendMessage $ pullGroup D)
        , ("M-C-<Up>", sendMessage $ pullGroup U)
        , ("M-C-<Right>", sendMessage $ pullGroup R)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- Multimedia Keys
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")
        ]
    -- Appending search engine prompts to keybindings list.
    -- Look at "search engines" section of this config for values for "k".
        ++ [("M-s " ++ k, S.promptSearch jeyj0XPConfig f) | (k,f) <- searchList ]
        ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
    -- Appending some extra xprompts to keybindings list.
    -- Look at "xprompt settings" section this of config for values for "k".
        ++ [("M-p " ++ k, f jeyj0XPConfig) | (k,f) <- promptList ]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

-- these are the open-by-default workspaces
-- they have to be defined in the project list as well
myWorkspaces :: [String]
myWorkspaces = ["home", "web"]

projects :: [Project]
projects =
    [ Project { projectName = "home"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "web"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "lychee"
              , projectDirectory = "~/Documents/06-mass-storage/3d-printing"
              , projectStartHook = Nothing
              }
    ]

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    --xmproc0 <- spawnPipe "xmobar -x 0 /home/jeyj0/.config/xmobar/xmobarrc0"
    -- xmproc1 <- spawnPipe "xmobar -x 0 /home/jeyj0/.config/xmobar/xmobarrc1"
    -- xmproc2 <- spawnPipe "xmobar -x 1 /home/jeyj0/.config/xmobar/xmobarrc2"
    dbus <- mkDbusClient
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh $ dynamicProjects projects def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , focusFollowsMouse  = False
        -- , logHook = workspaceHistoryHook <+> dynamicLogWithPP xmobarPP
        , logHook = workspaceHistoryHook <+> dynamicLogWithPP (polybarHook dbus)
                        -- { ppOutput = \x -> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x -- >> hPutStrLn xmproc2 x
                        -- , ppCurrent = xmobarColor "#8ec07c" "" . wrap "[ " " ]" -- Current workspace in xmobar
                        -- , ppVisible = xmobarColor "#8ec07c" "" . wrap "  " "  "                -- Visible but not current workspace
                        -- , ppHidden = xmobarColor "#458588" "" . wrap "( " " )"   -- Hidden workspaces in xmobar
                        -- , ppHiddenNoWindows = xmobarColor "#928374" "" . wrap "  " "  "        -- Hidden workspaces (no windows)
                        -- , ppTitle = xmobarColor "#ebdbb2" "" . shorten 60     -- Title of active window in xmobar
                        -- , ppSep =  "<fc=#928374> <fn=2>|</fn> </fc>"          -- Separators in xmobar
                        -- , ppUrgent = xmobarColor "#cc241d" "" . wrap "!" "!"  -- Urgent workspace
                        -- , ppExtras  = []-- [windowCount] -- # of windows current workspace
                        -- , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        -- }
        } `additionalKeysP` myKeys

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log") opts
  return dbus
  where
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let
    opath = D.objectPath_ "/org/xmonad/Log"
    iname = D.interfaceName_ "org.xmonad.Log"
    mname = D.memberName_ "Update"
    signal = (D.signal opath iname mname)
    body = [D.toVariant $ UTF8.decodeString str]
  in D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let
    wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "}") "%{F-}" s
                | otherwise = mempty
    green = "#c3e88d"
    blue = "#82aaff"
    gray = "#444a73"
    fg = "#c8d3f5"
    red = "#ff757f"
  in def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrapper green . wrap "[ " " ]"
    , ppVisible = wrapper green . wrap "  " "  "
    , ppUrgent = wrapper red . wrap " !" "! "
    , ppHidden = wrapper blue . wrap "( " " )"
    , ppHiddenNoWindows = wrapper gray . wrap "  " "  "
    , ppTitle = shorten 60 . wrapper fg -- . wrap "  " "  "
    , ppSep =  "%{F" <> gray <> "} | %{F-}"
    }

