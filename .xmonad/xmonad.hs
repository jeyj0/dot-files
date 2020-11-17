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
import XMonad.Actions.DynamicProjects (Project(Project), projectName, projectDirectory, projectStartHook, switchProjectPrompt, dynamicProjects)
import qualified XMonad.Actions.DynamicWorkspaces as DynWs
import qualified XMonad.Actions.DynamicWorkspaceOrder as DynWsOrd

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

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
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Prompt.Directory
import Control.Arrow (first)

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Hack Nerd Font:bold:size=9:antialias=true:hinting=true"

myPromptFont :: String
myPromptFont = "xft:Hack Nerd Font:bold:size=24:anialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty "   -- Sets default terminal
-- myTerminal = "alacritty --working-directory $(cat ~/pwd) "   -- Sets default terminal

myBrowser :: String
myBrowser = "firefox "               -- Sets qutebrowser as browser for tree select
-- myBrowser = myTerminal ++ " -e lynx " -- Sets lynx as browser for tree select

myEditor :: String
-- myEditor = "emacsclient -c -a emacs "  -- Sets emacs as editor for tree select
myEditor = myTerminal ++ " -e nvim "    -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 5          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282828"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#8ec07c"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "nitrogen --restore &"
          setWMName "LG3D"

jeyj0XPConfig :: XPConfig
jeyj0XPConfig = def
      { font                = myPromptFont
      , bgColor             = "#282828"
      , fgColor             = "#ebdbb2"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#8ec07c"
      , promptBorderWidth   = 0
      , promptKeymap        = dtXPKeymap
      -- , position            = Top
      , position            = CenteredAt { xpCenterY = 0.1, xpWidth = 0.7 }
      , height              = 64
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      -- , maxComplRows        = Nothing      -- set to 'Just 5' for 5 rows
      , maxComplRows        = Just 10      -- set to 'Just 5' for 5 rows
      }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
jeyj0XPConfig' :: XPConfig
jeyj0XPConfig' = jeyj0XPConfig
      { autoComplete        = Nothing
      }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             -- , ("p", passPrompt)         -- get passwords (requires 'pass')
             -- , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             -- , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             , ("p", switchProjectPrompt)
             ]

-- Same as the above list except this is for my custom prompts.
promptList' :: [(String, XPConfig -> String -> X (), String)]
promptList' = [ ("c", calcPrompt, "qalc")         -- requires qalculate-gtk
              ]

calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace

dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
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

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] Simplest
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
floats   = renamed [Replace "floats"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] Simplest -- (smartBorders Simplest)
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] Simplest -- (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] Simplest -- (smartBorders Simplest)
           $ mySpacing 8
           $ spiral (6/7)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#3c3836"
                 , inactiveColor       = "#282828"
                 , activeBorderColor   = "#3c3836"
                 , inactiveBorderColor = "#1c1c1c"
                 , activeTextColor     = "#ebdbb2"
                 , inactiveTextColor   = "#928374"
                 }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Hack Nerd Font:bold:size=24"
    , swn_fade              = 0.3
    , swn_bgcolor           = "#1c1c1c"
    , swn_color             = "#ebdbb2"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     tall
                                 ||| noBorders tabs
                                 ||| grid
                                 ||| spirals

myWorkspaces = ["dev", "web"]

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myClickableWorkspaces :: [String]
myClickableWorkspaces = clickable . (map xmobarEscape)
               $ myWorkspaces
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces, and the names would very long if using clickable workspaces.
     [ title =? "Mozilla Firefox"     --> doShift "web" -- ( myWorkspaces !! 1 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     -- , title =? "Spotify" --> doShift ( myWorkspaces !! 3 )
     , title =? "Slack" --> doShift "chat" -- ( myWorkspaces !! 2 )
     , title =? "Zoom" --> doShift "chat" -- ( myWorkspaces !! 2 )
     , className =? "discord" --> doShift "chat" -- ( myWorkspaces !! 2 )
     ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0


-- M (GUI)  : Do something with xmonad
--             Common actions should only require this key to be held. Everything in this config should require it to be held though.
-- M1 (Alt) : Windows
--             Manipulating windows should be done with Alt, as it's generally a fairly frequent action, but not always fully frequent
-- C (Ctrl) : Workspaces/Projects and screens
--             Workspaces are less common to manipulate than windows, so they should be accessed with ctrl (which is not as nice to access for me)
-- S (Shift): Unusual actions
--             This is really not nice to access. Don't put normal stuff here.
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-S-c", spawn "xmonad --recompile") -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")   -- Restarts xmonad
        , ("M-S-q", io exitSuccess)             -- Quits xmonad

    -- Run Prompt
        , ("M-o", shellPrompt jeyj0XPConfig) -- Shell Prompt

    -- Useful programs to have a keybinding for launch
        , ("M-t", spawn (myTerminal))
        , ("M-b", spawn (myBrowser))
        , ("M-e", spawn "emacsclient -c -a 'emacs'")
        , ("M-v", spawn (myEditor))
        -- , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

    -- Kill windows
        , ("M-c", kill1)                         -- Kill the currently focused client
        , ("M-M1-c", killAll)                       -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-u", moveTo Prev nonNSP)
        , ("M-i", moveTo Next nonNSP)
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
        , ("M-w c", DynWs.removeEmptyWorkspace)
        , ("M-C-i", DynWsOrd.swapWith Next NonEmptyWS)
        , ("M-C-u", DynWsOrd.swapWith Prev NonEmptyWS)

    -- Floating windows
        -- , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-f", withFocused $ windows . W.sink)  -- Push floating window back to tile
        -- , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("M-M1-h", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-M1-l", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-M1-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-M1-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-M1-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-C-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-M1-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", sendMessage ToggleStruts)     -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase number of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease number of windows

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
        , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
        , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
        -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")
        ]
    -- Appending search engine prompts to keybindings list.
    -- Look at "search engines" section of this config for values for "k".
        ++ [("M-s " ++ k, S.promptSearch jeyj0XPConfig' f) | (k,f) <- searchList ]
        ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
    -- Appending some extra xprompts to keybindings list.
    -- Look at "xprompt settings" section this of config for values for "k".
        ++ [("M-p " ++ k, f jeyj0XPConfig') | (k,f) <- promptList ]
        ++ [("M-p " ++ k, f jeyj0XPConfig' g) | (k,f,g) <- promptList' ]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

projects :: [Project]
projects =
    [ Project { projectName = "dev"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "web"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                  spawn myBrowser
              }
    , Project { projectName = "chat"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "music"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                  spawn "spotify"
              }
    , Project { projectName = "bgRun"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "done"
              , projectDirectory = "~/projects/done/"
              , projectStartHook = Just $ do
                  spawn myEditor
              }
    , Project { projectName = "easyforms"
              , projectDirectory = "~/projects/easyforms"
              , projectStartHook = Just $ do
                  spawn myEditor
              }
    ]

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc0 <- spawnPipe "xmobar -x 0 /home/jeyj0/.config/xmobar/xmobarrc0"
    --xmproc1 <- spawnPipe "xmobar -x 1 /home/jeyj0/.config/xmobar/xmobarrc2"
    --xmproc2 <- spawnPipe "xmobar -x 2 /home/jeyj0/.config/xmobar/xmobarrc1"
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
        -- , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , focusFollowsMouse  = False
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x  -- >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x
                        , ppCurrent = xmobarColor "#8ec07c" "" . wrap "[ " " ]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#8ec07c" "" . wrap "  " "  "                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#458588" "" . wrap "( " " )"   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#928374" "" . wrap "  " "  "        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#ebdbb2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#928374> <fn=2>|</fn> </fc>"          -- Separators in xmobar
                        , ppUrgent = xmobarColor "#cc241d" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys
