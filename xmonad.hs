-- core
import XMonad
import Data.Monoid
import System.Exit
import System.IO (hPutStrLn)

-- data 
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- layouts
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

-- prompts
import XMonad.Prompt
import XMonad.Prompt.Shell

-- utilities
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig

-----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
-- user variables
myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#dddddd"

myFocusedBorderColor = "#3de8c6"
-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "pkill xmobar; xmonad --recompile; xmonad --restart")
    , ((modm,               xK_f     ), spawn "firefox")
    , ((modm,               xK_u     ), shellPrompt def)
    ]
    ++
    -- change workspaces
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- move windows to workspaces
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
-- Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]
-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-- Layouts
myLayout = avoidStruts(spacing 10 $ smartBorders (tiled ||| Mirror tiled ||| Full))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
-- event handling
myEventHook = mempty
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
myLogHook = mempty

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
-- manage hooks
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
    spawnOnce "picom --experimental-backends &"
    spawnOnce "feh --bg-scale ~/Downloads/wallpapers/wallhaven-oxp58l.jpg "
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
-- bar
myBar = "xmobar"

myPP = xmobarPP {
    ppCurrent = xmobarColor "#ff79c6" "" . wrap "[""]",
    ppVisible = xmobarColor "#bd93f9" "",
    ppHidden = xmobarColor "#f8f8f2" "",
    ppHiddenNoWindows = xmobarColor "#6272a4" "",
    ppTitle = xmobarColor "#8be9fd" "" . shorten 60,
    ppSep = "|",
    ppOrder  = \(ws:l:t:ex)  -> [ws,t]
}

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
-- Main
main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults
defaults = def {
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
	startupHook        = myStartupHook,
        logHook            = myLogHook,
	terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout
}
