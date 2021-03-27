-- Imports ----------------------------------------

import System.IO
import System.Exit

import XMonad

import Data.Array
import Data.String
import Data.Monoid
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen

import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes


-- Constants --------------------------------------

_modMask = mod4Mask

_terminal :: String
_terminal = "urxvt"

_xmobar :: String
_xmobar = "~/.config/xmonad/xmobar.hs"

_workspaces = ["1","2","3","4","5","6","7","8","9"]

_focusFollowsMouse :: Bool
_focusFollowsMouse = True

_clickJustFocuses :: Bool
_clickJustFocuses = False

_normalBorderColor :: String 
_normalBorderColor = "#6e4a94"

_focusedBorderColor :: String 
_focusedBorderColor = "#dddddd"

_xmobarTitleColor :: String
_xmobarTitleColor = "FFB6B0"

_xmobarCurrentWorkspaceColor :: String
_xmobarCurrentWorkspaceColor = "CEFFAC"

-- Keybindings ------------------------------------

_keys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ 
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) 
    , ((modm .|. shiftMask, xK_p), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_w), spawn "brave")
    , ((modm .|. shiftMask, xK_e), spawn "emacs")
    , ((modm .|. shiftMask, xK_r), spawn "code")
    , ((modm .|. shiftMask, xK_s), spawn "spotify")
    , ((modm .|. shiftMask, xK_d), spawn "discord")
   
    -- Cycle layout
    , ((modm, xK_space ), sendMessage NextLayout)
    -- Reset layout
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Window
    -- Kill
    , ((modm .|. shiftMask, xK_c), kill)
    , ((modm, xK_n), refresh)    
    -- Move 
    , ((modm, xK_Tab), windows W.focusDown)
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)
    -- Swap focused with master
    , ((modm, xK_Return), windows W.swapMaster)
    -- Swap focused with next
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap focused window with previous
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Resize master
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    -- Push back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Number in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
   
    -- Restart xmonad
    , ((modm, xK_z), spawn "xmonad --recompile && xmonad --restart")
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_z), io (exitWith ExitSuccess))
    ] ++
    
    -- mod-shift-{u, i, o}, Move client to screen L, M, or R
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_i, xK_o, xK_u] [0..],
          (f, m) <- [ (W.view, 0), (W.shift, shiftMask) ]
    ] ++
    
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modm, k), windows $ f i) 
     | (i, k) <- zip (XMonad.workspaces conf) [ xK_1 .. xK_9 ],
       (f, m) <- [ (W.greedyView, 0), (W.shift, shiftMask) ]]
    -- ++
    
_mouse (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))]

-- Layouts ----------------------------------------

defaultGaps = [(U,15),(D,15),(R,15),(L,15)]

_layout = avoidStruts $
  gaps defaultGaps ( tiled ||| 
  Mirror tiled) |||
  noBorders Full
    where
      tiled   = Tall nmaster delta ratio
      nmaster = 1
      ratio   = 1/2
      delta   = 3/100
      theme   = darkTheme 

_borderWidth = 2

-- Hooks -----------------------------------------

_manageHook = composeAll [
  manageDocks,
  className =? "Vlc" --> doFloat,
  className =? "gmrun" --> doFloat,
  isFullscreen --> doFullFloat ]

_eventHook = mempty

_logHook = return ()

_startupHook = mempty

-- Main ------------------------------------------

main = do
    xmproc <- spawnPipe ("xmobar " ++ _xmobar)
    xmonad $ docks defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor _xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor _xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
      , manageHook = manageDocks <+> _manageHook

      , startupHook = _startupHook
      , handleEventHook = docksEventHook
  }

defaults = def {
        terminal           = _terminal,
        focusFollowsMouse  = _focusFollowsMouse,
        clickJustFocuses   = _clickJustFocuses,
        borderWidth        = _borderWidth,
        modMask            = _modMask,
        workspaces         = _workspaces,
        normalBorderColor  = _normalBorderColor,
        focusedBorderColor = _focusedBorderColor,

        keys               = _keys,
        mouseBindings      = _mouse,

        layoutHook         = _layout,
        manageHook         = _manageHook,
        handleEventHook    = _eventHook,
        logHook            = _logHook,
        startupHook        = _startupHook
    }
