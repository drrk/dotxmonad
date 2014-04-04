import Control.Monad
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import System.Exit
import System.IO

-- Window layouts
layouts =
    -- apply avoidStruts to all layouts to allow xmobar and trayer at the top
    -- of the screen
    avoidStruts
    -- apply a full screen window toggle to all layouts, allowing a window to
    -- be temporarily made full screen via a key binding
    $ toggleLayouts Full
    -- Layouts below, in order of preference
    $   tall
    ||| Mirror tall
    ||| ThreeCol nMaster resizeDelta masterRatio
    ||| Full
    -- add a basic floating-only layout at the end of the layout list
    ||| simpleFloat
  where tall = Tall nMaster resizeDelta masterRatio
        resizeDelta = 3/100
        masterRatio = 1/2
        nMaster     = 1

keyBindings = [
        -- Lock the screen
          ((0, xK_Pause),                  spawn "xscreensaver-command -lock")
        , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        -- Prompt to execute a program
        , ((mod4Mask, xK_r),               shellPrompt defaultXPConfig)
        -- Toggle the current window to full screen
        , ((mod4Mask .|. shiftMask, xK_m), sendMessage $ Toggle "Full")
        -- Replacement quit binding
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
        , ((mod4Mask .|. shiftMask, xK_q),                 spawn "xmonad --recompile && xmonad --restart")
    ]

main = do
    -- Create an xmobar communication pipe
    xmproc <- spawnPipe "xmobar"
    -- Run XMonad, altering the default config
    xmonad $ defaultConfig
        { borderWidth = 2
        -- Called via sh -c, use exec to keep useless shells down
        , terminal = "exec xterm"
        -- Windows key is the control key
        , modMask = mod4Mask
        -- Change the window manager name to make Java apps happy about a
        -- non-reparenting window manager
        , startupHook = setWMName "LG3D"
        -- Add some dock management for xmobar and trayer
        , manageHook = manageDocks <+> manageHook defaultConfig
        -- Custom layouts list
        , layoutHook = layouts
        -- Use a log hook to:
        --  * bodge around an ICCCM compliance issue affecting Java Swing
        --  * update xmobar
        , logHook = takeTopFocus >> dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "grey90" "" . wrap "[" "]"
                        , ppTitle = xmobarColor "grey90" "" . shorten 50
                        }
        , focusFollowsMouse = False
        , normalBorderColor = "grey40"
        }
        -- apply additional and over-riding key bindings
        `additionalKeys` keyBindings
        -- the default quite binding is too close to Google Chrome's
        -- the default restart binding is too close to other quit bindings
        `removeKeys` [ (mod4Mask, xK_q) ]

-- This set of definitions creates a function which can be used to get around
-- an ICCCM compliance problem in XMonad which causes the focus in Java Swing
-- apps to behave erratically.

atom_WM_TAKE_FOCUS :: X Atom
atom_WM_TAKE_FOCUS = getAtom "WM_TAKE_FOCUS"

takeFocusX :: Window -> X ()
takeFocusX w =
  withWindowSet . const $ do
    dpy       <- asks display
    wmtakef   <- atom_WM_TAKE_FOCUS
    wmprot    <- atom_WM_PROTOCOLS
    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $
      io . allocaXEvent $ \ev -> do
          setEventType ev clientMessage
          setClientMessageEvent ev w wmprot 32 wmtakef currentTime
          sendEvent dpy w False noEventMask ev

takeTopFocus :: X ()
takeTopFocus =
  withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . peek
