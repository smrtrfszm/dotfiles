module GtkFrameExtents (
    gtkFrameExtents,
    gtkRemoveFrameExtents,
    ) where

import XMonad
import XMonad.Prelude

gtkFrameExtents :: XConfig a -> XConfig a
gtkFrameExtents c = c { startupHook = startupHook c <> gtkFrameExtentsStartUp
                      , manageHook = manageHook c <> gtkFrameExtentsRemove
                      }

gtkFrameExtentsStartUp :: X ()
gtkFrameExtentsStartUp = addSupported ["_GTK_FRAME_EXTENTS", "_NET_WM_STATE_MAXIMIZED_HORZ", "_NET_WM_STATE_MAXIMIZED_VERT"]

gtkRemoveFrameExtents :: X ()
gtkRemoveFrameExtents = withFocused $ \w -> do
    withDisplay $ \dpy -> do
        -- io $ spawn "xmessage asd"
        at <- getAtom "_NET_WM_STATE"
        atoms <- mapM getAtom
            [ "_NET_WM_STATE_MAXIMIZED_HORZ"
            , "_NET_WM_STATE_MAXIMIZED_VERT"
            ]
        io $ changeProperty32 dpy w at aTOM propModeAppend (fmap fromIntegral atoms)

gtkFrameExtentsRemove :: ManageHook
gtkFrameExtentsRemove = ask >>= \w -> liftX $ do
    dpy <- asks display
    a <- getAtom "_GTK_FRAME_EXTENTS"
    extents <- io $ join . maybeToList <$> getWindowProperty32 dpy a w
    case extents of
        [] -> return ()
        otherwise -> do
            at <- getAtom "_NET_WM_STATE"
            atoms <- mapM getAtom
                [ "_NET_WM_STATE_MAXIMIZED_HORZ"
                , "_NET_WM_STATE_MAXIMIZED_VERT"
                ]
            io $ changeProperty32 dpy w at aTOM propModeAppend (fmap fromIntegral atoms)
    mempty

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
        supportedList <- join . maybeToList <$> getWindowProperty32 dpy a r
        changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)
