import SFML.Window


main = do
    desktopMode <- getDesktopMode
    fsModes <- getFullscreenModes
    
    putStrLn $ "Current desktop mode:\n\n" ++ show desktopMode
    putStrLn ""
    putStrLn $ "Fullscreen modes:"
    putStrLn ""
    mapM_ (\m -> putStrLn (show m) >> putStrLn "") fsModes
    
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    loop wnd
    destroy wnd


loop :: Window -> IO ()
loop wnd = do
    evt <- waitEvent wnd
    case evt of
        Just SFEvtClosed -> return ()
        _ -> loop wnd

