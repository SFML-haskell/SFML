import SFML.Graphics
import SFML.Window

import Paths_sfml_demos


txtSize = 24
angry = "è_é"


main = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow (VideoMode 40 32 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    fontPath <- getDataFileName "Vera.ttf"
    fnt <- err $ fontFromFile fontPath
    txt <- err $ createText
    setTextFont txt fnt
    setTextCharacterSize txt txtSize
    setTextColor txt blue
    setTextStringU txt angry
    loop wnd txt
    destroy txt
    destroy fnt
    destroy wnd


loop :: RenderWindow -> Text -> IO ()
loop wnd txt = do
    evt <- waitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        Just _ -> do
            clearRenderWindow wnd $ Color 240 240 240 255
            drawText wnd txt Nothing
            display wnd
            loop wnd txt
