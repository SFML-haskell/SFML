import SFML.Audio
import SFML.Graphics
import SFML.Window

import Foreign.Ptr (nullPtr)
import Paths_sfml_demos


txtSize = 24


data DemoState = DemoState
    { xmouse :: Int
    , ymouse :: Int
    , key    :: String
    }


main = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    fontPath <- getDataFileName "Vera.ttf"
    fnt <- err $ fontFromFile fontPath
    txt <- err $ createText
    setTextFont txt fnt
    setTextCharacterSize txt txtSize
    setTextColor txt blue
    let ds = DemoState 0 0 ""
    loop wnd txt ds
    destroy txt
    destroy fnt
    destroy wnd


loop :: RenderWindow -> Text -> DemoState -> IO ()
loop wnd txt ds = do
    ret <- processEvt wnd ds
    case ret of
        Nothing -> return ()
        Just ds' -> do
            clearRenderWindow wnd $ Color 240 240 240 255
            setTextString txt $ "Mouse: " ++ show (xmouse ds') ++ ", " ++ show (ymouse ds')
            drawText wnd txt Nothing
            setTextString txt $ "Keyboard: " ++ key ds'
            let rs = renderStates { transform = (translation 0 $ 2 * fromIntegral txtSize) }
            drawText wnd txt $ Just rs
            display wnd
            loop wnd txt ds'


processEvt :: RenderWindow -> DemoState -> IO (Maybe DemoState)
processEvt wnd ds = do
    evt <- pollEvent wnd
    case evt of
        Just SFEvtClosed -> return Nothing
        Just (SFEvtMouseMoved x y) -> processEvt wnd $ ds { xmouse = x, ymouse = y }
        Just e@SFEvtKeyPressed{} -> processEvt wnd $ ds { key = show . code $ e }
        Nothing -> return . Just $ ds
        _ -> processEvt wnd ds
