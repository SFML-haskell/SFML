import SFML.Graphics
import SFML.Window

import Data.Maybe (fromJust)


main = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    tex <- fmap fromJust $ textureFromFile "Haskell-Logo.png" Nothing
    spr <- fmap fromJust $ createSprite
    setTexture spr tex True
    loop wnd spr
    destroySprite spr
    destroyTexture tex
    destroyRenderWindow wnd


loop :: RenderWindow -> Sprite -> IO ()
loop wnd spr = do
    drawSprite wnd spr Nothing
    display wnd
    evt <- waitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        _ -> loop wnd spr

