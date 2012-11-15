import SFML.Audio
import SFML.Graphics
import SFML.Window

import Data.Maybe (fromJust)
import Paths_sfml_demos


main = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    logoPath  <- getDataFileName "hello/Haskell-Logo.png"
    fontPath  <- getDataFileName "hello/Vera.ttf"
    musicPath <- getDataFileName "hello/DST-BreakOut.ogg"
    tex <- fmap fromJust $ textureFromFile logoPath Nothing
    spr <- fmap fromJust $ createSprite
    fnt <- fmap fromJust $ fontFromFile fontPath
    txt <- fmap fromJust $ createText
    setTextString txt "Hello world!"
    setTextFont txt fnt
    setTextCharacterSize txt 50
    setTextColor txt green
    msc <- fmap fromJust $ musicFromFile musicPath
    play msc
    setTexture spr tex True
    loop wnd spr txt
    destroyMusic msc
    destroyText txt
    destroyFont fnt
    destroySprite spr
    destroyTexture tex
    destroyRenderWindow wnd


loop :: RenderWindow -> Sprite -> Text -> IO ()
loop wnd spr txt = do
    drawSprite wnd spr Nothing
    drawText   wnd txt Nothing
    display wnd
    evt <- waitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        _ -> loop wnd spr txt

