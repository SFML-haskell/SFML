import SFML.Audio
import SFML.Graphics
import SFML.Window

import Foreign.Ptr (nullPtr)

import Paths_sfml_demos


main = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    logoPath  <- getDataFileName "Haskell-Logo.png"
    fontPath  <- getDataFileName "Vera.ttf"
    musicPath <- getDataFileName "DST-BreakOut.ogg"
    tex <- err $ textureFromFile logoPath Nothing
    spr <- err $ createSprite
    fnt <- err $ fontFromFile fontPath
    txt <- err $ createText
    setTextString txt "Haskell SFML\n  Version 2.0"
    setTextFont txt fnt
    setTextCharacterSize txt 20
    setTextColor txt blue
    msc <- err $ musicFromFile musicPath
    play msc
    setTexture spr tex True
    loop wnd spr txt
    destroy msc
    destroy txt
    destroy fnt
    destroy spr
    destroy tex
    destroy wnd


loop :: RenderWindow -> Sprite -> Text -> IO ()
loop wnd spr txt = do
    drawSprite wnd spr Nothing
    drawText   wnd txt $ Just (RenderStates BlendAlpha (translation 460 40) (Texture nullPtr) (Shader nullPtr))
    display wnd
    evt <- waitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        _ -> loop wnd spr txt
        