import SFML.Graphics
import SFML.Window


colors :: [Color]
colors = [red, blue, green, magenta, cyan, yellow]

nColors :: Int
nColors = length colors


main = do
  vMode <- getDesktopMode
  wnd <- createRenderWindow vMode "SFML Haskell Demo" [SFFullscreen] Nothing
  va <- createVA
  setPrimitiveType va Triangles
  loop wnd va
  destroy va
  destroy wnd


loop :: RenderWindow -> VertexArray -> IO ()
loop wnd va = do
  ret <- processEvt wnd va
  case ret of
    False -> return ()
    True -> do
      clearRenderWindow wnd black
      drawVertexArray wnd va Nothing
      display wnd
      loop wnd va


appendVertex :: VertexArray -> Int -> Int -> Int -> IO ()
appendVertex va f x y = do
  let color = colors !! (f `mod` nColors)
      x1 = fromIntegral $ x - 10
      x2 = fromIntegral $ x + 10
      y1 = fromIntegral $ y - 10
      y2 = fromIntegral $ y + 10
      corners = [ Vec2f x1 y1, Vec2f x1 y2, Vec2f x2 y2,
                  Vec2f x1 y1, Vec2f x2 y2, Vec2f x2 y1 ]
      vtx v = Vertex v color v
      vertices = map vtx corners
  mapM_ (appendVA va) vertices


processEvt :: RenderWindow -> VertexArray -> IO Bool
processEvt wnd va = do
  evt <- pollEvent wnd
  case evt of
    Just SFEvtClosed -> return False
    Just (SFEvtKeyPressed {code = KeyEscape}) -> return False
    Just (SFEvtTouchBegan f x y) -> appendVertex va f x y >> processEvt wnd va
    Just (SFEvtTouchMoved f x y) -> appendVertex va f x y >> processEvt wnd va
    Just (SFEvtTouchEnded f x y) -> appendVertex va f x y >> processEvt wnd va
    Nothing -> return True
    _ -> processEvt wnd va
