module SFML.Graphics.Types
where


import Foreign.C.Types (CInt)
import Foreign.Ptr
import Foreign.Storable


newtype CircleShape = CircleShape (Ptr CircleShape)
newtype ConvexShape = ConvexShape (Ptr ConvexShape)
newtype Font = Font (Ptr Font)
newtype Image = Image (Ptr Image)
newtype Shader = Shader (Ptr Shader)
newtype RectangleShape = RectangleShape (Ptr RectangleShape)
newtype RenderTexture = RenderTexture (Ptr RenderTexture)
newtype RenderWindow = RenderWindow (Ptr RenderWindow)
newtype Shape = Shape (Ptr Shape)
newtype Sprite = Sprite (Ptr Sprite)
newtype Text = Text (Ptr Text)
newtype Texture = Texture (Ptr Texture)
newtype Transformable = Transformable (Ptr Transformable)
newtype VertexArray = VertexArray (Ptr VertexArray)
newtype View = View (Ptr View)


instance Storable Texture where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = peek (castPtr ptr) >>= return . Texture
    poke ptr (Texture p) = poke (castPtr ptr) p


instance Storable Shader where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = peek (castPtr ptr) >>= return . Shader
    poke ptr (Shader p) = poke (castPtr ptr) p

