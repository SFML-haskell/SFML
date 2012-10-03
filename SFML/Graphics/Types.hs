module SFML.Graphics.Types
where


import Foreign.Ptr (Ptr)


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

