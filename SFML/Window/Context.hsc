{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Context
(
    createContext
,   destroyContext
,   setActiveContext
)
where


import SFML.Window.Types

import Foreign.C.Types

#include <SFML/Window/Context.h>


-- | Create a new context.
-- 
-- This function activates the new context.
createContext = sfContext_create

foreign import ccall "sfContext_create"
    sfContext_create :: IO SFContext

-- | Destroy the context.
destroyContext = sfContext_destroy

foreign import ccall "sfContext_destroy"
    sfContext_destroy :: SFContext -> IO ()

-- | Activate or deactivate explicitely a context.
setActiveContext :: SFContext -> Bool -> IO ()
setActiveContext ctx val = sfContext_setActive ctx (fromIntegral . fromEnum $ val)

foreign import ccall "sfContext_setActive"
    sfContext_setActive :: SFContext -> CChar -> IO ()

