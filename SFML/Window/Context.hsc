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

foreign import ccall unsafe "sfContext_create"
    sfContext_create :: IO Context

-- | Destroy the context.
destroyContext = sfContext_destroy

foreign import ccall unsafe "sfContext_destroy"
    sfContext_destroy :: Context -> IO ()

-- | Activate or deactivate explicitely a context.
setActiveContext :: Context -> Bool -> IO ()
setActiveContext ctx val = sfContext_setActive ctx (fromIntegral . fromEnum $ val)

foreign import ccall unsafe "sfContext_setActive"
    sfContext_setActive :: Context -> CInt -> IO ()

