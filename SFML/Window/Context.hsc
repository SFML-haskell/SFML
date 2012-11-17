{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Context
(
    createContext
,   destroy
,   setActiveContext
)
where


import SFML.SFResource
import SFML.Window.Types

import Foreign.C.Types

#include <SFML/Window/Context.h>


-- | Create a new context.
-- 
-- This function activates the new context.
createContext = sfContext_create

foreign import ccall unsafe "sfContext_create"
    sfContext_create :: IO Context

-- CSFML_WINDOW_API sfContext* sfContext_create(void);


instance SFResource Context where
    
    {-# INLINABLE destroy #-}
    destroy = sfContext_destroy

foreign import ccall unsafe "sfContext_destroy"
    sfContext_destroy :: Context -> IO ()

-- CSFML_WINDOW_API void sfContext_destroy(sfContext* context);


-- | Activate or deactivate explicitely a context.
setActiveContext :: Context -> Bool -> IO ()
setActiveContext ctx val = sfContext_setActive ctx (fromIntegral . fromEnum $ val)

foreign import ccall unsafe "sfContext_setActive"
    sfContext_setActive :: Context -> CInt -> IO ()

-- CSFML_WINDOW_API void sfContext_setActive(sfContext* context, sfBool active);

