module SFML.Utils
(
    err
,   tagErr
)
where


import Control.Exception (Exception, throwIO)


-- | Run the given IO action and throw an error if it fails.
err :: Exception e => IO (Either e a) -> IO a
err = (either throwIO return =<<)


-- | Potentially tag a 'Maybe' value with an error.
tagErr :: e -> Maybe a -> Either e a
tagErr err Nothing  = Left err
tagErr err (Just x) = Right x

