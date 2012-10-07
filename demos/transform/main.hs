import SFML.Graphics.Transform


main = do
    let
        t = translation 10 10
        r = rotation 45
        s = scaling 2 4
    
    printmat' "Translation:" t
    printmat' "Rotation:" r
    printmat' "Scale:" s
    
    let
        t0 = t * r
        i  = inverse t0
        fi = fastInverse t0
        i0 = t0 * i
        i1 = t0 * fi
    
    printmat' "Transform:" t0
    printmat' "Inverse:" i
    printmat' "Fast inverse:" fi
    printmat' "Transform * Inverse:" i0
    printmat' "Transform * Fast Inverse:" i1
    

printmat' title t = do
    putStrLn title
    putStrLn ""
    printmat t
    putStrLn ""


printmat t =
    let
        f = 0.00001
        m00' = if (abs $ m00 t) < f then 0 else m00 t
        m01' = if (abs $ m01 t) < f then 0 else m01 t
        m02' = if (abs $ m02 t) < f then 0 else m02 t
        m10' = if (abs $ m10 t) < f then 0 else m10 t
        m11' = if (abs $ m11 t) < f then 0 else m11 t
        m12' = if (abs $ m12 t) < f then 0 else m12 t
        m20' = if (abs $ m20 t) < f then 0 else m20 t
        m21' = if (abs $ m21 t) < f then 0 else m21 t
        m22' = if (abs $ m22 t) < f then 0 else m22 t
    in do
        putStrLn $ show m00' ++ "\t" ++ show m10' ++ "\t" ++ show m20'
        putStrLn $ show m01' ++ "\t" ++ show m11' ++ "\t" ++ show m21'
        putStrLn $ show m02' ++ "\t" ++ show m12' ++ "\t" ++ show m22'

