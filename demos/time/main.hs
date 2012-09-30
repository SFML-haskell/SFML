import SFML.System


main = do
    clock <- createClock
    putStrLn $ "Clock at: " ++ show clock
    loop clock 3
    destroyClock clock


loop clock 0 = return ()
loop clock n = do
    time <- getElapsedTime clock
    if asSeconds time >= 1
        then putStrLn "tick" >> restartClock clock >> loop clock (n-1)
        else loop clock n

