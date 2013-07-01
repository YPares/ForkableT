# Example

```haskell
import Control.Monad.Trans.ForkableT
import Control.Monad.Trans (liftIO)

-- The 'r' comes from ContT.
process :: ForkableT r IO Int
process = do
    liftIO $ do
      putStrLn $ "The rest will be ran concurrently for each integer between 0 and 5,"
      putStrLn $ "the output will be interlaced, thus illegible."
    x <- forkForEach [0..5]
    liftIO $ putStrLn $ "I received a " ++ show x ++ ". I will just return it."
    return x

runProcess :: IO [Int]
runProcess = runForkableT process

unixStyleExample = runForkableT $ do
    b <- forkForEach [False, True]
    liftIO $ putStrLn $ if b then "I'm the child"
                             else "I'm the father"
```

