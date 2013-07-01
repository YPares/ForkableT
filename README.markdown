# Example

```haskell
import Control.Monad.Trans.ForkableT

process = runForkableT $ do
    liftIO $ putStrLn "The rest will be run concurrently for each integer between 0 and 5. The output will be interlaced, thus illegible."
    x <- forkForeach [0..5]
    liftIO $ putStrLn $ "I received a " ++ show x
```

