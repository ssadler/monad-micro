
module Control.Monad.Micro where







newtype Resource a = Resource { unResource :: forall ret. (a -> IO ret) -> IO ret }

instance Functor Resource where
  fmap f (Resource p) = Resource $
    \rest -> p (rest . f)

instance Applicative Resource where
  pure = return
  (<*>) = ap

instance Monad Resource where
  return x = Resource ($ x)
  Resource f >>= g = Resource $
    \rest -> f (\a -> unResource (g a) rest)

instance MonadUnliftIO Resource where
  withRunInIO inner =
    Resource $ \f -> inner (\(Resource z) -> z pure) >>= f

instance MonadIO Resource where
  liftIO ma = Resource $ \rest -> ma >>= rest

runResource :: Resource a -> IO a
runResource (Resource f) = f pure

allocate :: IO () -> Resource ()
allocate ma = Resource $ \rest -> finally (rest ()) ma

testResource = do
  runResource do
    alloc $ putStrLn "world"
    handle (\e -> liftIO $ print (e :: SomeException)) do
      alloc $ putStrLn "hello"
      error "oh no!"

