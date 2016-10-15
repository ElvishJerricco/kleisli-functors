kleisli-functors
---

[Based on this blog post](https://elvishjerricco.github.io/2016/10/12/kleisli-functors.html). A
Kleisli functor of `m` is a functor from `Kliesli m` to `Hask`.

```haskell
class (Monad m, Functor f) =>
	  KleisliFunctor m f where
  kmap :: (a -> m b) -> f a -> f b
```

These are functors that are able to absorb the monad. For instance,
lists are a Kleisli functor of `Maybe`, because the list functor can
absorb the `Maybe` monad by removing `Nothing` elements.

```haskell
instance KleisliFunctor Maybe [] where
  kmap _ [] = []
  kmap f (a:as) =
    case f a of
	  -- Remove `Nothing` elements
	  Nothing -> kmap f as
	  -- Keep `Just` elements
	  Just b -> b:kmap f as
```

There are many other ways to use Kleisli functors, concurrency being
my favorite example. Using the `async` package, we get access to a
`Concurrently` type for doing `IO` concurrently. Along with
`Traversable`, this makes for a really useful Kleisli functor.

```haskell
kfor
  :: (Traversable t, KleisliFunctor m f, Applicative f)
  => (a -> m b) -> t a -> f (t b)
kfor t f = for t (kmap f . pure)

instance KleisliFunctor IO Concurrently where
  kmap f (Concurrently a) = Concurrently (a >>= f)

concurrent :: Traversable t => t Int -> IO (t Int)
concurrent t = runConcurrently . kfor t $ \i -> do
  -- This block lives in `IO`,
  -- and will be automatically concurrent with the other iterations.
  ...
  return x
```
