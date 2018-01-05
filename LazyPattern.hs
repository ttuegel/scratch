{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module LazyPattern where

import Debug.Trace (trace)

data Foo a = Foo a
  deriving (Show)

data Bar a = Bar a
  deriving (Show)

-- | Lazy pattern matching
-- The argument is not evaluated until 'a' is forced.
--
-- >>> lazy (Foo ())
-- Bar right a
-- Foo
-- left a
-- ()
lazy ~(trace "Foo" -> Foo (trace "left a" -> a)) =
    Bar (trace "right a" a)

-- | Ordinary pattern matching
-- The argument is evaluated to weak head normal form before 'Bar' is applied.
--
-- >>> weak (Foo ())
-- Foo
-- Bar right a
-- left a
-- ()
weak (trace "Foo" -> Foo (trace "left a" -> a)) =
    Bar (trace "right a" a)

-- | Strict pattern matching
-- 'a' is evaluated to weak head normal form before 'Bar' is applied.
--
-- >>> strict (Foo ())
-- Foo
-- left a
-- Bar right a
-- ()
strict (trace "Foo" -> Foo (trace "left a" -> !a)) =
    Bar (trace "right a" a)
