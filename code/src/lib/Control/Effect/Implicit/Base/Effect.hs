
module Control.Effect.Implicit.Base.Effect
  ( Effect
  , type (~>)
  )
where

-- | A less scary type alias to `Monad`. We hope that implicit-effects can
-- help Haskell beginners to learn how to better structure effectful programs
-- without first going through all the struggles of understanding the
-- machineries of monad, monad transformers, free monad, extensible effects,
-- etc before they can properly write a "good" effectful program. Instead
-- beginners should be taught from first principles about algebraic effects,
-- and go through something similar to the
-- <https://www.eff-lang.org/handlers-tutorial.pdf Eff language tutorial>,
-- which does a great job explaning algebraic effects without mentioning the
-- M-word.
type Effect = Monad

type eff1 ~> eff2 = forall x . eff1 x -> eff2 x
