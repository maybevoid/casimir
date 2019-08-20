{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Implicit.Param
  ( ImplicitParam
  , NamedParam
  , TaggedParam
  , captureParam
  , captureName
  , captureTag
  , withParam
  , withName
  , withTag
  )
where

import Data.Kind (Type)
import GHC.Types (Symbol)
import Unsafe.Coerce (unsafeCoerce)
import Data.Constraint (Dict (..))

class
  ImplicitParam' k (label :: k) a
  | label -> a
  where
    captureParam' :: a

data ParamReflector k (label :: k) a = ParamReflector {
  _reflectParam :: a
}

type ImplicitParam k (label :: k) = ImplicitParam' k label
type NamedParam label = ImplicitParam Symbol label
type TaggedParam label = ImplicitParam Type label

captureParam
  :: forall k (label :: k) a
   . (ImplicitParam k label a)
  => a
captureParam = captureParam' @k @label @a

captureName
  :: forall label a
   . (NamedParam label a)
  => a
captureName = captureParam @Symbol @label

captureTag
  :: forall label a
   . (TaggedParam label a)
  => a
captureTag = captureParam @Type @label

withParam
  :: forall k (label :: k) a r
   . a
  -> ((ImplicitParam k label a) => r)
  -> r
withParam x cont = case dict of Dict -> cont
 where
  dict :: Dict (ImplicitParam k label a)
  dict = unsafeCoerce $ ParamReflector @k @label @a x

withName
  :: forall label a r
   . a
  -> ((NamedParam label a) => r)
  -> r
withName = withParam @Symbol @label @a

withTag
  :: forall label a r
   . a
  -> ((TaggedParam label a) => r)
  -> r
withTag = withParam @Type @label @a