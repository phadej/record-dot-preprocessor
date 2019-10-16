{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# LANGUAGE FlexibleContexts #-}
module Optics.HasField (
    field,
    HasField (..),
    view,
    set,
    setFlipped,
    over,
    overFlipped,
    (%),
    LabelOptic (..),
    LabelOptic',
    A_Lens,
    Lens',
    lens,
    mkLens,
    ) where

import GHC.Records.Compat (HasField (..))
import GHC.Records.Extra ()
import GHC.TypeLits       (Symbol)
import Optics.Core        (Lens', lens)
import Optics.Label (LabelOptic (..), LabelOptic')

import Optics.Core (view, set, over, (%), A_Lens, _1)

setFlipped o s a = set o a s
overFlipped o s f = over o f s

mkLens :: (r -> (a -> r, a)) -> Lens' r a
mkLens f = lens (snd . f) (fst . f)
{-# INLINE mkLens #-}

field :: forall (name :: Symbol) r a. HasField name r a => Lens' r a
field = lens (snd . f) (fst . f)
  where
    f :: r -> (a -> r, a)
    f = hasField @name
{-# INLINE field #-}
