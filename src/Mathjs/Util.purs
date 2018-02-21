module Mathjs.Util where

import Prelude (class Eq, class Show, (==), show, (<>))

type Numbers = Array Number

data InvalidSize = InvalidSize Int

instance showInvalidSize :: Show InvalidSize where
    show (InvalidSize x) = "InvalidSize " <> show x

instance eqInvalidSize :: Eq InvalidSize where
    eq (InvalidSize s1) (InvalidSize s2) = s1 == s2
