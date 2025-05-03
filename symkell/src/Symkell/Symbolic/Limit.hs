{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Symkell.Symbolic.Limit
  ( limit,
    LimitPoint (..),
    Direction (..),
    ExtendedReal (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Symkell.Symbolic (Expression)
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

-- | Represents the point towards which the limit is taken.
data LimitPoint
  = PositiveInfinity
  | NegativeInfinity
  | RealValue Double -- Assuming we are working with real limits for now
  deriving (Eq, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric LimitPoint

-- | Represents the direction from which the limit is approached.
data Direction
  = FromLeft
  | FromRight
  | Bidirectional
  deriving (Eq, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric Direction

-- | Represents extended real numbers, including infinities and undefined results.
data ExtendedReal
  = NegInfinity
  | PosInfinity
  | Finite Double
  | Undefined -- For cases where the limit does not exist
  | Indeterminate -- For indeterminate forms like 0/0 before resolution
  deriving (Eq, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric ExtendedReal

-- | Computes the limit of an expression.
-- limit :: Expression -> Text -> LimitPoint -> Direction -> ExtendedReal
-- limit expr var limitPoint direction =
-- Implement the limit calculation logic here, starting with bidirectional checks and transformations.
-- For now, we'll leave it undefined.
limit :: Expression -> Text -> LimitPoint -> Direction -> ExtendedReal
limit _ _ _ _ = Undefined -- Placeholder implementation 