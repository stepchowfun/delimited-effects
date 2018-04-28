module Context
  ( Context
  , initialContext
  , intType
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax (EVar(..), TVar(..), Type(..))

-- Contexts can hold term variables and type variables.
type Context = (Map EVar Type, Set TVar)

intTVar :: TVar
intTVar = UserTVar "Int"

intType :: Type
intType = TVar intTVar

initialContext :: Context
initialContext = (Map.empty, Set.fromList [intTVar])
