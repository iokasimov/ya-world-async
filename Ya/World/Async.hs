module Ya.World.Async where

import Ya
import Ya.World

import Data.Either (Either (Left, Right))
import Control.Concurrent.Async (concurrently, race)

pattern Async :: World i `AR__` World `L` World `T` (Void `P` Void) `T` i
pattern Async e = Label e

instance Mapping T'I'II T'I'II (AR) (AR) (Covariant Day (AR) (P) (P) World (World `L` World `T` (Void `P` Void)) i ii) World where
 mapping = rewrap `identity` \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x (Label xx)) (T'I'II f))) ->
  concurrently x xx `yo` (\(x', xx') -> x' `hjd` xx') `ho` f `ho` from

instance Mapping T'I'II T'I'II (AR) (AR) (Covariant Day (AR) (P) (S) World (World `L` World `T` (Void `P` Void)) i ii) World where
 mapping = rewrap `identity` \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x (Label xx)) (T'I'II f))) ->
  race x xx `yo` (\case { Left x' -> This x'; Right xx' -> That xx' }) `ho` f `ho` from
