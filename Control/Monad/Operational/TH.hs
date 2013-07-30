{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.Operational.TH (makeSingletons) where
import Prelude hiding (mapM)
import Language.Haskell.TH
import Control.Monad.Operational.Class
import Data.Char

import Control.Monad.Trans.State
import Control.Applicative
import qualified Data.Map as Map
import Control.Lens
import Data.List (nub)
import Data.Traversable

renameType :: Map.Map Name Type -> Type -> Type
renameType m (VarT n) = case n `Map.lookup` m of
    Just t -> t
    Nothing -> VarT n
renameType m (SigT t k)          = SigT (renameType m t) k
renameType m (AppT l r)          = AppT (renameType m l) (renameType m r)
renameType _ t                   = t

tyVars :: Type -> [Name]
tyVars (VarT n) = [n]
tyVars (AppT l r) = tyVars l ++ tyVars r
tyVars _ = []

makeSingletons :: Name -> Q [Dec]
makeSingletons name = do
    TyConI dec <- reify name
    case dec of
        DataD _ _ vs cs _ -> fmap concat $ mapM (fromCon (map fromTyVarBndr vs)) cs
        _ -> fail "Expecting a type construcor"
    where
        gen vs_ eqs_ conName argTypes_ resultType_ = do
            let bodyName = let (b:bs) = nameBase conName in mkName (toLower b : bs)
            
            let refresh1 m i = case Map.lookup i m of
                    Just (VarT v) -> v
                    _ -> i

            let ref = Map.fromList [(v, VarT $ mkName $ "v" ++ show i) | (i, v) <- zip [0..] vs_]
            let vs = map (refresh1 ref) vs_
            let resultType = renameType ref resultType_
            let argTypes = map (renameType ref) argTypes_
            let eqs = [EqualP (renameType ref s) (renameType ref t) | EqualP s t <- eqs_]

            let eqm = Map.fromList [(v, t) | EqualP (VarT v) t <- eqs]
            let vs' = map (refresh1 eqm) vs
            let resultType' = renameType eqm resultType
            let argTypes' = map (renameType eqm) argTypes

            let instr = renameType eqm $ foldl AppT (ConT name) $ map VarT (init vs')
            let m = mkName "m"

            let vars = map PlainTV $ (m :) $ nub
                    $ tyVars instr ++ tyVars resultType' ++ concatMap tyVars argTypes'

            let sig = SigD bodyName $ ForallT vars [ClassP ''(:!) [instr, VarT m]]
                    $ foldr (\x y -> AppT ArrowT x `AppT` y) (AppT (VarT m) resultType') argTypes'
            
            ps <- mapM (newName . ("p"++) . show) [0..length argTypes - 1]

            let body = AppE (VarE 'singleton) $ foldl AppE (ConE conName) (map VarE ps)

            return [sig, FunD bodyName [Clause (map VarP ps) (NormalB body) []]]  

        fromCon vs (ForallC _ eqs (NormalC conName ts)) = gen vs eqs conName (map snd ts) (VarT $ last vs)
        fromCon vs (NormalC conName ts) = gen vs [] conName (map snd ts) (VarT $ last vs)
        fromCon _ _ = fail "Unsupported data constructor"

        fromTyVarBndr :: TyVarBndr -> Name
        fromTyVarBndr (PlainTV n) = n
        fromTyVarBndr (KindedTV n _) = n