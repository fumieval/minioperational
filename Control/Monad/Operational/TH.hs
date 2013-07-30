{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.Operational.TH (makeSingletons) where

import Language.Haskell.TH
import Control.Monad.Operational.Class
import Data.Char

import Control.Monad.Trans.State
import Control.Applicative
import Data.List (union)
import Data.Maybe (fromJust)

renameType :: [(Name, Name)] -> Type -> State Int Type
renameType m (VarT n) = case n `lookup` m of
    Just n' -> return $ VarT n'
    Nothing -> do
        i <- get
        modify (+1)
        return $ VarT $ mkName $ "a" ++ show i
renameType m (SigT t k)          = SigT <$> renameType m t <*> pure k
renameType m (AppT l r)          = AppT <$> renameType m l <*> renameType m r
renameType _ t                   = pure t

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
        gen vs conName argTypes resultType = do
            let bodyName = let (b:bs) = nameBase name in mkName (toLower b : bs)
                ref = [(n, mkName ("p" ++ show i)) | (i, n) <- zip [(0::Int)..] (init vs)]
                instr = foldl AppT (ConT name) $ map VarT $ map (fromJust . (`lookup`ref)) (init vs)
                
                m = mkName "m"
                
                ts = evalState (mapM (renameType ref) argTypes) 0
                result = evalState (renameType ref resultType) 0
                
                vars = PlainTV m : map PlainTV (map snd ref `union` tyVars result)
                
                sig = SigD bodyName $ ForallT vars [ClassP ''(:!) [instr, VarT m]]
                    $ foldr (\x y -> AppT ArrowT x `AppT` y) (AppT (VarT m) result) ts
            
            ps <- mapM (newName . ("p"++) . show) [0..length argTypes - 1]

            let body = AppE (VarE 'singleton) $ foldl AppE (ConE conName) (map VarE ps)

            return [sig, FunD bodyName [Clause (map VarP ps) (NormalB body) []]]  

        fromCon vs (ForallC _ [EqualP _ t] (NormalC conName ts)) = gen vs conName (map snd ts) t
        fromCon vs (NormalC conName ts) = gen vs conName (map snd ts) (VarT $ last vs)
        fromCon _ _ = fail "Unsupported data constructor"

        fromTyVarBndr :: TyVarBndr -> Name
        fromTyVarBndr (PlainTV n) = n
        fromTyVarBndr (KindedTV n _) = n