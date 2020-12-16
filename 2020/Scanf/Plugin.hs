{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Scanf.Plugin (plugin) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Constraint
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq
import           Data.Traversable


import qualified Class as T
import           Constraint (Ct(..))
import qualified Constraint as T
import qualified FastString as T
import           GhcPlugins (Plugin)
import qualified GhcPlugins as P
import qualified Module as T
import qualified OccName as T
import qualified PrelNames as T
import qualified TcEvidence as T
import           TcPluginM (TcPluginM)
import qualified TcPluginM as P
import           TcRnTypes (TcPluginResult)
import qualified TcRnTypes as T
import           TcType (TcType)
import           TyCon (TyCon)
import qualified TyCon as T
import qualified Type as T
import qualified TysWiredIn as T

import           Scanf.Plugin.Format
import           Scanf.Specifier

showObj obj = P.showSDocUnsafe (P.ppr obj)
display obj = P.tcPluginIO $ putStrLn $ showObj obj

type EnvR = IORef (Maybe Env)
data Env = Env {
  parseTc :: T.TyCon,
  typeErrorTc :: T.TyCon,
  typeErrorTextTc :: T.TyCon,
  specTc :: T.TyCon,
  specTypeKind :: T.Kind,
  specLitTc :: T.TyCon,
  specSpaceTc :: T.TyCon,
  specReadTc :: T.TyCon
  }

initEnv :: TcPluginM Env
initEnv = do
  mod_result <- P.findImportedModule (T.mkModuleName "Scanf.Specifier") Nothing
  let mod = case mod_result of
              P.Found ml m -> m
  parseTc <- P.tcLookupTyCon =<< P.lookupOrig mod (T.mkTcOcc "ParseScanf")
  specTc <- P.tcLookupTyCon =<< P.lookupOrig mod (T.mkTcOcc "Spec")
  specLitTc <- P.promoteDataCon <$> (P.tcLookupDataCon =<< P.lookupOrig mod (T.mkDataOcc "Lit"))
  specReadTc <- P.promoteDataCon <$> (P.tcLookupDataCon =<< P.lookupOrig mod (T.mkDataOcc "SpecRead"))
  specSpaceTc <- P.promoteDataCon <$> (P.tcLookupDataCon =<< P.lookupOrig mod (T.mkDataOcc "SpecSpace"))
  let specTypeKind = T.mkTyConApp specTc [T.typeSymbolKind]
  typeErrorTc <- P.tcLookupTyCon T.errorMessageTypeErrorFamName
  typeErrorTextDc <- P.tcLookupDataCon T.typeErrorTextDataConName
  let typeErrorTextTc = P.promoteDataCon typeErrorTextDc
  return Env{..}

plugin :: Plugin
plugin = P.defaultPlugin {
  P.tcPlugin = const $ Just $ T.TcPlugin {
      T.tcPluginInit = P.tcPluginIO (newIORef Nothing),
      T.tcPluginSolve = solve,
      T.tcPluginStop = const (pure ())
      }
  }

solve :: EnvR -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve envr givens wds wanteds = do
  e@Env{..} <- initEnv
  P.tcPluginIO $ putStrLn $ "====== solve ======="
  for_ wanteds $ \ct -> do
    case ct of
      CFunEqCan{cc_ev, cc_fun, cc_tyargs, cc_fsk}
        | cc_fun == parseTc -> display (cc_ev, cc_fun, cc_tyargs, cc_fsk)
      CDictCan{cc_ev, cc_class, cc_tyargs=[arg1, arg2], cc_pend_sc} ->
        display (arg1, arg2)
      _ -> display ct
  traverse_ display givens
  traverse_ display wds
  traverse_ display wanteds
  solveResult <- runMaybeT (asum $ map (solveWanted e) wanteds)
  case solveResult of
    Just r -> return r
    Nothing -> return (T.TcPluginOk [] [])

type App = MaybeT TcPluginM
type Reduce = StateT Bool (ExceptT String TcPluginM)

fromSpec :: Env -> Spec String -> TcType
fromSpec e@Env{..} (Lit s) = T.mkTyConApp specLitTc [T.typeSymbolKind, T.mkStrLitTy (T.mkFastString s)]
fromSpec e@Env{..} SpecRead = T.mkTyConApp specReadTc [T.typeSymbolKind]
fromSpec e@Env{..} SpecSpace = T.mkTyConApp specSpaceTc [T.typeSymbolKind]

fromSpecs :: Env -> [Spec String] -> TcType
fromSpecs e@Env{..} specs = T.mkPromotedListTy specTypeKind (map (fromSpec e) specs)

reduceParsed :: Env -> String -> Reduce TcType
reduceParsed e@Env{..} fmt = case parseScanf fmt of
  Right specs -> pure (fromSpecs e specs)
  Left msg -> throwError msg

reduceType :: Env -> TcType -> Reduce TcType
reduceType e@Env{..} ty = case T.splitTyConApp_maybe ty of
  Just (tc, [arg])
    | tc == parseTc -> case T.isStrLitTy arg of
        Just fs -> do new_ty <- reduceParsed e (T.unpackFS fs)
                      put True
                      return new_ty
        _ -> return ty
  _ -> return ty
-- case splitAppTys ty of
--   (tc, [fmt]) | tc == parseTc -> case isStrLitTy fmt of

errorMessage :: Env -> String -> TcType
errorMessage Env{..} msg = T.mkTyConApp typeErrorTc [T.mkTyConApp typeErrorTextTc [T.mkStrLitTy (T.mkFastString msg)]]

derived :: T.CtEvidence -> T.EvTerm
derived ev = case T.ctev_dest ev of
  T.EvVarDest var -> T.EvExpr (T.evId var)

solveWanted :: Env -> Ct -> App TcPluginResult
solveWanted e@Env{..} ct = case ct of
  CDictCan{cc_ev, cc_class, cc_tyargs, cc_pend_sc} -> do
    reduceResult <- lift $ runExceptT $ runStateT (traverse (reduceType e) cc_tyargs) False
    case reduceResult of
      (Left msg) -> return (T.TcPluginContradiction [ct])
      (Right (new_args, True)) -> do
        let new_pred = T.mkTyConApp (T.classTyCon cc_class) new_args
        new_ev <- lift (P.newWanted (T.ctev_loc cc_ev) new_pred)
        return (T.TcPluginOk [(derived new_ev, ct)] [CNonCanonical new_ev])
      (Right (_, False)) -> mzero
  _ -> mzero
