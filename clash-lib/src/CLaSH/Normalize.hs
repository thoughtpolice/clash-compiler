{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Turn CoreHW terms into normalized CoreHW Terms
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module CLaSH.Normalize where

import           Control.Concurrent.Supply        (Supply)
import           Control.Lens                     ((.=),(^.),_1,_3)
import qualified Control.Lens                     as Lens
import           Control.Monad                    (when)
import           Data.Either                      (partitionEithers)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.IntMap.Strict               (IntMap)
import           Data.List                        (mapAccumL,intersect)
import qualified Data.Map                         as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Set                         as Set
import qualified Data.Set.Lens                    as Lens
import           Unbound.Generics.LocallyNameless (unembed)

import           SrcLoc                           (SrcSpan,noSrcSpan)

import           CLaSH.Core.FreeVars              (termFreeIds)
import           CLaSH.Core.Pretty                (showDoc)
import           CLaSH.Core.Subst                 (substTms)
import           CLaSH.Core.Term                  (Term (..), TmName)
import           CLaSH.Core.Type                  (Type, splitCoreFunForallTy)
import           CLaSH.Core.TyCon                 (TyCon, TyConName)
import           CLaSH.Core.Util                  (collectArgs, mkApps, termType)
import           CLaSH.Core.Var                   (Id,varName)
import           CLaSH.Driver.Types               (CLaSHOpts (..))
import           CLaSH.Netlist.BlackBox.Types     (BlackBoxTemplate)
import           CLaSH.Netlist.Types              (HWType)
import           CLaSH.Netlist.Util               (splitNormalized)
import           CLaSH.Normalize.Strategy
import           CLaSH.Normalize.Transformations  (bindConstantVar, caseCon,
                                                   reduceConst, topLet )
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Primitives.Types           (PrimMap)
import           CLaSH.Rewrite.Combinators        ((>->),(!->),repeatR,topdownR)
import           CLaSH.Rewrite.Types              (DebugLevel (..), RewriteEnv (..), RewriteState (..),
                                                   bindings, curFun, dbgLevel,
                                                   tcCache, extra)
import           CLaSH.Rewrite.Util               (isUntranslatableType,
                                                   runRewrite,
                                                   runRewriteSession)
import           CLaSH.Util

-- | Run a NormalizeSession in a given environment
runNormalization :: CLaSHOpts
                 -- ^ Level of debug messages to print
                 -> Supply
                 -- ^ UniqueSupply
                 -> HashMap TmName (Type,SrcSpan,Term)
                 -- ^ Global Binders
                 -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
                 -- ^ Hardcoded Type -> HWType translator
                 -> HashMap TyConName TyCon
                 -- ^ TyCon cache
                 -> IntMap TyConName
                 -- ^ Tuple TyCon cache
                 -> (HashMap TyConName TyCon -> Bool -> Term -> Term)
                 -- ^ Hardcoded evaluator (delta-reduction)
                 -> PrimMap BlackBoxTemplate
                 -- ^ Primitive Definitions
                 -> HashMap TmName Bool
                 -- ^ Map telling whether a components is part of a recursive group
                 -> NormalizeSession a
                 -- ^ NormalizeSession to run
                 -> a
runNormalization opts supply globals typeTrans tcm tupTcm eval primMap rcsMap
  = runRewriteSession rwEnv rwState
  where
    rwEnv     = RewriteEnv
                  (opt_dbgLevel opts)
                  typeTrans
                  tcm
                  tupTcm
                  eval
                  (opt_allowZero opts)

    rwState   = RewriteState
                  0
                  globals
                  supply
                  (error $ $(curLoc) ++ "Report as bug: no curFun",noSrcSpan)
                  0
                  normState

    normState = NormalizeState
                  HashMap.empty
                  Set.empty
                  Map.empty
                  HashMap.empty
                  (opt_specLimit opts)
                  HashMap.empty
                  (opt_inlineLimit opts)
                  (opt_inlineBelow opts)
                  primMap
                  rcsMap

{--
normalize :: [TmName]
          -> NormalizeSession (HashMap TmName (Type,SrcSpan,Term))
normalize []  = return HashMap.empty
normalize top = do
  (new,topNormalized) <- unzip <$> mapM normalize2' top
  newNormalized <- normalize (Set.toList $ Set.unions new)
  return (HashMap.union (HashMap.fromList topNormalized) newNormalized)
--}

normalize :: [TmName]
          -> NormalizeSession (HashMap TmName (Type,SrcSpan,Term))
normalize ts = go ts Set.empty []
  where
    go [] news acc =
      case Set.toList news of
        [] -> return $ HashMap.fromList acc
        vs -> go vs Set.empty acc

    go (x:xs) news acc = do
      (new, topNorm) <- normalize2' x
      go xs (new `Set.union` news) (topNorm:acc)

normalize2' :: TmName
            -> NormalizeSession _
normalize2' nm = fmap (HashMap.lookup nm) (Lens.use bindings) >>= \case
  Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ showDoc nm ++ " not found"
  Just (ty, sp, tm) ->
    hasTranslatableType ty >>= \case
      -- found, but the type cannot be properly translated, so simply find
      -- any other bindings, report an error, and leave
      False -> do
        prevNorm <- getNormalizedBndrs
        let toNormalize = Set.difference (getUsedBndrs tm) (Set.insert nm prevNorm)
        traceNonTranslatableTy nm ty tm
          (return (toNormalize,(nm,(ty,sp,tm))))

      -- the type allows proper translation, so do the business
      True -> do
        tcm <- Lens.view tcCache
        (tyN,spN,tmN) <- cacheNormalized nm $ do
          curFun .= (nm,sp)
          tm' <- rewriteExpr ("normalization", normalization) (showDoc nm, tm)
          ty' <- termType tcm tm'
          return (ty',sp,tm')

        let usedBndrs = getUsedBndrs tmN
        when (nm `elem` usedBndrs) (traceBndrStillRecursive nm tyN tmN)

        prevNorm <- getNormalizedBndrs
        let toNormalize = Set.difference (getUsedBndrs tm) (Set.insert nm prevNorm)
        return (toNormalize,(nm,(tyN,spN,tmN)))

cacheNormalized :: TmName
                -> NormalizeSession (Type, SrcSpan, Term)
                -> NormalizeSession (Type, SrcSpan, Term)
cacheNormalized key create = do
  cache <- Lens.use (extra.normalized)
  case HashMap.lookup key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      (extra.normalized)    Lens.%= HashMap.insert key value
      (extra.normalizedNms) Lens.%= Set.insert key
      return value

traceBndrStillRecursive :: TmName -> Type -> Term -> NormalizeSession ()
traceBndrStillRecursive nm ty tm = do
  traceIf True
    (concat [ $(curLoc),"Expr belonging to bndr: ", showDoc nm, " (:: "
            , showDoc ty
            , ") remains recursive after normalization:\n"
            , showDoc tm
            ])
    (return ())

traceNonTranslatableTy :: TmName -> Type -> Term -> NormalizeSession a -> NormalizeSession a
traceNonTranslatableTy nm ty tm act = do
  lvl <- Lens.view dbgLevel
  traceIf (lvl >= DebugFinal)
          (concat [ $(curLoc), "Expr belonging to bndr: ", showDoc nm, " (:: "
                  , showDoc ty
                  , ") has a non-representable return type."
                  , " Not normalising:\n", showDoc tm
                  ] )
          act

getUsedBndrs :: Term -> Set.Set TmName
getUsedBndrs tm = Lens.setOf termFreeIds tm

getNormalizedBndrs :: NormalizeSession (Set.Set TmName)
getNormalizedBndrs = Lens.use (extra.normalizedNms)

hasTranslatableType :: Type -> NormalizeSession Bool
hasTranslatableType ty = do
  tcm <- Lens.view tcCache
  let (_, resTy) = splitCoreFunForallTy tcm ty
  fmap not (isUntranslatableType resTy)

normalize' :: TmName
           -> NormalizeSession ([TmName],(TmName,(Type,SrcSpan,Term)))
normalize' nm = do
  exprM <- HashMap.lookup nm <$> Lens.use bindings
  let nmS = showDoc nm
  case exprM of
    Just (ty,sp,tm) -> do
      tcm <- Lens.view tcCache
      let (_,resTy) = splitCoreFunForallTy tcm ty
      resTyRep <- not <$> isUntranslatableType resTy
      if resTyRep
         then do
            tmNorm <- makeCached nm (extra.normalized) $ do
                        curFun .= (nm,sp)
                        tm' <- rewriteExpr ("normalization",normalization) (nmS,tm)
                        ty' <- termType tcm tm'
                        return (ty',sp,tm')
            let usedBndrs = Lens.toListOf termFreeIds (tmNorm ^. _3)
            traceIf (nm `elem` usedBndrs)
                    (concat [ $(curLoc),"Expr belonging to bndr: ",nmS ," (:: "
                            , showDoc (tmNorm ^. _1)
                            , ") remains recursive after normalization:\n"
                            , showDoc (tmNorm ^. _3) ])
                    (return ())
            prevNorm <- fmap HashMap.keys $ Lens.use (extra.normalized)
            let toNormalize = filter (`notElem` (nm:prevNorm)) usedBndrs
            return (toNormalize,(nm,tmNorm))
         else do
            let usedBndrs = Lens.toListOf termFreeIds tm
            prevNorm <- fmap HashMap.keys $ Lens.use (extra.normalized)
            let toNormalize = filter (`notElem` (nm:prevNorm)) usedBndrs
            lvl <- Lens.view dbgLevel
            traceIf (lvl >= DebugFinal)
                    (concat [$(curLoc), "Expr belonging to bndr: ", nmS, " (:: "
                            , showDoc ty
                            , ") has a non-representable return type."
                            , " Not normalising:\n", showDoc tm] )
                    (return (toNormalize,(nm,(ty,sp,tm))))
    Nothing -> error $ $(curLoc) ++ "Expr belonging to bndr: " ++ nmS ++ " not found"

-- | Rewrite a term according to the provided transformation
rewriteExpr :: (String,NormRewrite) -- ^ Transformation to apply
            -> (String,Term) -- ^ Term to transform
            -> NormalizeSession Term
rewriteExpr (nrwS,nrw) (bndrS,expr) = do
  lvl <- Lens.view dbgLevel
  let before = showDoc expr
  let expr' = traceIf (lvl >= DebugFinal)
                (bndrS ++ " before " ++ nrwS ++ ":\n\n" ++ before ++ "\n")
                expr
  rewritten <- runRewrite nrwS nrw expr'
  let after = showDoc rewritten
  traceIf (lvl >= DebugFinal)
    (bndrS ++ " after " ++ nrwS ++ ":\n\n" ++ after ++ "\n") $
    return rewritten

-- | Check if the call graph (second argument), starting at the @topEnity@
-- (first argument) is non-recursive. Returns the list of normalized terms if
-- call graph is indeed non-recursive, errors otherwise.
checkNonRecursive :: TmName -- ^ @topEntity@
                  -> HashMap TmName (Type,SrcSpan,Term) -- ^ List of normalized binders
                  -> HashMap TmName (Type,SrcSpan,Term)
checkNonRecursive topEntity norm =
  let cg = callGraph norm topEntity
  in  case mkRecursiveComponents cg of
       []  -> norm
       rcs -> error $ $(curLoc) ++ "Callgraph after normalisation contains following recursive cycles: " ++ show rcs

-- | Perform general \"clean up\" of the normalized (non-recursive) function
-- hierarchy. This includes:
--
--   * Inlining functions that simply \"wrap\" another function
cleanupGraph :: TmName
             -> (HashMap TmName (Type,SrcSpan,Term))
             -> NormalizeSession (HashMap TmName (Type,SrcSpan,Term))
cleanupGraph topEntity norm = do
  let ct = mkCallTree [] norm topEntity
  ctFlat <- flattenCallTree ct
  return (HashMap.fromList $ snd $ callTreeToList [] ctFlat)


data CallTree = CLeaf   (TmName,(Type,SrcSpan,Term))
              | CBranch (TmName,(Type,SrcSpan,Term)) [CallTree]

mkCallTree :: [TmName] -- ^ Visited
           -> HashMap TmName (Type,SrcSpan,Term) -- ^ Global binders
           -> TmName -- ^ Root of the call graph
           -> CallTree
mkCallTree visited bindingMap root = case used of
                            [] -> CLeaf   (root,rootTm)
                            _  -> CBranch (root,rootTm) other
  where
    rootTm = Maybe.fromMaybe (error $ $(curLoc) ++ show root ++ " is not a global binder") $ HashMap.lookup root bindingMap
    used   = Set.toList $ Lens.setOf termFreeIds $ (rootTm ^. _3)
    other  = map (mkCallTree (root:visited) bindingMap) (filter (`notElem` visited) used)

stripArgs :: [TmName]
          -> [Id]
          -> [Either Term Type]
          -> Maybe [Either Term Type]
stripArgs _      (_:_) []   = Nothing
stripArgs allIds []    args = if any mentionsId args
                                then Nothing
                                else Just args
  where
    mentionsId t = not $ null (either (Lens.toListOf termFreeIds) (const []) t
                              `intersect`
                              allIds)

stripArgs allIds (id_:ids) (Left (Var _ nm):args)
      | varName id_ == nm = stripArgs allIds ids args
      | otherwise         = Nothing
stripArgs _ _ _ = Nothing

flattenNode :: CallTree
            -> NormalizeSession (Either CallTree ((TmName,Term),[CallTree]))
flattenNode c@(CLeaf (nm,(_,_,e))) = do
  tcm  <- Lens.view tcCache
  norm <- splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (map varName ids) (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),[]))
        Nothing        -> return (Left c)
    _ -> return (Left c)
flattenNode b@(CBranch (nm,(_,_,e)) us) = do
  tcm  <- Lens.view tcCache
  norm <- splitNormalized tcm e
  case norm of
    Right (ids,[(_,bExpr)],_) -> do
      let (fun,args) = collectArgs (unembed bExpr)
      case stripArgs (map varName ids) (reverse ids) (reverse args) of
        Just remainder -> return (Right ((nm,mkApps fun (reverse remainder)),us))
        Nothing        -> return (Left b)
    _ -> return (Left b)

flattenCallTree :: CallTree
                -> NormalizeSession CallTree
flattenCallTree c@(CLeaf _) = return c
flattenCallTree (CBranch (nm,(ty,sp,tm)) used) = do
  flattenedUsed   <- mapM flattenCallTree used
  (newUsed,il_ct) <- partitionEithers <$> mapM flattenNode flattenedUsed
  let (toInline,il_used) = unzip il_ct
  newExpr <- case toInline of
               [] -> return tm
               _  -> rewriteExpr ("bindConstants",(repeatR (topdownR $ (bindConstantVar >-> caseCon >-> reduceConst))) !-> topdownSucR topLet) (showDoc nm, substTms toInline tm)
  return (CBranch (nm,(ty,sp,newExpr)) (newUsed ++ (concat il_used)))

callTreeToList :: [TmName]
               -> CallTree
               -> ([TmName],[(TmName,(Type,SrcSpan,Term))])
callTreeToList visited (CLeaf (nm,(ty,sp,tm)))
  | nm `elem` visited = (visited,[])
  | otherwise         = (nm:visited,[(nm,(ty,sp,tm))])
callTreeToList visited (CBranch (nm,(ty,sp,tm)) used)
  | nm `elem` visited = (visited,[])
  | otherwise         = (visited',(nm,(ty,sp,tm)):(concat others))
  where
    (visited',others) = mapAccumL callTreeToList (nm:visited) used
