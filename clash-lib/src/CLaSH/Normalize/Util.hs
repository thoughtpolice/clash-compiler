{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utility functions used by the normalisation transformations
-}

{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module CLaSH.Normalize.Util where

import           Control.Lens            ((%=),(^.),_3)
import qualified Control.Lens            as Lens
import           Data.Function           (on)
import qualified Data.Graph              as Graph
import           Data.Graph.Inductive    (Gr,LNode,lsuc,mkGraph,iDom)
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.List               as List
import qualified Data.Maybe              as Maybe
import qualified Data.Set                as Set
import qualified Data.Set.Lens           as Lens
import           Unbound.Generics.LocallyNameless (Fresh, bind, embed, rec)

import           SrcLoc                  (SrcSpan)

import           CLaSH.Core.FreeVars     (termFreeIds)
import           CLaSH.Core.Var          (Var (Id))
import           CLaSH.Core.Term         (Term (..), TmName)
import           CLaSH.Core.Type         (Type)
import           CLaSH.Core.TyCon        (TyCon, TyConName)
import           CLaSH.Core.Util         (collectArgs, isPolyFun)
import           CLaSH.Normalize.Types
import           CLaSH.Rewrite.Types     (bindings,extra)
import           CLaSH.Rewrite.Util      (specialise)
import           CLaSH.Util              (curLoc)

-- | Determine if a function is already inlined in the context of the 'NetlistMonad'
alreadyInlined :: TmName -- ^ Function we want to inline
               -> TmName -- ^ Function in which we want to perform the inlining
               -> NormalizeMonad (Maybe Int)
alreadyInlined f cf = do
  inlinedHM <- Lens.use inlineHistory
  case HashMap.lookup cf inlinedHM of
    Nothing       -> return Nothing
    Just inlined' -> return (HashMap.lookup f inlined')

addNewInline :: TmName -- ^ Function we want to inline
             -> TmName -- ^ Function in which we want to perform the inlining
             -> NormalizeMonad ()
addNewInline f cf =
  inlineHistory %= HashMap.insertWith
                     (\_ hm -> HashMap.insertWith (+) f 1 hm)
                     cf
                     (HashMap.singleton f 1)

-- | Specialize under the Normalization Monad
specializeNorm :: NormRewrite
specializeNorm = specialise specialisationCache specialisationHistory specialisationLimit

-- | Determine if a term is closed
isClosed :: Fresh m
         => HashMap TyConName TyCon
         -> Term
         -> m Bool
isClosed tcm = fmap not . isPolyFun tcm

-- | Determine if a term represents a constant
isConstant :: Term -> Bool
isConstant e = case collectArgs e of
  (Data _, args)   -> all (either isConstant (const True)) args
  (Prim _ _, args) -> all (either isConstant (const True)) args
  (Literal _,_)    -> True
  _                -> False

isRecursiveBndr :: TmName -> NormalizeSession Bool
isRecursiveBndr f = do
  cg <- Lens.use (extra.recursiveComponents)
  case HashMap.lookup f cg of
    Just isR -> return isR
    Nothing -> do
      bndrs <- Lens.use bindings
      let cg'  = callGraph bndrs f
          rcs  = concat $ mkRecursiveComponents cg'
          isR  = f `elem` rcs
          cg'' = HashMap.fromList
               $ map (\(t,_) -> (t,t `elem` rcs)) cg'
      (extra.recursiveComponents) %= HashMap.union cg''
      return isR

-- | Create a call graph for a set of global binders, given a root
callGraph :: HashMap TmName (Type,SrcSpan,Term) -- ^ Global binders
          -> TmName -- ^ Root of the call graph
          -> [(TmName,[TmName])]
callGraph = go []
  where
    go visited bindingMap root = node:other
      where
        rootTm = Maybe.fromMaybe (error $ show root ++ " is not a global binder") $ HashMap.lookup root bindingMap
        used   = Set.toList $ Lens.setOf termFreeIds (rootTm ^. _3)
        node   = (root,used)
        other  = concatMap (go (root:visited) bindingMap) (filter (`notElem` visited) used)

-- | Determine the sets of recursive components given the edges of a callgraph
mkRecursiveComponents :: [(TmName,[TmName])] -- ^ [(calling function,[called function])]
                    -> [[TmName]]
mkRecursiveComponents cg = map (List.sortBy (compare `on` (`List.elemIndex` fs)))
                         . Maybe.catMaybes
                         . map (\case {Graph.CyclicSCC vs -> Just vs; _ -> Nothing})
                         . Graph.stronglyConnComp
                         $ map (\(n,es) -> (n,n,es)) cg
  where
    fs = map fst cg

lambdaDropPrep :: HashMap TmName (Type,SrcSpan,Term)
               -> TmName
               -> HashMap TmName (Type,SrcSpan,Term)
lambdaDropPrep bndrs topEntity = bndrs'
  where
    depGraph = callGraph bndrs topEntity
    used     = HashMap.fromList depGraph
    rcs      = mkRecursiveComponents depGraph
    dropped  = map (lambdaDrop bndrs used) rcs
    bndrs'   = foldr (\(k,v) b -> HashMap.insert k v b) bndrs dropped

lambdaDrop :: HashMap TmName (Type,SrcSpan,Term) -- ^ Original Binders
           -> HashMap TmName [TmName]    -- ^ Dependency Graph
           -> [TmName]                   -- ^ Recursive block
           -> (TmName,(Type,SrcSpan,Term))       -- ^ Lambda-dropped Binders
lambdaDrop bndrs depGraph cyc@(root:_) = block
  where
    doms  = dominator depGraph cyc
    block = blockSink bndrs doms (0,root)

lambdaDrop _ _ [] = error $ $(curLoc) ++ "Can't lambdadrop empty cycle"

dominator :: HashMap TmName [TmName] -- ^ Dependency Graph
          -> [TmName]                -- ^ Recursive block
          -> Gr TmName TmName        -- ^ Recursive block dominator
dominator cfg cyc = mkGraph nodes (map (\(e,b) -> (b,e,nodesM HashMap.! e)) doms)
  where
    nodes    = zip [0..] cyc
    nodesM   = HashMap.fromList nodes
    nodesI   = HashMap.fromList $ zip cyc [0..]
    cycEdges = HashMap.map ( map (nodesI HashMap.!)
                           . filter (`elem` cyc)
                           )
             $ HashMap.filterWithKey (\k _ -> k `elem` cyc) cfg
    edges    = concatMap (\(i,n) -> zip3 (repeat i) (cycEdges HashMap.! n) (repeat ())
                         ) nodes
    graph    = mkGraph nodes edges :: Gr TmName ()
    doms     = iDom graph 0

blockSink :: HashMap TmName (Type,SrcSpan,Term) -- ^ Original Binders
          -> Gr TmName TmName           -- ^ Recursive block dominator
          -> LNode TmName               -- ^ Recursive block dominator root
          -> (TmName,(Type,SrcSpan,Term))       -- ^ Block sank binder
blockSink bndrs doms (nId,tmName) = (tmName,(ty,sp,newTm))
  where
    (ty,sp,tm) = bndrs HashMap.! tmName
    sucTm   = lsuc doms nId
    tmS     = map (blockSink bndrs doms) sucTm
    bnds    = map (\(tN,(ty',_,tm')) -> (Id tN (embed ty'),embed tm')) tmS
    newTm   = case sucTm of
                [] -> tm
                _  -> Letrec (bind (rec bnds) tm)
