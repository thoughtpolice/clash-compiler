{-|
  Copyright   :  (C) 2012-2016, University of Twente, 2017, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Module that connects all the parts of the CLaSH compiler library
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module CLaSH.Driver where

import qualified Control.Concurrent.Supply        as Supply
import           Control.DeepSeq
import           Control.Monad                    (when, unless)
import           Control.Monad.State              (evalState, get)
import qualified Data.HashMap.Lazy                as HML
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HashSet
import           Data.IntMap                      (IntMap)
import           Data.Maybe                       (fromMaybe)
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as Text
import qualified Data.Time.Clock                  as Clock
import qualified System.Directory                 as Directory
import           System.FilePath                  ((</>), (<.>))
import qualified System.FilePath                  as FilePath
import qualified System.IO                        as IO
import           Text.PrettyPrint.Leijen.Text     (Doc, hPutDoc)
import           Unbound.Generics.LocallyNameless (name2String)

import           GHC.Extra                        ()

import           CLaSH.Annotations.TopEntity      (TopEntity (..))
import           CLaSH.Backend
import           CLaSH.Core.Term                  (Term, TmName)
import           CLaSH.Core.Type                  (Type)
import           CLaSH.Core.TyCon                 (TyCon, TyConName)
import           CLaSH.Driver.TestbenchGen
import           CLaSH.Driver.TopWrapper
import           CLaSH.Driver.Types
import           CLaSH.Netlist                    (genComponentName, genNetlist)
import           CLaSH.Netlist.BlackBox.Parser    (runParse)
import           CLaSH.Netlist.BlackBox.Types     (BlackBoxTemplate)
import           CLaSH.Netlist.Types              (Component (..), HWType)
import           CLaSH.Normalize                  (checkNonRecursive, cleanupGraph,
                                                   normalize, runNormalization)
import           CLaSH.Normalize.Util             (callGraph, mkRecursiveComponents)
import           CLaSH.Primitives.Types
import           CLaSH.Util                       (first, second)

-- | Create a set of target HDL files for a set of functions
generateHDL :: forall backend . Backend backend
            => BindingMap -- ^ Set of functions
            -> Maybe backend
            -> PrimMap (Text.Text) -- ^ Primitive / BlackBox Definitions
            -> HashMap TyConName TyCon -- ^ TyCon cache
            -> IntMap TyConName -- ^ Tuple TyCon cache
            -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType)) -- ^ Hardcoded 'Type' -> 'HWType' translator
            -> (HashMap TyConName TyCon -> Bool -> Term -> Term) -- ^ Hardcoded evaluator (delta-reduction)
            -> (TmName,Maybe TopEntity) -- ^ topEntity bndr + (maybe) TopEntity annotation
            -> Maybe TmName -- ^ testInput bndr
            -> Maybe TmName -- ^ expectedOutput bndr
            -> CLaSHOpts -- ^ Debug information level for the normalization process
            -> (Clock.UTCTime,Clock.UTCTime)
            -> IO ()
generateHDL bindingsMap hdlState primMap tcm tupTcm typeTrans eval (topEntity,annM) testInpM expOutM opts (startTime,prepTime) = do
  let primMap' = (HM.map parsePrimitive :: PrimMap Text.Text -> PrimMap BlackBoxTemplate) primMap

  (supplyN,supplyTB) <- Supply.splitSupply
                      . snd
                      . Supply.freshId
                     <$> Supply.newSupply

  let doNorm     = do norm <- normalize [topEntity]
                      let normChecked = checkNonRecursive topEntity norm
                      cleanupGraph topEntity normChecked
      cg         = callGraph bindingsMap topEntity
      rcs        = concat $ mkRecursiveComponents cg
      rcsMap     = HML.fromList
                 $ map (\(t,_) -> (t,t `elem` rcs)) cg
      transformedBindings = runNormalization opts supplyN bindingsMap typeTrans tcm tupTcm eval primMap' rcsMap doNorm

  normTime <- transformedBindings `deepseq` Clock.getCurrentTime
  let prepNormDiff = Clock.diffUTCTime normTime prepTime
  putStrLn $ "Normalisation took " ++ show prepNormDiff

  let modName   = takeWhile (/= '.') (name2String topEntity)
      iw        = opt_intWidth opts
      hdlsyn    = opt_hdlSyn opts
      hdlState' = setModName modName
                $ fromMaybe (initBackend iw hdlsyn :: backend) hdlState
      mkId      = evalState mkBasicId hdlState'
      topNm     = maybe (mkId (Text.pack $ modName ++ "_topEntity"))
                        (Text.pack . t_name)
                        annM

  (netlist,dfiles,seen) <- genNetlist transformedBindings primMap' tcm
                                 typeTrans Nothing modName [] iw mkId [topNm] topEntity

  netlistTime <- netlist `deepseq` Clock.getCurrentTime
  let normNetDiff = Clock.diffUTCTime netlistTime normTime
  putStrLn $ "Netlist generation took " ++ show normNetDiff

  let topComponent = head
                   $ filter (\(_,Component cName _ _ _ _) ->
                                Text.isSuffixOf (genComponentName [topNm] mkId modName topEntity)
                                  cName)
                            netlist

  (testBench,dfiles') <- genTestBench opts supplyTB primMap'
                             typeTrans tcm tupTcm eval mkId seen bindingsMap
                             testInpM
                             expOutM
                             modName
                             dfiles
                             (snd topComponent)


  testBenchTime <- testBench `seq` Clock.getCurrentTime
  let netTBDiff = Clock.diffUTCTime testBenchTime netlistTime
  putStrLn $ "Testbench generation took " ++ show netTBDiff

  let topWrapper = mkTopWrapper primMap' mkId annM modName iw (snd topComponent)
      hdlDocs = createHDL hdlState' modName ((noSrcSpan,topWrapper) : netlist ++ testBench)
      dir = fromMaybe "." (opt_hdlDir opts) </>
            CLaSH.Backend.name hdlState' </>
            takeWhile (/= '.') (name2String topEntity)
  prepareDir (opt_cleanhdl opts) (extension hdlState') dir
  mapM_ (writeHDL dir) hdlDocs
  copyDataFiles (opt_importPaths opts) dir dfiles'

  endTime <- hdlDocs `seq` Clock.getCurrentTime
  let startEndDiff = Clock.diffUTCTime endTime startTime
  putStrLn $ "Total compilation took " ++ show startEndDiff

parsePrimitive :: Primitive Text -> Primitive BlackBoxTemplate
parsePrimitive (BlackBox pNm libM imps inc templT) =
  let (templ,err) = either (first Left . runParse) (first Right . runParse) templT
      inc'        = case fmap (second runParse) inc of
                      Just (x,(t,[])) -> Just (x,t)
                      _ -> Nothing
  in  case err of
        [] -> BlackBox pNm libM imps inc' templ
        _  -> error $ "Errors in template for: " ++ show pNm ++ ":\n" ++ show err
parsePrimitive (Primitive pNm typ) = Primitive pNm typ

-- | Pretty print Components to HDL Documents
createHDL :: Backend backend
           => backend     -- ^ Backend
           -> String
           -> [(SrcSpan,Component)] -- ^ List of components
           -> [(String,Doc)]
createHDL backend modName components = flip evalState backend $ do
  -- (hdlNms,hdlDocs) <- unzip <$> mapM genHDL components
  -- let hdlNmDocs = zip hdlNms hdlDocs
  (hdlNmDocs,incs) <- unzip <$> mapM (uncurry (genHDL modName)) components
  hwtys <- HashSet.toList <$> extractTypes <$> get
  typesPkg <- mkTyPackage modName hwtys
  let hdl   = map (first (<.> CLaSH.Backend.extension backend)) (typesPkg ++ hdlNmDocs)
      qincs = map (first (<.> "qsys")) (concat incs)
  return (hdl ++ qincs)

-- | Prepares the directory for writing HDL files. This means creating the
--   dir if it does not exist and removing all existing .hdl files from it.
prepareDir :: Bool -- ^ Remove existing HDL files
           -> String -- ^ File extension of the HDL files.
           -> String
           -> IO ()
prepareDir cleanhdl ext dir = do
  -- Create the dir if needed
  Directory.createDirectoryIfMissing True dir
  -- Clean the directory when needed
  when cleanhdl $ do
    -- Find all HDL files in the directory
    files <- Directory.getDirectoryContents dir
    let to_remove = filter ((==ext) . FilePath.takeExtension) files
    -- Prepend the dirname to the filenames
    let abs_to_remove = map (FilePath.combine dir) to_remove
    -- Remove the files
    mapM_ Directory.removeFile abs_to_remove

-- | Writes a HDL file to the given directory
writeHDL :: FilePath -> (String, Doc) -> IO ()
writeHDL dir (cname, hdl) = do
  handle <- IO.openFile (dir </> cname) IO.WriteMode
  hPutDoc handle hdl
  IO.hPutStr handle "\n"
  IO.hClose handle

copyDataFiles :: [FilePath] -> FilePath -> [(String,FilePath)] -> IO ()
copyDataFiles idirs dir = mapM_ (copyFile' idirs)
  where
    copyFile' dirs (nm,old) = do
      oldExists <- Directory.doesFileExist old
      if oldExists
        then Directory.copyFile old new
        else goImports dirs
      where
        new = dir FilePath.</> nm

        goImports [] = do
          oldExists <- Directory.doesFileExist old
          if oldExists
            then Directory.copyFile old new
            else unless (null old) (putStrLn ("WARNING: file " ++ show old ++ " does not exist"))
        goImports (d:ds) = do
          let old2 = d FilePath.</> old
          old2Exists <- Directory.doesFileExist old2
          if old2Exists
            then Directory.copyFile old2 new
            else goImports ds
