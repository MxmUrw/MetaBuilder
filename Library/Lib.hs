
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}

module Lib
  ( Collection(..)
  , AgdaProjectConfig(..)
  , HaskellStackProjectConfig(..)
  , GlobalConfig(..)
  , build
  , readCollection
  )
  where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Exception
import Control.Monad

import Data.Typeable
import Data.Aeson
import qualified Data.Yaml
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath.Find as FP
import System.Environment.Executable

import GHC.Generics

-- my submodules
import AgdaPublish.Common
import qualified AgdaPublish
import qualified AgdaPublish.Highlevel

-- iterateM :: Monad m => Int -> (m a)
-- iterateM n f mx = sequence . take n . iterate (>>= f) $ mx


import Agda.Interaction.Highlighting.LaTeX.ExternalCall
import Agda.Interaction.Highlighting.LaTeX.Prettify


(.>) :: a -> (a -> b) -> b
(.>) x f = f x
infixl 9 .>

data MBException
  = CouldNotFindRootFile
  | FoundMultipleRootFiles
  deriving (Typeable)

instance Show MBException where
  show CouldNotFindRootFile   = "Error: Could not find a .metabuild-root file."
  show FoundMultipleRootFiles = "Error: Found multiple .metabuild-root files in same directory."

instance Exception MBException

filterRoot :: FilePath -> Bool
filterRoot file = takeExtension file == ".metabuild-root"

findProjectRootFile_impl :: FilePath -> IO FilePath
findProjectRootFile_impl cur_dir = do
  files <- listDirectory cur_dir
  let filtered = filter filterRoot files
  case filtered of
    [] -> let parent = takeDirectory cur_dir
          in if (isDrive cur_dir || parent == cur_dir)
             then (throw CouldNotFindRootFile)
             else (findProjectRootFile_impl parent)
    [x] -> (return (cur_dir </> x))
    x:xs -> throw (FoundMultipleRootFiles)

findProjectRootFile :: IO FilePath
findProjectRootFile = do
  getCurrentDirectory >>= findProjectRootFile_impl

-- findProjectRoot :: IO FilePath
-- findProjectRoot = takeDirectory <$> findProjectRootFile

data AgdaPublishProjectConfig = AgdaPublishProjectConfig
  { source_RelDir          :: FilePath
  , include_RelFiles       :: [FilePath]
  , autobuild              :: Bool
  , fastbuild              :: Bool
  , projectName            :: String
  , libraryDefinitions_Filename :: String
  }
  deriving (Generic)
instance FromJSON AgdaPublishProjectConfig
data ExtraAgdaPublishProjectConfig = ExtraAgdaPublishProjectConfig
  {
    source_AbDir                      :: FilePath
  , include_AbFiles                   :: [FilePath]
  , mainPdf_AbFile                    :: FilePath
  , buildPdf_AbFile                   :: FilePath
  , mainTex_AbFile                    :: FilePath
  , originalConfig                    :: AgdaPublishProjectConfig
  , generateLiterate_Source_AbDir     :: FilePath
  , generateLiterate_Target_AbDir     :: FilePath
  , generateTex_Source_AbDir          :: FilePath
  , generateTex_Target_AbDir          :: FilePath
  , generatePdf_Source_AbDir          :: FilePath
  , generateTex_Target_AbFiles        :: [FilePath]
  , libraryDefinitions_Source_AbFile  :: FilePath
  , libraryDefinitions_Target_AbFile  :: FilePath
  , agdaSty_Target_AbFile             :: FilePath
  , commands_AbFile                   :: FilePath
  }

data AgdaProjectConfig = AgdaProjectConfig
  -- all paths should be relative
  { sourceRelDir               :: FilePath
  , mainRelFile                :: FilePath
  , agdaBin_RelFile            :: String
  , haskellStackTemplateRelDir :: FilePath
  , agdaAutobuild              :: Bool
  }
  deriving (Generic)
instance FromJSON AgdaProjectConfig
data ExtraAgdaProjectConfig = ExtraAgdaProjectConfig
  -- absolute versions of paths in `AgdaProjectConfig`
  { transpilationSource_AbDir         :: FilePath
  , transpilationTarget_AbDir         :: FilePath
  , agdaTarget_AbDir                  :: FilePath
  , agdaBin_AbFile                    :: FilePath
  , mainTranspilationSource_AbFile    :: FilePath
  -- derived paths
  , haskellStack_TemplateSource_AbDir :: FilePath
  , haskellStack_TemplateTarget_AbDir :: FilePath
  -- fixed paths
  , ghcshim_AbFile                    :: FilePath
  -- original settings
  , originalAgdaConfig                :: AgdaProjectConfig
  }

data HaskellStackProjectConfig = HaskellStackProjectConfig
  { haskellStackBin_RelFile   :: FilePath
  , haskellStackSource_RelDir :: FilePath
  , haskellStackAutobuild     :: Bool
  , installGlobal             :: Bool
  }
  deriving (Generic)
instance FromJSON HaskellStackProjectConfig
data ExtraHaskellStackProjectConfig = ExtraHaskellStackProjectConfig
  { haskellStackBin_AbFile     :: FilePath
  , haskellStackSource_AbDir   :: FilePath
  -- original settings
  , originalHaskellStackConfig :: HaskellStackProjectConfig
  }

data GlobalConfig = GlobalConfig
  { buildRelDir      :: FilePath
  , binRelDir        :: FilePath
  }
  deriving (Generic)
instance FromJSON GlobalConfig
data ExtraGlobalConfig = ExtraGlobalConfig
  { rootAbDir            :: FilePath
  , root_AbFile          :: FilePath
  , buildAbDir           :: FilePath
  , binAbDir             :: FilePath
  , metabuilder_AbFile    :: FilePath
  , home_AbDir           :: FilePath
  }

data Collection = Collection
  { globalConfig         :: GlobalConfig
  , agdaProject          :: Maybe AgdaProjectConfig
  , agdaPublishProject   :: Maybe AgdaPublishProjectConfig
  , haskellStackProjects :: [HaskellStackProjectConfig]
  } deriving (Generic)
instance FromJSON Collection

data ExtraCollection = ExtraCollection
  { extraGlobalConfig         :: ExtraGlobalConfig
  , extraAgdaProject          :: Maybe ExtraAgdaProjectConfig
  , extraAgdaPublishProject   :: Maybe ExtraAgdaPublishProjectConfig
  , extraHaskellStackProjects :: [ExtraHaskellStackProjectConfig]
  }




deriveExtraProjectConfig_Agda :: ExtraGlobalConfig -> AgdaProjectConfig -> ExtraAgdaProjectConfig
deriveExtraProjectConfig_Agda egpc ap =
  ExtraAgdaProjectConfig
  -- sources:
  { transpilationSource_AbDir         = egpc.>rootAbDir </> ap.>sourceRelDir
  , mainTranspilationSource_AbFile    = egpc.>rootAbDir </> ap.>sourceRelDir </> ap.>mainRelFile
  , haskellStack_TemplateSource_AbDir = egpc.>rootAbDir </> ap.>haskellStackTemplateRelDir
  -- targets:
  , agdaTarget_AbDir                  = egpc.>buildAbDir </> ap.>haskellStackTemplateRelDir </> "src"
  , transpilationTarget_AbDir         = egpc.>buildAbDir </> ap.>haskellStackTemplateRelDir </> "src" </> "MAlonzo" </> "Code"
  , haskellStack_TemplateTarget_AbDir = egpc.>buildAbDir </> ap.>haskellStackTemplateRelDir
  , agdaBin_AbFile                    = egpc.>binAbDir </> ap.>agdaBin_RelFile <.> exe
  -- fixed paths:
  , ghcshim_AbFile                    = egpc.>buildAbDir </> "ghcshim" </> "ghc"
  , originalAgdaConfig                = ap
  }

deriveExtraProjectConfig_AgdaPublish :: ExtraGlobalConfig -> AgdaPublishProjectConfig -> ExtraAgdaPublishProjectConfig
deriveExtraProjectConfig_AgdaPublish egc appc =
  let buildLiterateRoot = egc.>buildAbDir </> appc.>projectName </> "build-literate"
      buildLiterate = buildLiterateRoot </> appc.>source_RelDir
      buildTex     = egc.>buildAbDir </> appc.>projectName </> "build-tex"
      bin          = egc.>binAbDir

      source_AbDir = egc.>rootAbDir </> appc.>source_RelDir
      include_AbFiles = (source_AbDir </>) <$> appc.>include_RelFiles

      generateLiterate_Source_AbDir = egc.>rootAbDir -- source_AbDir --
      generateLiterate_Target_AbDir = buildLiterateRoot
      generateTex_Source_AbDir     = buildLiterate
      generateTex_Target_AbDir     = buildTex
      generatePdf_Source_AbDir     = buildTex

      generateTex_Target_AbFiles = ((\f -> generateTex_Target_AbDir </> appc.>source_RelDir </> f -<.> ".tex") <$> (appc.>include_RelFiles))

      libraryDefinitions_Source_AbFile = egc.>rootAbDir </> appc.>libraryDefinitions_Filename
      libraryDefinitions_Target_AbFile = buildLiterateRoot </> appc.>libraryDefinitions_Filename

      agdaSty_Target_AbFile = buildTex </> "agda.sty"

      commands_AbFile                   = egc.>buildAbDir </> "generated" </> "all.metabuild-cmd"

  in ExtraAgdaPublishProjectConfig
  { source_AbDir     = source_AbDir
  , include_AbFiles  = include_AbFiles
  , mainTex_AbFile   = generateTex_Target_AbDir </> appc.>projectName <.> "tex"
  , mainPdf_AbFile   = bin   </> appc.>projectName <.> "pdf"
  , buildPdf_AbFile  = buildTex </> appc.>projectName <.> "pdf"
  , generateLiterate_Source_AbDir = generateLiterate_Source_AbDir
  , generateLiterate_Target_AbDir = generateLiterate_Target_AbDir
  , generateTex_Source_AbDir = generateTex_Source_AbDir
  , generateTex_Target_AbDir = generateTex_Target_AbDir
  , generatePdf_Source_AbDir = generatePdf_Source_AbDir
  , generateTex_Target_AbFiles = generateTex_Target_AbFiles
  , libraryDefinitions_Source_AbFile = libraryDefinitions_Source_AbFile
  , libraryDefinitions_Target_AbFile = libraryDefinitions_Target_AbFile
  , agdaSty_Target_AbFile = agdaSty_Target_AbFile
  , originalConfig = appc
  , commands_AbFile = commands_AbFile
  }

deriveExtraProjectConfig_HaskellStack :: ExtraGlobalConfig -> HaskellStackProjectConfig -> ExtraHaskellStackProjectConfig
deriveExtraProjectConfig_HaskellStack egpc hpc =
  let haskellStackBin_AbFile   = if hpc.>installGlobal
                                    then (egpc.>home_AbDir) </> ".local" </> "bin"  </> hpc.>haskellStackBin_RelFile <.> exe
                                    else egpc.>binAbDir </> hpc.>haskellStackBin_RelFile <.> exe
      haskellStackSource_AbDir = egpc.>rootAbDir </> hpc.>haskellStackSource_RelDir
  in ExtraHaskellStackProjectConfig
     { haskellStackBin_AbFile     = haskellStackBin_AbFile
     , haskellStackSource_AbDir    = haskellStackSource_AbDir
     , originalHaskellStackConfig  = hpc
     }

deriveExtraProjectConfig_Global :: FilePath -> GlobalConfig -> IO ExtraGlobalConfig
deriveExtraProjectConfig_Global rootfile gpc = do
  let root = takeDirectory rootfile
  let buildAbDir = root </> gpc.>buildRelDir
  let binAbDir   = root </> gpc.>binRelDir
  metabuilder_AbFile <- getExecutablePath
  home_AbDir <- getHomeDirectory
  return ExtraGlobalConfig
     { rootAbDir  = root
     , root_AbFile = rootfile
     , buildAbDir = buildAbDir
     , binAbDir   = binAbDir
     , metabuilder_AbFile = metabuilder_AbFile
     , home_AbDir = home_AbDir
     }

deriveExtraCollection :: FilePath -> Collection -> IO ExtraCollection
deriveExtraCollection root c = do
  extraGlobalConfig <- deriveExtraProjectConfig_Global root (c.>globalConfig)
  let extraAgdaProject  = deriveExtraProjectConfig_Agda extraGlobalConfig <$> (c.>agdaProject)
  let extraAgdaPublishProject  = deriveExtraProjectConfig_AgdaPublish extraGlobalConfig <$> (c.>agdaPublishProject)
  let extraHaskellStackProjects = deriveExtraProjectConfig_HaskellStack extraGlobalConfig <$> (c.>haskellStackProjects)
  return ExtraCollection
     { extraGlobalConfig         = extraGlobalConfig
     , extraAgdaProject          = extraAgdaProject
     , extraAgdaPublishProject   = extraAgdaPublishProject
     , extraHaskellStackProjects = extraHaskellStackProjects
     }

makeRules_HaskellStackProject :: ExtraGlobalConfig -> ExtraHaskellStackProjectConfig -> Rules ()
makeRules_HaskellStackProject egpc ehc = do
  if (ehc.>originalHaskellStackConfig.>haskellStackAutobuild)
    then want [ehc.>haskellStackBin_AbFile]
    else return ()

  phony (ehc.>originalHaskellStackConfig.>haskellStackBin_RelFile) $ do
    need [ehc.>haskellStackBin_AbFile]

  haskellStack_Files <- liftIO $ getDirectoryFilesIO (ehc.>haskellStackSource_AbDir) ["//*.hs", "//*.yaml", "//*.cabal", "//*.metabuild-template"]
  let haskellStackSource_Files = ((ehc.>haskellStackSource_AbDir </>) <$> haskellStack_Files)

  ehc.>haskellStackBin_AbFile %> \_ -> do
    need haskellStackSource_Files
    cmd_ "stack" (Cwd (ehc.>haskellStackSource_AbDir)) ["install", "--local-bin-path=" ++ (dropFileName (ehc.>haskellStackBin_AbFile))]



makeRules_AgdaProject :: ExtraGlobalConfig -> ExtraAgdaProjectConfig -> Rules ()
makeRules_AgdaProject egpc eapc = do
  if (eapc.>originalAgdaConfig.>agdaAutobuild)
    then want [eapc.>agdaBin_AbFile]
    else return ()

  phony (eapc.>originalAgdaConfig.>agdaBin_RelFile) $ do
    need [eapc.>agdaBin_AbFile]

  haskellStack_Template_Files <- liftIO $ getDirectoryFilesIO (eapc.>haskellStack_TemplateSource_AbDir) ["//*.hs", "//*.yaml"]
  let filtered_haskellStack_Template_Files = filter (\f -> not (elem ".stack-work" (splitDirectories f))) haskellStack_Template_Files
  let haskellStack_TemplateSource_Files = ((eapc.>haskellStack_TemplateSource_AbDir </>) <$> filtered_haskellStack_Template_Files)
  let haskellStack_TemplateTarget_Files = ((eapc.>haskellStack_TemplateTarget_AbDir </>) <$> filtered_haskellStack_Template_Files)

  transpilation_Files <- liftIO $ getDirectoryFilesIO (eapc.>transpilationSource_AbDir) ["//*.agda"]
  let transpilationSource_Files = ((\f -> eapc.>transpilationSource_AbDir </> f)            <$> transpilation_Files)
  let transpilationTarget_Files = ((\f -> eapc.>transpilationTarget_AbDir </> f -<.> ".hs") <$> transpilation_Files)

  eapc.>agdaBin_AbFile %> \a -> do
    need (transpilationTarget_Files ++ haskellStack_TemplateTarget_Files)
    cmd_ "stack" (Cwd (eapc.>haskellStack_TemplateTarget_AbDir)) ["install", "--local-bin-path=" ++ egpc.>binAbDir]

  transpilationTarget_Files &%> \files -> do
    need transpilationSource_Files
    need [eapc.>ghcshim_AbFile]
    let ghc_shimpath = takeDirectory (eapc.>ghcshim_AbFile)
    cmd_ "agda" (AddPath [ghc_shimpath] []) ["--compile", "--compile-dir=" ++ eapc.>agdaTarget_AbDir, eapc.>mainTranspilationSource_AbFile]

  (eapc.>haskellStack_TemplateTarget_AbDir ++ "//*") %> \file -> do
    let relfile = makeRelative (eapc.>haskellStack_TemplateTarget_AbDir) file
    let sourceFile = eapc.>haskellStack_TemplateSource_AbDir </> relfile
    let targetFile = eapc.>haskellStack_TemplateTarget_AbDir </> relfile
    putInfo $ "Copying " ++ relfile ++ " from " ++ eapc.>haskellStack_TemplateSource_AbDir
    copyFile' sourceFile targetFile

  (eapc.>ghcshim_AbFile) %> \file -> do
    putInfo "Generating ghc shim"
    liftIO $ writeFile file "#!/bin/bash\n echo \"==== executing shim ====\""
    perm <- liftIO $ getPermissions file
    let perm2 = setOwnerExecutable True perm
    liftIO $ setPermissions file perm2

makeRules_AgdaPublishProject :: ExtraGlobalConfig -> ExtraAgdaPublishProjectConfig -> Rules ()
makeRules_AgdaPublishProject egc eappc = do
  if (eappc.>originalConfig.>autobuild)
    then want [eappc.>mainPdf_AbFile]
    else return ()


  eappc.>mainPdf_AbFile %> \file -> do
    copyFile' (eappc.>buildPdf_AbFile) (eappc.>mainPdf_AbFile)

  eappc.>buildPdf_AbFile %> \_ -> do
    -- Generate list of all agda files
    source_Files <- getDirectoryFiles (eappc.>source_AbDir) ["//*.agda"]
    -- Turn them into a list of lagda files which need to be generated.
    -- We need to include them as dependencies in case of them not being included in the tex output,
    -- but being needed for successfull agda processing
    let generateTex_Source_Files = ((\f -> eappc.>generateTex_Source_AbDir </> f -<.> "lagda")            <$> source_Files)

    let deps = [eappc.>mainTex_AbFile , eappc.>agdaSty_Target_AbFile] ++ (eappc.>generateTex_Target_AbFiles) ++ generateTex_Source_Files
    need deps

    let build = cmd_ "xelatex" (Cwd (eappc.>generatePdf_Source_AbDir)) [eappc.>mainTex_AbFile]
    case (eappc.>originalConfig.>fastbuild) of
      True  -> build
      False -> build >> build

  eappc.>mainTex_AbFile %> \file -> do
    template <- liftIO AgdaPublish.templatefileMainTex
    need [template, egc.>metabuilder_AbFile, egc.>root_AbFile]
    content <- liftIO $ AgdaPublish.generateMainTex (eappc.>generateTex_Target_AbFiles)
    liftIO $ TIO.writeFile file content

  (eappc.>generateLiterate_Target_AbDir ++ "//*.lagda") %> \file -> do
    let relfile = makeRelative (eappc.>generateLiterate_Target_AbDir) file
    let sourcefile = eappc.>generateLiterate_Source_AbDir </> relfile -<.> ".agda"
    let targetfile = eappc.>generateLiterate_Target_AbDir </> relfile -<.> ".lagda"
    need [sourcefile, egc.>metabuilder_AbFile, eappc.>commands_AbFile]
    putInfo $ "Generating literate file " ++ targetfile ++ " for " ++ sourcefile

    -- read input file
    content <- liftIO $ TIO.readFile sourcefile

    -- load command file for code snippet prettification
    commands <- liftIO $ Data.Yaml.decodeFileThrow (eappc.>commands_AbFile)

    content2 <- liftIO $ assumeRight $ (AgdaPublish.generateLiterate commands content)
    liftIO $ TIO.writeFile targetfile content2

  (eappc.>libraryDefinitions_Target_AbFile) %> \file -> do
    copyFile' (eappc.>libraryDefinitions_Source_AbFile) (eappc.>libraryDefinitions_Target_AbFile)

  (eappc.>agdaSty_Target_AbFile) %> \file -> do
    template <- liftIO AgdaPublish.templatefileAgdaSty
    need [template, egc.>metabuilder_AbFile]
    content <- liftIO (AgdaPublish.generateAgdaSty)
    liftIO $ TIO.writeFile file content

  (eappc.>commands_AbFile) %> \file -> do
    source_Files <- getDirectoryFiles (eappc.>source_AbDir) ["//*.agda"]
    let source_AbFiles = [eappc.>source_AbDir </> f | f <- source_Files]
    need ([egc.>metabuilder_AbFile])
    contents <- liftIO $ mapM (TIO.readFile) source_AbFiles
    commands <- liftIO $ assumeRight $ AgdaPublish.generateCommands contents
    putInfo $ "Generating commands file. Found commands: " <> show commands
    liftIO $ Data.Yaml.encodeFile file commands


  (eappc.>generateTex_Target_AbDir ++ "//*.tex") %> \file -> do
    let relfile = makeRelative (eappc.>generateTex_Target_AbDir) file
    let sourcefile = eappc.>generateLiterate_Target_AbDir </> relfile -<.> ".lagda"
    let targetfile = eappc.>generateTex_Target_AbDir </> relfile -<.> ".tex"
    need [sourcefile , eappc.>libraryDefinitions_Target_AbFile , eappc.>commands_AbFile]
    -- need generateTex_Source_Files -- we want all files, such that not -in tex- included files also get translated into literal ones
    putInfo $ "Generating tex file " ++ targetfile ++ " for " ++ sourcefile
    -- putInfo $ ">>> My source dir is: " ++ (eappc.>generateTex_Source_AbDir)
    let targetDir = eappc.>generateTex_Target_AbDir
    liftIO $ createDirectoryIfMissing True targetDir

    let fastbuildarg = if (eappc.>originalConfig.>fastbuild)
          then ["--only-scope-checking"]
          else []

    -- load command file for prettifier
    commands <- liftIO $ Data.Yaml.decodeFileThrow (eappc.>commands_AbFile)

    -- instantiate the prettifier
    let prtf = Prettifier (AgdaPublish.Highlevel.parseAndGenerate commands) (AgdaPublish.Common.prettyChars)

    -- call latex generation in the agda library
    liftIO $ generatePrettyLatexIO prtf sourcefile targetDir


    -- cmd_ "agda" (Cwd (eappc.>generateTex_Source_AbDir)) (["--latex"] ++ fastbuildarg ++ ["--latex-dir=" ++ targetDir, sourcefile])


  -- source_Files <- liftIO $ getDirectoryFilesIO (eapc.>source_AbDir) ["//*.agda"]
  -- let generateLiterateSource_Files = ((\f -> eapc.>generateLiterateSource_AbDir </> f)            <$> source_Files)
  -- let generateLiterateTarget_Files = ((\f -> eapc.>generateLiterateTarget_AbDir </> f)            <$> source_Files)

  -- let generateTexSource_Files = ((\f -> eapc.>generateTexSource_AbDir </> f -<.> ".lagda.tex") <$> source_Files)
  -- let generateTexSource_Files = ((\f -> eapc.>generateTexTarget_AbDir </> f -<.> ".lagda.tex") <$> source_Files)

  -- generateLiterateTarget_Files &%> \files -> do
  --   need generateLiterateSource_Files

  return ()
    where 
        changeTokens :: forall a. TokenLike a => a -> a
        changeTokens = id --  \a -> let t = getTokenText a in setTokenText a (t <> T.pack "AA")


makeRules_Clean :: ExtraGlobalConfig -> Rules ()
makeRules_Clean epc = do
  phony "clean" $ do
    putInfo $ "Cleaning files in " ++ epc.>buildAbDir
    removeFilesAfter (epc.>buildAbDir) ["//*"]
    putInfo $ "Cleaning files in " ++ epc.>binAbDir
    removeFilesAfter (epc.>binAbDir) ["//*"]

readCollection :: IO Collection
readCollection = do
  rootFile <- findProjectRootFile
  Data.Yaml.decodeFileThrow rootFile


build :: Collection -> IO ()
build c = do

  rootfile <- findProjectRootFile
  ec <- deriveExtraCollection rootfile c

  shakeArgs shakeOptions{shakeFiles=ec.>extraGlobalConfig.>buildAbDir, shakeThreads=1} $ do
    mapM_ (makeRules_AgdaProject (ec.>extraGlobalConfig)) (ec.>extraAgdaProject)
    mapM_ (makeRules_AgdaPublishProject (ec.>extraGlobalConfig)) (ec.>extraAgdaPublishProject)
    mapM_ (makeRules_HaskellStackProject (ec.>extraGlobalConfig)) (ec.>extraHaskellStackProjects)
    makeRules_Clean (ec.>extraGlobalConfig)

