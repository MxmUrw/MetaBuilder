module Main where

import Lib

import Development.Shake.FilePath

import Agda.Interaction.Highlighting.LaTeX.ExternalCall

main :: IO ()
main = do
  collection <- readCollection
  build collection

global :: GlobalConfig
global = GlobalConfig
         { buildRelDir = "_build" </> "metabuild"
         , binRelDir   = "_build" </> "bin"
         }

lambdac :: AgdaProjectConfig
lambdac = AgdaProjectConfig
          { sourceRelDir = "source"
          , mainRelFile  = "Frontend" </> "Main.agda"
          , agdaBin_RelFile   = "lambdac"
          , haskellStackTemplateRelDir = "buildsystem" </> "HaskellStack.metabuild-template"
          , agdaAutobuild = True
          }

metabuilder :: HaskellStackProjectConfig
metabuilder = HaskellStackProjectConfig
              { haskellStackBin_RelFile   = "metabuild"
              , haskellStackSource_RelDir = "buildsystem" </> "MetaBuilder"
              , haskellStackAutobuild     = False
              , installGlobal             = True
              }

collection :: Collection
collection =
  Collection
  { globalConfig         = global
  , agdaProject          = Nothing
  , agdaPublishProject   = Nothing
  , haskellStackProjects = []
  }

