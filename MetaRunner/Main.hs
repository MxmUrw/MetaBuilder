
module Main where


import Options.Applicative
import Data.Semigroup ((<>))

import System.Process

data BuildArgs = BuildArgs
  { buildTarget :: [String]
  }

data Cmd =
  CmdBuild BuildArgs
  -- { Cmdbuild :: Command
  -- }



pCmdBuild :: Parser Cmd
pCmdBuild = CmdBuild <$> pBuildArgs
  where
    pBuildArgs :: Parser BuildArgs
    pBuildArgs = BuildArgs <$> many (argument str (metavar "MetaBuilder arguments..."))


pCommand :: Parser Cmd
pCommand = subparser (command "build" (info pCmdBuild (progDesc "Call MetaBuilder.")))

main :: IO ()
main = do
  cmd <- execParser (info (pCommand <**> helper)
                          (fullDesc <> progDesc "I am Meta, I dispatch your commands. For ever at your service!" <> header "Meta"))
  dispatch cmd

dispatch :: Cmd -> IO ()
dispatch (CmdBuild ba) = do
  callProcess "metabuild" ["metabuild"]
  callProcess "metabuild" (buildTarget ba)



