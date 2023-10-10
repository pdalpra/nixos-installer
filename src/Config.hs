{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}

module Config (Args (..), getArgs, Paths (..), mkPaths) where

import Control.Monad.Extra
import Data.String.Interpolate
import System.Console.CmdArgs
import System.Directory


data Args = Args
  { machine :: String
  , user :: String
  }
  deriving (Show, Data, Typeable)


data Paths = Paths
  { configDir :: FilePath
  , gpgHome :: FilePath
  , sshHome :: FilePath
  , gpgKeys :: FilePath
  , sshKeys :: FilePath
  }


argsDef :: Args
argsDef =
  Args
    { machine = def &= help "The machine type to install NixOS on"
    , user = def &= help "The user to setup home-manager for"
    }


getArgs :: IO Args
getArgs = cmdArgs argsDef


mkPaths :: FilePath -> FilePath -> IO Paths
mkPaths tempDir home =
  do
    failWhenM (doesDirectoryExist gpgKeys') [i|Could not find the GPG keys directory (#{gpgKeys'})|]
    failWhenM (doesDirectoryExist paths.sshKeys) [i|Could not find the SSH keys directory (#{sshKeys'})|]
    return paths
  where
    failWhenM test message = unlessM test $ fail message
    gpgKeys' = paths.gpgKeys
    sshKeys' = paths.sshKeys
    paths =
      Paths
        { configDir = tempDir <> "nix-config"
        , gpgHome = home <> "/.gnupg"
        , sshHome = home <> "/.ssh"
        , gpgKeys = home <> "/keys/gpg"
        , sshKeys = home <> "/keys/ssh"
        }
