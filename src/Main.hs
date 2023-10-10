{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Config
import Control.Monad
import Control.Monad.Extra
import Crypto.Gpgme
import Data.Foldable
import Data.List
import Data.String.Interpolate
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process


newtype Email = Email String deriving (Eq, Show)


nixRepo :: String
nixRepo = "git@github.com:pdalpra/nix-config.git"


main :: IO ()
main =
  do
    args <- getArgs
    banner "Check requirements"
    _ <- findExecutable "git"
    _ <- findExecutable "git-secret"

    withSystemTempDirectory "installer" $ \tempDir ->
      do
        home <- getHomeDirectory
        paths <- mkPaths tempDir home

        {-
        Import SSH and GPG keys:
          * SSH keys -> used to clone the Nix configuration
          * GPG keys -> used to decrypt secrets from git-secret
        Both will be copied after install to the home directory
        -}
        banner "Import SSH keys"
        importSshKeys paths
        banner "Import GPG keys"
        emails <- withCtx paths.gpgHome "C" OpenPGP $ importGpgKeys paths.gpgHome

        {-
        Git repository setup:
          * Clone the repository
          * Validate imported keys match what is known by git-secret
          * Decrypt using git-secret
        -}
        banner [i|Setting up Nix configuration repository\nSource #{nixRepo}|]
        callCommand [i|git clone #{nixRepo} #{configDir paths}|]
        decryptSecrets paths emails

        banner "Format disks"
        return ()


importSshKeys :: Paths -> IO ()
importSshKeys config =
  do
    createDirectoryIfMissing True config.sshHome
    files <- listDirectory config.sshKeys
    let copyKey source = copyFile source (config.sshHome </> takeFileName source)
    traverse_ copyKey files


importGpgKeys :: FilePath -> Ctx -> IO [Email]
importGpgKeys keysPath ctx =
  do
    toImport <- getDirectoryContents keysPath
    traverse_ importKey toImport
    keys <- listKeys ctx WithSecret
    uids <- concat <$> traverse keyUserIds' keys
    return $ Email . userEmail . keyuserId <$> uids
  where
    importKey key = importKeyFromFile ctx key >>= flip whenJust (fail . show)


decryptSecrets :: Paths -> [Email] -> IO ()
decryptSecrets config emails =
  do
    whoKnows <- (Email <$>) . lines <$> readProcess "git-secret" ["whoknows"] ""
    let isKnown = not $ null (emails `intersect` whoKnows)
    unless isKnown $ fail [i|Known users (#{whoKnows}) don't match imported GPG keys (#{emails})|]
    runShellIn config.configDir "git-secret" []


banner :: String -> IO ()
banner message =
  printDashes >> traverse (putStrLn . formatLine) lines' >> printDashes
  where
    lines' = lines message
    maxLength = maximum $ length <$> lines'
    formatLine l =
      let length' = length l
          spacing = ((maxLength - length') `div` 2) + 1
          spaces = replicate spacing ' '
      in  intercalate spaces ["|", l, "|"]
    printDashes = putStrLn $ replicate (maxLength + 4) '-'


runShellIn :: FilePath -> String -> [String] -> IO ()
runShellIn workingDirectory cmd args =
  void . createProcess $ (proc cmd args) {cwd = Just workingDirectory}
