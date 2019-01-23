import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents, listDirectory)

import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception (handle, SomeException)
import Control.Monad (forM, filterM)
import GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      return (if exists then [pat] else [])
  | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          case baseName of
            "**" -> do
              pathNames <- forM dirs listDirectories
              return (concat pathNames)
            _ -> do
              let listDir = if isPattern baseName
                            then listMatches
                            else listPlain
              pathNames <- forM dirs $ \dir -> do
                             baseNames <- listDir dir baseName
                             return (map (dir </>) baseNames)
              return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listDirectories :: String -> IO [String]
listDirectories dir = do
  allFiles <- do
    files <- listDirectory dir
    return $ map (dir </>) files
  dirs <- filterM doesDirectoryExist allFiles
  subdirs <- forM dirs listDirectories
  return $ dirs ++ (concat subdirs)


listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle handler $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter matcher names')
  where handler :: SomeException -> IO [String]
        handler = const (return [])
        matcher :: FilePath -> Bool
        matcher fp = case fp `matchesGlob` pat of
                       Right b  -> b
                       Left err -> error err

isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])
