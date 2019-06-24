module Assignment3 where
  import Control.Monad ( forM, forM_, liftM )
  import System.Directory
  import System.FilePath
  import System.IO
  import Data.List

  {-find all contents in specified directory-}
  dir_walk :: FilePath -> IO [FilePath]
  dir_walk top = do
    names <- getDirectoryContents top
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
      let path = top </> name
      isDir <- doesDirectoryExist path
      if isDir
        then dir_walk path
        else return [path]
    return (concat paths)

  {-determin if file in under specified directory -}
  search :: FilePath -> FilePath -> IO [FilePath]
  search key path = do
    names <- dir_walk path
    return (filter (\name -> key `isInfixOf` name) names)
  
  {-Test current directory and name is in it-}
  main :: IO ()
  main = do
    let dir = ".."
        searchKey = "assignment3.hs"
    pathes <- dir_walk dir
    putStrLn (dir ++ " directory:")
    print pathes
    putStrLn (searchKey ++ " under " ++ dir ++ ":")
    files <- search searchKey dir
    print files
    
      