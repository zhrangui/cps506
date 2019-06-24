module Assignment3 where
  import Control.Monad ( forM, forM_, liftM )
  import System.Directory
  import System.FilePath
  import System.IO
  import Data.List

  {-find all contents in specified directory-}
  dirWalk :: FilePath -> IO [FilePath]
  dirWalk dir = do
    names <- getDirectoryContents dir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then dirWalk path
        else return [path]
    return (concat paths)

  {-determine if file in under specified directory -}
  search :: FilePath -> FilePath -> IO [FilePath]
  search key path = do
    names <- dirWalk path
    return (filter (\name -> key `isInfixOf` name) names)
  
  {-Test current directory and name is in it-}
  main :: IO ()
  main = do
    let dir = ".."
        searchKey = "assignment3.hs"
    pathes <- dirWalk dir
    putStrLn (dir ++ " directory:")
    print pathes
    putStrLn (searchKey ++ " under " ++ dir ++ ":")
    files <- search searchKey dir
    print files
    
      