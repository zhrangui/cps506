module Assignment3 where
  import Control.Monad ( forM, forM_, liftM )
  import System.Directory
  import System.FilePath.Posix
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
  search :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
  search p path = do
    names <- dir_walk path
    return (filter p names)
  
  {-Test current directory and name is in it-}
  main :: IO ()
  main = do
    let dir = "."
    let searchKey = "assignment3"
    pathes <- dir_walk dir
    putStrLn (dir ++ " directory:")
    print pathes
    putStrLn (searchKey ++ " under " ++ dir ++ ":")
    files <- search (\name -> searchKey `isInfixOf` name) "."
    print files
    
      