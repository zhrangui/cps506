module Assignment3 where
  import Control.Monad ( forM, forM_, liftM )
  import System.Directory
  import System.FilePath.Posix
  import System.IO

  getRecursiveContents :: FilePath -> IO [FilePath]
  getRecursiveContents topPath = do
    names <- getDirectoryContents topPath
    let
      properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
      let path = topPath </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
      then getRecursiveContents path
      else return [path]
    return (concat paths)


  simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
  simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)
   
  dir_walk :: FilePath -> (FilePath -> IO ()) -> IO ()
  dir_walk top filefunc = do
    isDirectory <- doesDirectoryExist top
    if isDirectory
      then 
        do
          files <- listDirectory top
          mapM_ (\file -> dir_walk (top </> file) filefunc) files
      else
        filefunc top
   
  main :: IO ()
  main = do
    let worker fname = do
          putStrLn fname
    dir_walk "." worker