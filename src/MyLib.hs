module MyLib where

import System.Directory
import qualified Data.Text as T

type Path = String

writeProject :: Path -> IO ()
writeProject path = do
  createDirectory path
  writeFile (path ++ "/index.html") (T.unpack undefined) 

