module MyLib where

import System.Directory

type Path = String

writeProject :: Path -> IO ()
writeProject path = do
  createDirectory path
