module Gen.HTML where

type Content = String
type Path = String

genHTML :: Path -> Content -> IO ()
genHTML p c = writeFile p c 
