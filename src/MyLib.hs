module MyLib where

import System.Directory
import qualified Data.Text as T
import Data.Text (Text)

type Path = String
type TagName = T.Text 

data Tag = Tag {
  html :: T.Text  
}

(<#>) :: T.Text -> T.Text -> T.Text
front <#> back = 
  let newline = T.pack "\n"
  in  front <> newline <> back

tag :: TagName -> T.Text 
tag name = 
  let oTag = T.pack "<"
      cTag = T.pack ">"
  in oTag <> name <> cTag 

endTag :: TagName -> T.Text 
endTag name = 
  let oTag = T.pack "<"
      cTag = T.pack "/>"
  in oTag <> name <> cTag 

tags :: TagName -> T.Text
tags name = tag name <> endTag name

template :: Tag
template = Tag $ tag (T.pack "!DOCTYPE html")

writeTemplate :: Path -> IO ()
writeTemplate = 
  let content = T.pack "html"
  in flip (writeFile) (T.unpack $ tags content) 

writeProject :: Path -> IO ()
writeProject path = do
  createDirectory path
  writeTemplate (path ++ "/index.html") 
