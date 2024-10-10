module MyLib where

import System.Directory
import qualified Data.Text as T

type Path = String
type TagName = T.Text 

data Tag = Tag {
  html :: T.Text  
}

-- newline operator 
(<#>) :: T.Text -> T.Text -> T.Text
front <#> back = 
  let nl = T.pack "\n"
  in  front <> nl <> back

newline :: T.Text -> T.Text
newline = 
  let nl = T.pack "\n"
  in  flip (<>) nl

increment :: T.Text -> T.Text
increment = (T.pack "  " <>) 

indentNewLine :: T.Text -> T.Text
indentNewLine = (increment . newline)


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

tags :: [T.Text] -> T.Text
tags (x:xs) = tag x <> increment (tags xs) <> endTag x
tags [] = T.empty

template :: Tag
template = Tag $ tag (T.pack "!DOCTYPE html") <#> tags (T.pack "html")

writeTemplate :: Path -> IO ()
writeTemplate = 
  let content = T.pack "html"
  in flip (writeFile) (T.unpack $ tags content) 

writeProject :: Path -> IO ()
writeProject path = do
  createDirectory path
  writeTemplate (path ++ "/index.html") 
