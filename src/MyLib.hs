module MyLib where

import System.Directory
import qualified Data.Text as T

type Path = String
type TagName = T.Text 

-- newline operator 
(<#>) :: T.Text -> T.Text -> T.Text
front <#> back = 
  let nl = T.pack "\n"
  in  front <> nl <> (increment back)

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

nTags :: [T.Text] -> T.Text
nTags (x:xs) = tag x <#> increment (tags xs) <#> endTag x
nTags [] = T.empty

tags :: [T.Text] -> T.Text
tags (x:xs) = tag x <#> endTag x <> newline (tags xs)
tags [] = T.empty

template :: T.Text 
template = 
  tag (T.pack "!DOCTYPE html") <#> nTags (T.pack <$> ["html", "head", "body"])

writeProject :: Path -> IO ()
writeProject path = do
  createDirectory path
  writeFile (path ++ "/index.html") (T.unpack template) 

