module Tags.Internal where

import qualified Data.Text as T

type TagName = T.Text

-- newline operator 
(<#>) :: T.Text -> T.Text -> T.Text
front <#> back = 
  let nl = T.pack "\n"
  in  front <> nl <> back 

newline :: T.Text -> T.Text
newline = 
  let nl = T.pack "\n"
  in  flip (<>) nl

openingTag :: T.Text -> T.Text 
openingTag name = 
  let oTag = T.pack "<"
      cTag = T.pack ">"
  in oTag <> name <> cTag 

closingTag :: T.Text -> T.Text 
closingTag name = 
  let oTag = T.pack "</"
      cTag = T.pack ">"
  in oTag <> name <> cTag 

tag :: TagName -> T.Text -> T.Text
tag name content
  | T.null content =  openingTag name <> closingTag name 
  | otherwise = openingTag name <> content <> closingTag name 

multiPack :: [String] -> [T.Text]
multiPack = fmap (T.pack) 

-- nests other tags (T.Text)
applyComponent :: (T.Text -> T.Text -> T.Text) -> String -> T.Text -> T.Text
applyComponent f str t = f (T.pack str) t

-- writes content inside tag (String)
applyContent:: (T.Text -> T.Text -> T.Text) -> String -> String -> T.Text
applyContent f t t' = f (T.pack t) (T.pack t')


