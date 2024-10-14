module Tags.Internal where

import qualified Data.Text as T

type TagName = T.Text
type CSSid = T.Text
type CSSClasses = [T.Text]
type AttrName = T.Text
type AttrValue = T.Text
type Attribute = T.Text

data Tag = Tag (TagName, [Attribute]) deriving Show 

-- newline operator 
(<#>) :: T.Text -> T.Text -> T.Text
front <#> back = 
  let nl = T.pack "\n"
  in  front <> nl <> back 

-- whitespace operator
(<!>) :: T.Text -> T.Text -> T.Text
front <!> back =
  let ws = T.pack " "
  in  front <> ws <> back 

newline :: T.Text -> T.Text
newline = 
  let nl = T.pack "\n"
  in  flip (<>) nl

openingTag :: T.Text -> [Attribute] -> T.Text 
openingTag name attr = 
  let oTag = T.pack "<"
      cTag = T.pack ">"
  in oTag <> name <!> expandAttr attr <> cTag 

closingTag :: T.Text -> T.Text 
closingTag name = 
  let oTag = T.pack "</"
      cTag = T.pack ">"
  in oTag <> name <> cTag 

tag :: [Tag] -> T.Text
tag ((Tag (name, attr)):xs) = 
  openingTag name attr <#> tag xs <#> closingTag name
tag [] = T.empty

attribute :: AttrName -> AttrValue -> T.Text
attribute n v = 
  let assign = (T.pack "=")
  in  n <> assign <> v

expandAttr :: [Attribute] -> T.Text
expandAttr = 
  let ws = T.pack " "
  in  T.intercalate ws   

multiPack :: [String] -> [T.Text]
multiPack = fmap (T.pack) 

-- nests other tags (T.Text)
applyComponent :: (T.Text -> T.Text -> T.Text) -> String -> T.Text -> T.Text
applyComponent f str t = f (T.pack str) t

-- writes content inside tag (String)
applyContent:: (T.Text -> T.Text -> T.Text) -> String -> String -> T.Text
applyContent f t t' = f (T.pack t) (T.pack t')


