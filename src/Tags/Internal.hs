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

nestTag :: [Tag] -> T.Text
nestTag ((Tag (name, attr)):xs) = 
  openingTag name attr <#> nestTag xs <#> closingTag name
nestTag [] = T.empty

expandTag :: [Tag] -> T.Text
expandTag ((Tag (name, attr)):xs) = 
  openingTag name attr <#> closingTag name <#> expandTag xs 
expandTag [] = T.empty

tags :: [Either Tag [Tag]] -> T.Text
tags ((Left (Tag (name, attr))):xs) = openingTag name attr <#> closingTag name <#> tags xs 
tags ((Right x):xs) = nestTag x <#> tags xs
tags [] = T.empty


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

-- test purposes -------------------------------

doctype :: T.Text
doctype = T.pack "<!DOCTYPE html>"

htmlTag :: Tag
htmlTag = 
  Tag ((T.pack "html"), [])

headTag :: Tag
headTag = 
  Tag ((T.pack "head"), []) 

bodyTag :: Tag
bodyTag = 
  Tag ((T.pack "body"), []) 

app :: T.Text
app = 
  doctype <#>
  tags [Right [[htmlTag, headTag, bodyTag]] 
