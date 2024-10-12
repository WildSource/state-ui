module Tags.Catalogue where

import Prelude hiding (head)
import qualified Data.Text as T
import Tags.Internal

type Title = String 

docType :: T.Text
docType = T.pack "<!DOCTYPE html>"

html :: T.Text -> T.Text
html = applyComponent tag "html" 

head :: T.Text -> T.Text
head = applyComponent tag "head"   

title :: String -> T.Text 
title = applyContent tag "title" 

body :: T.Text -> T.Text
body = applyComponent tag "body"

--Test part ---------------------------------

inHTML :: T.Text 
inHTML = 
  head (title "Example App") <#>
  body T.empty 
  

app :: T.Text
app =
  docType <#>
  html inHTML
