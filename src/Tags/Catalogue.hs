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

p :: String -> T.Text
p = applyContent tag "p"

div :: T.Text -> T.Text
div = applyComponent tag "div" 
