import Data.Map (Map)
import qualified Data.Map as Map
import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg

-- Plans: Show a simple list of strings.  Then get it into an svg
-- (look into reflex-dom-contrib) then make it a tree.  then add the
-- expand-collapse behavior.  Yes, this works out better.  Keep the
-- type in the tree separate from the underlying graph. Use a view
-- transformation (application for lenses?) for conversion.

data Tree = T String [Tree] deriving Show

leaf s = T s []
basicList = T "root" ([T "child" [leaf "c1", leaf "c2", leaf "c3"]] ++ ( map leaf ["A", "B", "C"]))


childListElem c = svg "ellipse" $ listElem c
listElem (T str []) = text str
listElem (T str children) = svg "g" $ do
   svg "text" $ text str
   mapM_ childListElem children
   return ()

main = mainWidget $ svg "svg" $ listElem basicList

