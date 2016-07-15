{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
--import Reflex.Dom
------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Reflex
import Reflex.Dom
------------------------------------------------------------------------------

{-# INLINABLE svgDynAttr' #-}
svgDynAttr' :: forall t m a. MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m (El t, a)
svgDynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

{-# INLINABLE svgDynAttr #-}
svgDynAttr :: forall t m a. MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m a
svgDynAttr elementTag attrs child = snd <$> svgDynAttr' elementTag attrs child

{-# INLINABLE svgAttr' #-}
svgAttr' :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m (El t, a)
svgAttr' elementTag attrs child = svgDynAttr' elementTag (constDyn attrs) child

{-# INLINABLE svgAttr #-}
svgAttr :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m a
svgAttr elementTag attrs child = svgDynAttr elementTag (constDyn attrs) child

{-# INLINABLE svg' #-}
svg' :: forall t m a. MonadWidget t m => String -> m a -> m (El t, a)
svg' elementTag child = svgAttr' elementTag (Map.empty :: AttributeMap) child

{-# INLINABLE svg #-}
svg :: forall t m a. MonadWidget t m => String -> m a -> m a
svg elementTag child = svgAttr elementTag Map.empty child

svgClass :: forall t m a. MonadWidget t m => String -> String -> m a -> m a
svgClass elementTag c child = svgAttr elementTag ("class" =: c) child

-- ok, this didn't work, so just paste in the body of the source here.
-- import Reflex.Dom.Contrib.Widgets.Svg


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
   svg "text" str
   mapM_ childListElem children
   return ()

main = mainWidget $ svg "svg" $ listElem basicList

