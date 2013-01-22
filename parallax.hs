{-# LANGUAGE NoImplicitPrelude #-}

module Parallax where


import Language.Fay.Prelude
import Language.Fay.FFI
import Language.Fay.JQuery


movement :: Double -> Double -> Double -> Double -> Maybe Double
movement startY endY speed y
  | y >= startY || y <= endY = Just (speed * (y - startY))
  | otherwise                = Nothing

object1 :: (String, Double -> Maybe Double)
object1 = ("#object1", movement 1033 2600 1.2)

object2 :: (String, Double -> Maybe Double)
object2 = ("#object2", movement 800 2600 0.5)

object3 :: (String, Double -> Maybe Double)
object3 = ("#object3", movement 1066 2600 2)

objects :: [(String, Double -> Maybe Double)]
objects = [object1, object2, object3]


main :: Fay ()
main = documentReady onReady document

onReady :: Event -> Fay ()
onReady _ = do
  win <- selectElement window
  scroll onScroll win
  mapM_ (uncurry addScrollAnimation) navItemEvents


-- Scroll Animation

scrollSpeed :: Double
scrollSpeed = 0.5

navItemEvents :: [(String, Double)]
navItemEvents = [("#nav-item1", 100), ("#nav-item2", 1250), ("#nav-item3", 2400)]

addScrollAnimation :: String -> Double -> Fay JQuery
addScrollAnimation elementName position =
  select elementName >>= click onClick
 where
  onClick _ = do
    body <- select "body"
    oldPosition <- getScrollTop body
    let duration = abs (oldPosition - position) * scrollSpeed
    animateScrollTop position duration body


-- Scroll Event Processing

onScroll :: Event -> Fay ()
onScroll _ = mapM_ processObject objects

processObject :: (String, Double -> Maybe Double) -> Fay ()
processObject (objectName, objectMovement) = do
  win <- selectElement window
  pos <- getScrollTop win
  object <- select objectName
  case objectMovement pos of
       Nothing -> hide Instantly object >> return ()
       Just y -> do
         jshow Instantly object
         setPositionY y object


-- FFI

setPositionY :: Double -> JQuery -> Fay ()
setPositionY = ffi "%2.css(\"top\", %1)"

animateScrollTop :: Double -> Double -> JQuery -> Fay ()
animateScrollTop = ffi "%3.animate({'scrollTop': %1}, %2)"

window :: Element
window = ffi "window"

document :: Document
document = ffi "document"

