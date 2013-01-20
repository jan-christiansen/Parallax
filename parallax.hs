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
object1 = ("#object1", movement 1000 2600 1.2)

object2 :: (String, Double -> Maybe Double)
object2 = ("#object2", movement 1000 2600 0.5)

object3 :: (String, Double -> Maybe Double)
object3 = ("#object3", movement 1000 2600 2)

objects :: [(String, Double -> Maybe Double)]
objects = [object1, object2, object3]


main :: Fay ()
main = do
  win <- selectElement window
  scroll onScroll win
  return ()

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
         setPositionY object y

setPositionY :: JQuery -> Double -> Fay ()
setPositionY = ffi "%1.css(\"top\", %2)"

window :: Element
window = ffi "window"
