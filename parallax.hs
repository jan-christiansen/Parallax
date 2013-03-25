{-# LANGUAGE NoImplicitPrelude #-}

module Parallax where


import Prelude
import FFI
import JQuery

type DataName = String
type Class = String

movableClass :: Class
movableClass = ".movable"

navItemClass :: Class
navItemClass = ".navItem"

scrollPositionData :: DataName
scrollPositionData = "scrollpos"

speedData :: DataName
speedData = "speed"

offsetData :: DataName
offsetData = "offset"

visibilityData :: DataName
visibilityData = "vis"

endYData :: DataName
endYData = "end"

startYData :: DataName
startYData = "start"

main :: Fay ()
main = documentReady onReady document

onReady :: Event -> Fay ()
onReady _ = do
  win <- selectElement window
  scroll onScroll win
  triggerScroll win
  navItemEvents <- select navItemClass
  each (addScrollAnimation scrollSpeed) navItemEvents
  return ()
 where
  scrollSpeed = 0.5

-- Scroll Animation

addScrollAnimation :: Double -> Double -> Element -> Fay Bool
addScrollAnimation scrollSpeed _ element =
  selectElement element >>= click onClick >> return True
 where
  onClick _ = do
    bodyElem <- body
    oldPosition <- getScrollTop bodyElem
    object <- selectElement element
    position <- getDataDouble scrollPositionData object
    let duration = abs (oldPosition - position) * scrollSpeed
    animateScrollTop position duration bodyElem


-- Scroll Event Processing

onScroll :: Event -> Fay ()
onScroll _ = do
  win <- selectElement window
  pos <- getScrollTop win
  movableObjects <- select movableClass
  each (placeElement pos) movableObjects
  return ()

data Visibility = Always
                | Range Double Double

movement :: Visibility -> Double -> Double -> Double -> Maybe Double
movement Always speed c y = Just (speed * y + c)
movement (Range startY endY) speed c y
  | y >= startY && y <= endY = Just (speed * y + c)
  | otherwise                = Nothing

placeElement :: Double -> Double -> Element -> Fay Bool
placeElement yPos _ element = do
  object <- selectElement element
  speed <- getDataDouble speedData object
  offset <- getDataDouble offsetData object
  vis <- getVisibility object
  case movement vis speed offset yPos of
       Nothing -> do
         hide Instantly object
         return ()
       Just y -> do
         jshow Instantly object
         setPositionY y object
  return True


getVisibility :: JQuery -> Fay Visibility
getVisibility obj = do
  visibility <- getData visibilityData obj
  case visibility of
    "always" -> return Always
    "range"  -> do
      start <- getDataDouble startYData obj
      end   <- getDataDouble endYData obj
      return (Range start end)

-- FFI

getData :: String -> JQuery -> Fay String
getData = ffi "%2.data(%1)"

getDataDouble :: String -> JQuery -> Fay Double
getDataDouble = ffi "%2.data(%1)"

setPositionY :: Double -> JQuery -> Fay ()
setPositionY = ffi "%2.css(\"top\", %1)"

animateScrollTop :: Double -> Double -> JQuery -> Fay ()
animateScrollTop = ffi "%3.animate({'scrollTop': %1}, %2)"

triggerScroll :: JQuery -> Fay ()
triggerScroll = ffi "%1['trigger']('scroll')"

body :: Fay JQuery
body = select "body"

window :: Element
window = ffi "window"

document :: Document
document = ffi "document"
