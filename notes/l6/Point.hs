module Point where

import Prelude hiding ((.))
import Data.Lens.Common
import Control.Category ((.))

data Point = Point {
      ptX :: Int
    , ptY :: Int
    } deriving (Show)

x, y :: Lens Point Int
x = lens ptX (\x pt -> pt {ptX = x})
y = lens ptY (\y pt -> pt {ptY = y})

data Line = Line {
      lnBeg :: Point
    , lnEnd :: Point
    } deriving (Show)

beg, end :: Lens Line Point
beg = lens lnBeg (\b l -> l {lnBeg = b})
end = lens lnEnd (\e l -> l {lnEnd = e})

b = Point 1 2
e = Point 3 4
l = Line b e
