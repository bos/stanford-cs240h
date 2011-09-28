
module Main where      -- redundant since Main is the default
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Char
import Network.HTTP.Enumerator (simpleHttp)
import System.Environment

main = do
  (url:_) <- getArgs        -- Sets url to first command-line argument
  page <- simpleHttp url    -- Sets page to contents as a ByteString
  putStr (L.toString page)  -- Converts ByteString to String and prints it
