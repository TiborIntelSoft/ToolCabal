module Debug
(
  traceShow,
  traceShow',
  traceShow'',
  traceFinished
)
where

import qualified Debug.Trace as T

debugText = "*************************************"

traceShow :: Show a => a -> b -> b
traceShow x y = T.traceShow (debugText,x,debugText) y

traceShow' :: Show a => a -> a
traceShow' x = traceShow x x

traceShow'' :: (Show a, Show b) => a -> b -> b
traceShow'' x y = traceShow (x,y) y

traceFinished :: Show a => a -> b -> b
traceFinished x b = traceShow ("start: " ++ show x) $ seq b $ traceShow ("end:" ++ show x) b 