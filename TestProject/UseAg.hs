module UseAg where

import AG

runAG :: [String] -> String
runAG ss = cString_Syn_SStrings $ wrap_SStrings (sem_SStrings (toSStrings ss)) (Inh_SStrings)
  where toSStrings [] = NilString
        toSStrings (x:xs) = ConsString (SString x) $ toSStrings xs
