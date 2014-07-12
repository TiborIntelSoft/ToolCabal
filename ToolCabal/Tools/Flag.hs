{-# LANGUAGE TemplateHaskell #-}
module Tools.Flag where

import Control.Lens hiding (argument)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid
import Data.Maybe
import Control.Applicative((<|>))
import Cabal
import Tools.DSet

-- * Data types

-- | The Argument type for @Flag@s. It is either a string argument
-- (e.g. FILE,DIR or URL)
-- or a set of enum values (like for the haddock -qual flag).
data Argument
  -- | A string argument that can be anything. The String parameter is 
  -- a short descriptive name as to what the argument should represent
  = Free String
  -- | An enum value should be supplied. The set describes the allowed
  -- values passed as an argument
  | Choice (Set String)
  deriving (Show, Read, Ord)

-- | The equality instance tests whether two Arguments
-- allow the same arguments.
instance Eq Argument where
  (Free _) == (Free _) = True
  (Choice s1) == (Choice s2) = s1 == s2
  _ == _ = False

--argToString :: Maybe Argument -> String
--argToString Nothing = ""
--argToString (Just s) = argumentCmdShow s

--argumentCmdShow :: Argument -> String
--argumentCmdShow (Free s) = s

-- | The data type for a flag
-- The type is not parametrised for simplicity in use
-- Ideally you would like a dependent type for the argument types
data Flag = Flag 
  {
    -- | The flag with only a single - prepended (e.g. -h, -O). 
    -- Set to the empty string if the short form is not supported for this flag
    _shortName :: String, 
    -- | The flag with two - prepended (e.g. --help, --version). 
    -- Set to the empty string if the long form is not supported for this flag
    _longName :: String,
    -- | The argument this flag supports. Nothing means the flag does not support an argument
    _argument :: Maybe Argument,
    -- | The default argument for the flag. Only has meaning if this flag accepts an argument.
    -- Nothing means that if an argument is supported one has to have been given
    _defaultArgument :: Maybe String,
    -- | A description of what this flag does
    _description :: String
  }
  deriving (Show, Read, Ord)

-- | Two Flags are equal one either the short name or the long name are equal
-- (empty string matches nothing. E.g. \"\" /= \"\" for the names)
-- and they admit the same set of arguments.
instance Eq Flag where
  f1 == f2 = ((_shortName f1 == _shortName f2) && _shortName f1 /= "") || (_longName f1 /= "" && (_longName f1 == _longName f2)) && _argument f1 == _argument f2

makeLenses ''Flag

--instance Monoid Flag where
--  mempty = Flag "" "" Nothing Nothing ""
--  _ `mappend` f = f

-- * Flag construction

-- | The empty Flag. This is not equal to any other flag including itself.
emptyFlag :: Flag
emptyFlag = Flag "" "" Nothing Nothing ""

-- | Constructs a new Flag with the specified long name
newFlag :: String -> Flag
newFlag longName = emptyFlag {_longName = longName}

-- | Constructs a new Flag with the specified short name
newShortFlag :: String -> Flag
newShortFlag shortName = emptyFlag {_shortName = shortName}

-- | Constructs a new Flag with the specified long name and argument.
-- The argument to the flag is stored in the defaultArgument slot.
-- This is because the Flag type is used to describe an allowed flag
-- and as the type for an actual flag supplied. Supplied arguments are
-- stored in the defaultArgument slot as at this point it is unknown if
-- it is an enum value or not.
newFlagWithArgument :: String -> String -> Flag
newFlagWithArgument ln a = emptyFlag {_longName = ln, _defaultArgument = Just a}

-- | Constructs a new Flag with the specified short name and argument.
-- The argument to the flag is stored in the defaultArgument slot.
-- This is because the Flag type is used to describe an allowed flag
-- and as the type for an actual flag supplied. Supplied arguments are
-- stored in the defaultArgument slot as at this point it is unknown if
-- it is an enum value or not.
newShortFlagWithArgument :: String -> String -> Flag
newShortFlagWithArgument sn a = emptyFlag {_shortName = sn, _defaultArgument = Just a}

-- * Command line functions

-- | This converts a set of Flags to a list of command line options
flagToProgArgs :: Set Flag -> [[ProgArg]]
flagToProgArgs = map flagToProgArg . S.toList

-- | This converts a Flag to a command line option
-- If the long name is set than it will use the long name, otherwise the short name
--
-- For short names this translates to \"-f\" or \"-f ARG\". For
-- long names it becomes \"--f\" or \"--f=ARG\"
flagToProgArg :: Flag -> [ProgArg]
flagToProgArg f | _longName f == "" = concat $ [['-':_shortName f]] ++ argShortArgument
                | otherwise = ["--" ++ _longName f ++ argCmdString]
  where argCmdString = maybe "" ('=':) $ _defaultArgument f
        argShortArgument = maybe [] (\x -> [[x]]) $ _defaultArgument f

-- * Flag validation

-- | This checks if a set of supplied flags satisfies the dependency constraints
-- and enum value constraints of the allowed flags
validFlags :: Set Flag -> DSet Flag -> Bool
validFlags = validDSet' satisfied

-- | Tests if the first argument is an instance of the second argument
satisfied :: Flag -> Flag -> Bool
satisfied check with = 
  ((_shortName check == _shortName with && _shortName check /= "") || (_longName check /= "" && _longName check == _longName with))
  && checkArgument (_defaultArgument check <|> _defaultArgument with) (_argument with)

-- | Tests if an argument is correct.
-- If no argument is allowed then no argument may be given.
-- An Free argument is trivially satisfied else a check 
-- is made if the supplied argument is an element of the allowed
-- enum values.
checkArgument :: Maybe String -> Maybe Argument -> Bool
checkArgument Nothing Nothing = True
checkArgument (Just _) (Just (Free _)) = True
checkArgument (Just f) (Just (Choice s)) = f `S.member` s
checkArgument _ _ = False

getFlags :: String -> Set Flag -> Maybe [Flag]
getFlags s d = if allEqual mf then (Just mf) else Nothing
  where dl = S.toList d
        mf = filter f dl
        f (Flag {_shortName = sn, _longName = ln}) = sn == s || ln == s

getEmptyFlag :: String -> Set Flag -> Maybe Flag
getEmptyFlag s d = case getFlags s d of
  Nothing -> Nothing
  Just mf -> Just $ baseFlag $ head mf

allEqual :: [Flag] -> Bool
allEqual [] = True
allEqual ((Flag {_shortName = s, _longName = l}):xs) = allEqual' xs
  where allEqual' [] = True
        allEqual' ((Flag {_shortName = sn, _longName = ln}):xs) = sn == s && ln == l && allEqual' xs

baseFlag :: Flag -> Flag
baseFlag (Flag {_shortName = sn, _longName = ln}) = emptyFlag {_shortName = sn, _longName = ln}

findFlagWithArg :: String -> [Flag] -> Maybe Flag
findFlagWithArg s (f@(Flag {_defaultArgument = Just a}):xs) = if a == s then Just f else findFlagWithArg s xs
findFlagWithArg _ _ = Nothing