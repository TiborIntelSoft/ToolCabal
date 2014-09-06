{-# LANGUAGE FlexibleContexts #-}
module Tools.Cabal.FileFormatParser where

import Data.Set (Set)
import qualified Data.Set as S

import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils

import Tools.Cabal.FileFormat
import Cabal
import Tools.Flag
import Tools.Core.Types

pFileFormat :: Parser FileFormat
pFileFormat = FileFormat 
  <$> pName
  <*> pVersion
  <*> pMaybe pLibrary
  <*> pList pExecutable
   <*> pList pToolDescription
   <*> ((pToken "Global:" *> pSpaces) *> pSourceInfo)

pName :: Parser String
pName = (pToken "Name:" *> pSpaces) *> pIdent

pIdent :: Parser String
pIdent = pList1 (pDigit <|> pLetter <|> pSym '.' <|> pSym '-') <* pSpaces

pVersion :: Parser Version
pVersion = (\x -> Version x []) <$> ((pToken "Version:" *> pSpaces) *> pNumList)

pNumList :: Parser [Int]
pNumList = (:) <$> pInteger <*> pList_ng (pToken "." *> pInteger) <* pSpaces

pLibrary :: Parser Library
pLibrary = Library
  <$> ((pToken "Library:" *> pSpaces) *> pIdent)
  <*> pVersion
  <*> ((pToken "Exposed modules:" *> pSpaces) *> pList_ng pFilePath)
  <*> pSourceInfo

pExecutable :: Parser Executable
pExecutable = Executable
  <$> ((pToken "Executable:" *> pSpaces) *> pIdent)
  <*> ((pToken "Main-module-path:" *> pSpaces) *> pFilePath)
  <*> pSourceInfo

pToolDescription :: Parser Executable
pToolDescription = Executable
  <$> ((pToken "Tool:" *> pSpaces) *> pIdent)
  <*> ((pToken "Main-module-path:" *> pSpaces) *> pFilePath)
  <*> pSourceInfo

pFilePath :: Parser String
pFilePath = pList1 pAllowedSyms <* pSpaces <<|> pQuotedString
  where pAllowedSyms = pDigit <|> pLetter <|> pSym '/' <|> pSym '\\' <|> pSym '.' <|> pSym '-' <|> pSym '_'

pSourceInfo :: Parser SourceInfo
pSourceInfo = f
  <$> ((pToken "Source-files:" *> pSpaces) *> pList_ng (pEither pFilePath pPreprocessFile))
  <*> ((pToken "Packages:" *> pSpaces) *> pList_ng pIdent)
  where f x = let (sf, pf) = toSourceInfo x
              in SourceInfo sf pf

pPreprocessFile :: Parser PreprocessFile
pPreprocessFile = (,) 
  <$> ((pToken "Use-tool:" *> pSpaces) *> pFilePath)
  <*> pList1 pPreprocess

pPreprocess :: Parser PreProcess
pPreprocess = pParens ((\x y z -> (x, y, z)) 
  <$> (pFilePath <* pToken "," <* pSpaces)
  <*> (pFilePath <* pToken "," <* pSpaces)
  <*> pSetFlag)
  <* pSpaces

pSetFlag :: Parser (Set Flag)
pSetFlag = S.fromList <$> pFlags

pFlags :: Parser [Flag]
pFlags = pList pFlag

pFlag :: Parser Flag
pFlag = pLongFlag <<|> pShortFlag

pLongFlag :: Parser Flag
pLongFlag = (\x y -> Flag "" x Nothing y "")
  <$> (pToken "--" *> pIdent)
  <*> pMaybe pArgument

pShortFlag :: Parser Flag
pShortFlag = (\x y -> Flag x "" Nothing y "")
  <$> (pToken "-" *> pIdent)
  <*> pMaybe pArgument

pArgument :: Parser String
pArgument = pToken "=" *> pFilePath

toSourceInfo :: [Either SourceFile PreprocessFile] -> ([SourceFile], [PreprocessFile])
toSourceInfo [] = ([],[])
toSourceInfo (x:xs) = case x of 
  Left y -> (y : sf, pf)
  Right y -> (sf, y : pf)
  where (sf, pf) = toSourceInfo xs