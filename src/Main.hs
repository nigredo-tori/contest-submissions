{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils (strip)

import Text.XML.HXT.Core
import Text.XML.HXT.DOM.QualifiedName
import Data.Tree.NTree.TypeDefs (NTree (..))

import Control.Exception (catch)
import Network.HTTP.Conduit (simpleHttp, HttpException(..))
import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.ICU.Convert as TC
import Text.CSV

-- Everything is synchronous, there's no error handling.
-- If parsing fails - everything just crashes.

type Url = String
data Section = Section {
    sectionHeader :: String
  , sectionBody :: String
  } deriving Show
data ParsedPage = ParsedPage {
    pageName :: String
  , pageSections :: [Section]
  } deriving Show

main :: IO ()
main = do
  urls <- lines <$> readFile "in.txt"
  parsedPages <- forM urls $ \url ->
    parsePage <$> getUrl url
  let table = makeTable parsedPages
  writeTable "out.csv" table

prettySection (Section h b) = "\n" ++ h ++ "\n    " ++ b ++ "\n"

-- hxt-http breaks on windows-1251 charset for some reason
getUrl :: String -> IO String
getUrl url = do
  raw <- BS.toStrict <$> worker
  putStrLn $ "Got URL " ++ url
  converter <- TC.open "CP1251" Nothing
  return $ T.unpack $ TC.toUnicode converter raw
  where
    -- After about 10 successive pages the server starts
    -- responding with 404.
    worker :: IO BS.ByteString
    worker = simpleHttp url `catch`
      \(StatusCodeException _ _ _) -> do
        putStrLn $ "Failed URL " ++ url ++ ", waiting for retry"
        threadDelay retryInterval *> worker
    retryInterval = 10 * 1000000      

writeTable :: String -> CSV -> IO ()
writeTable filename = writeFile filename . printCSV

makeTable :: [ParsedPage] -> CSV
makeTable pages = ("":names) : map makePropRow properties
  where
    names = map pageName pages
    properties = nub $ map sectionHeader $ pages >>= pageSections
    makePropRow prop = prop : map (cellValue prop) pages
    cellValue :: String -> ParsedPage -> String
    cellValue prop page = fromMaybe "—" $
      fmap sectionBody $
      find ((prop ==) . sectionHeader) $
      pageSections page    

parsePage :: String -> ParsedPage
parsePage = head . runLA arrowImpl
  where
    arrowImpl = hread >>>
      (parseName &&& parseSections) >>^
      makeCleanParsedPage
    parseName =
      deep(isElem >>> hasName "div" >>>
           hasAttrValue "id" (== "breadcrumbs")) />
      getText >>^ strip
    parseSections :: LA XmlTree [Section]
    parseSections = listA $
      deep (isElem >>> hasName "div" >>>
            hasAttrValue "id" (== "div_nest")) >>>
      getChildren >>.
      (map makeSection . splitDivNest)
      
    splitDivNest :: [XmlTree] -> [(XmlTree, XmlTrees)]
    splitDivNest = map (fromJust . uncons) . tail .
      (split $ keepDelimsL $ whenElt isHeaderTag) .
      takeWhile (not . isRestNest)
    makeSection :: (XmlTree, XmlTrees) -> Section
    makeSection = head . runLA (
      (getCleanHeader *** getCleanBody) >>^ (uncurry Section))
    isHeaderTag :: XmlTree -> Bool
    isHeaderTag = not . null . runLA (hasNameIn headers)
    isRestNest :: XmlTree -> Bool
    isRestNest = not . null . runLA (
      hasName "table" >>> hasAttrValue "class" (== "rest_nest"))
    headers = map (('h':) . show) [1..6]
    getCleanHeader :: LA XmlTree String
    getCleanHeader = getChildren >>> getText >>^ strip
    getCleanBody :: LA XmlTrees String
    getCleanBody = getTreeAsText
    
    makeCleanParsedPage :: (String, [Section]) -> ParsedPage
    makeCleanParsedPage (name, sects) = ParsedPage name sects'
      where
        sects' = map cleanSection sects
        cleanSection (Section header body) =
          Section (normalizeSectionHeader names header) body
        names = nub $ name : maybeToList rusName
        rusName = (strip . sectionBody) <$>
          find (("Русское название" ==) . strip . sectionHeader) sects

normalizeSectionHeader :: [String] -> String -> String
normalizeSectionHeader names =
  stripSuffices suffices . strip
  where
    -- Try to remove suffices in sequence, starting
    -- from the head of the list
    stripSuffices :: [String] -> String -> String
    stripSuffices suffices = foldl (flip (.)) id $
      map (\s -> strip . stripSuffix s) suffices

    suffices = names ++ customNames ++ uselessSuffices
    -- Section titles use shortened name for this one
    customNames = ["Диэтиламинопропионил"]
    -- These don't carry any information, but add to duplication
    uselessSuffices = ["вещества", "веществ"]
    stripSuffix suf s =
      if suf `isSuffixOf` s
      then dropLast (length suf) s
      else s
        
hasNameIn :: [String] -> LA XmlTree XmlTree
hasNameIn ss = hasNameWith (flip elem ss . localPart)

hasNameNotIn :: [String] -> LA XmlTree XmlTree
hasNameNotIn ss = hasNameWith (not . flip elem ss . localPart)

getTreeAsText :: LA XmlTrees String
getTreeAsText = listA(unlistA >>> toTextTree >>> deep getText) >>>
  arrL ((:[]) . strip . concat)
  where
    toTextTree = processBottomUp $
      (isText) <+>
      (isElem >>> (
          (hasNameIn ["br", "p"] >>> changeChildren (mkTextLeaf "\n" :)) <+>      
          hasNameNotIn ["script"]))

mkTextLeaf :: String -> XmlTree
mkTextLeaf s = head $ runLA mkText s

dropLast n = reverse . drop n . reverse
