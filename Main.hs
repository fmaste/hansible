{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

-- package: base.
import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.Char (isSpace)
-- package: attoparsec.
import qualified Data.Attoparsec.Text as AT
-- package:containers:
import qualified Data.Map as Map
import qualified Data.Set as Set
-- package: text.
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

--------------------------------------------------------------------------------

data Inventory = Inventory
        -- Sections map.
        (Map.Map Text.Text (Set.Set Text.Text, Variables))
        -- Hosts map.
        (Map.Map Text.Text (Set.Set Text.Text, Variables))
        deriving Show

type Variables = Map.Map Text.Text Text.Text

--------------------------------------------------------------------------------

-- Try it online at:https://dreampuf.github.io/GraphvizOnline/

main :: IO ()
main = do
        (filePath:_) <- getArgs
        fileText <- TextIO.readFile filePath
        let (Right ini) = AT.parseOnly parserInventoriesIni fileText
        -- print ini
        putStrLn "digraph inventory {"
        toDot $ fromIni ini
        putStrLn "}"

--------------------------------------------------------------------------------

toDot :: Inventory -> IO ()
toDot (Inventory sMap hMap) = do
        forM_
                (Map.toList sMap)
                (\(sectionName,(hSet,_)) -> do
                        forM_
                                (Set.toList hSet)
                                (\hostName -> do
                                        putStr "\""
                                        putStr (Text.unpack sectionName)
                                        putStr "\""
                                        putStr " -> "
                                        putStr "\""
                                        putStr (Text.unpack hostName)
                                        putStr "\""
                                        putStr "\n"
                                )
                )

--------------------------------------------------------------------------------

fromIni :: InventoriesIni -> Inventory
fromIni (InventoriesIni xss) = fromIni' xss (Inventory Map.empty Map.empty)


fromIni' :: [InventoriesIniSection] -> Inventory -> Inventory
fromIni' [] h = h
fromIni' ((InventoriesIniSection sectionName hosts):xss) (Inventory sMap hMap) =
        fromIni'
                xss
                (Inventory
                        (Map.insert
                                sectionName
                                (Set.fromList hosts, Map.empty)
                                sMap
                        )
                        (fromIni'' sectionName hosts hMap)
                )

fromIni'' :: Text.Text
          -> [Text.Text]
          -> (Map.Map Text.Text (Set.Set Text.Text, Variables))
          -> (Map.Map Text.Text (Set.Set Text.Text, Variables))
fromIni'' _ [] hMap = hMap
fromIni'' sectionName (hostName:hosts) hMap = fromIni'' sectionName hosts
        (Map.alter
                (\maybeValue -> case maybeValue of
                        Nothing -> Just $ (Set.singleton sectionName, Map.empty)
                        (Just (sSet, vars)) -> Just $
                                (Set.insert sectionName sSet, vars)
                )
                hostName
                hMap
        )

--------------------------------------------------------------------------------

data InventoriesIni = InventoriesIni [InventoriesIniSection]
        deriving Show

data InventoriesIniSection = InventoriesIniSection Text.Text [Text.Text]
        deriving Show

--------------------------------------------------------------------------------


parserInventoriesIni :: AT.Parser InventoriesIni
parserInventoriesIni = do
        sections <- parserInventoriesIni' []
        return $ InventoriesIni sections

parserInventoriesIni' :: [InventoriesIniSection]
                      -> AT.Parser [InventoriesIniSection]
parserInventoriesIni' xss = do
        parserWhiteSpace
        peek <- AT.peekChar
        case peek of
                Nothing -> return xss
                (Just _) -> do
                        section <- parseSection
                        parserInventoriesIni' (section:xss)

parseSection :: AT.Parser InventoriesIniSection
parseSection = do
        _ <- AT.char '['
        name <- AT.takeWhile (/= ']')
        _ <- AT.char ']'
        parserWhiteSpace
        content <- parseSectionContent []
        return $ InventoriesIniSection name content

parseSectionContent :: [Text.Text] -> AT.Parser [Text.Text]
parseSectionContent xss = do
        peek <- AT.peekChar
        case peek of
                Nothing -> return xss
                (Just char) -> if char == '['
                        then return xss
                        else do
                                content <- AT.takeTill isSpace
                                parserWhiteSpace
                                parseSectionContent (content:xss)

parserWhiteSpace :: AT.Parser ()
parserWhiteSpace = do
        AT.skipSpace
        maybeChar <- AT.peekChar
        case maybeChar of
                Nothing -> return ()
                (Just char) -> if char == ';' || char == '#'
                        then do
                                AT.skipWhile (/= '\n')
                                parserWhiteSpace
                        else
                                return ()
