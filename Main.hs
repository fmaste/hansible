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

data Hosts = Hosts
        -- Sections map.
        (Map.Map Text.Text (Set.Set Text.Text))
        -- Hosts map.
        (Map.Map Text.Text (Set.Set Text.Text))
        deriving Show

--------------------------------------------------------------------------------

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

toDot :: Hosts -> IO ()
toDot (Hosts sMap hMap) = do
        forM_
                (Map.toList sMap)
                (\(sectionName,hSet) -> do
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

fromIni :: InventoriesIni -> Hosts
fromIni (InventoriesIni xss) = fromIni' xss (Hosts Map.empty Map.empty)


fromIni' :: [InventoriesIniSection] -> Hosts -> Hosts
fromIni' [] h = h
fromIni' ((InventoriesIniSection sectionName hosts):xss) (Hosts sMap hMap) =
        fromIni'
                xss
                (Hosts
                        (Map.insert sectionName (Set.fromList hosts) sMap)
                        (fromIni'' sectionName hosts hMap)
                )

fromIni'' :: Text.Text
          -> [Text.Text]
          -> (Map.Map Text.Text (Set.Set Text.Text))
          -> (Map.Map Text.Text (Set.Set Text.Text))
fromIni'' _ [] hMap = hMap
fromIni'' sectionName (hostName:hosts) hMap = fromIni'' sectionName hosts
        (Map.alter
                (\maybeSet -> case maybeSet of
                        Nothing -> Just $ Set.singleton sectionName
                        (Just sSet) -> Just $ Set.insert sectionName sSet
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
