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

fromIni :: Ini -> Inventory
fromIni (Ini xss) = fromIni' xss (Inventory Map.empty Map.empty)

fromIni' :: [IniGroup] -> Inventory -> Inventory
fromIni' [] h = h
fromIni' ((IniGroup sectionName hosts):xss) (Inventory sMap hMap) =
        fromIni'
                xss
                (Inventory
                        (Map.insert
                                sectionName
                                (
                                          Set.fromList $ map
                                                (head. inventoriesIniMachines)
                                                hosts
                                        , Map.empty
                                )
                                sMap
                        )
                        (fromIni''
                                sectionName
                                (map
                                        (head . inventoriesIniMachines)
                                        hosts
                                )
                                hMap
                        )
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

-- | The list of sections.
data Ini = Ini [IniGroup]
        deriving Show

-- | The components of every group.
-- Groups of groups are parsed as one group here ("[southeast:children]"):
-- https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html#inheriting-variable-values-group-variables-for-groups-of-groups
-- Section level variables are parsed as machine names:
-- https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html#assigning-a-variable-to-many-machines-group-variables
data IniGroup = IniGroup Text.Text [IniMachines]
        deriving Show

-- | The components of every machine/variables.
data IniMachines = IniMachines
        {
                inventoriesIniMachines :: [Text.Text]
        } deriving Show

--------------------------------------------------------------------------------

-- INI parser starting point.
parserInventoriesIni :: AT.Parser Ini
parserInventoriesIni = do
        sections <- parserInventoriesIni' []
        return $ Ini sections

-- Group parser loop.
parserInventoriesIni' :: [IniGroup]
                      -> AT.Parser [IniGroup]
parserInventoriesIni' xss = do
        parserTrim
        peek <- AT.peekChar
        case peek of
                Nothing -> return xss
                (Just _) -> do
                        group <- parseGroup
                        parserInventoriesIni' (group:xss)

-- Parse a group.
-- The name of "[section_name:subsection_name]" or "[section_name:vars]" is
-- parsed as one text.
parseGroup :: AT.Parser IniGroup
parseGroup = do
        _ <- AT.char '['
        name <- AT.takeWhile (/= ']')
        _ <- AT.char ']'
        parserTrim
        content <- parseGroupContent []
        return $ IniGroup name content

-- Parse group content.
parseGroupContent :: [IniMachines]
                  -> AT.Parser [IniMachines]
parseGroupContent xss = do
        peek <- AT.peekChar
        case peek of
                Nothing -> return xss
                (Just char) -> if char == '['
                        then return xss
                        else do
                                machines <- parseMachines []
                                parserTrim
                                parseGroupContent
                                        ((IniMachines machines):xss)

-- Parse machine name and/or variables.
-- Variables are parsed as a single text.
parseMachines :: [Text.Text] -> AT.Parser [Text.Text]
parseMachines xss = do
        machineName <- AT.takeTill isSpace
        AT.skipWhile (\char -> char == ' ' || char == '\t')
        maybeChar <- AT.peekChar
        case maybeChar of
                Nothing -> return (xss ++ [machineName])
                (Just char) -> if AT.inClass "\r\n;#" char
                        then do
                                parserTrim
                                return (xss ++ [machineName])
                        else do
                                parserTrim
                                parseMachines (xss ++ [machineName])

-- Trim whitespace and comments.
parserTrim :: AT.Parser ()
parserTrim = do
        AT.skipSpace
        maybeChar <- AT.peekChar
        case maybeChar of
                Nothing -> return ()
                (Just char) -> if char == ';' || char == '#'
                        then do
                                AT.skipWhile (/= '\n')
                                parserTrim
                        else
                                return ()
