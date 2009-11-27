#!/usr/bin/runhaskell
{-
 * Copyright (C) 2009 Byron James Johnson

 * This file is free: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 2 of the License

 * This file is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License.
 * If not, see <http://www.gnu.org/licenses/>.
-}

import Data.Bits
import Data.Char
import System.Directory
import Data.Map as Map
import Data.List as List
import Data.List.Split
import Data.Maybe
import Control.Monad (forM, mapM, when)
import Data.ByteString.Internal (c2w)

import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink)
import System.FilePath.Posix ((</>))
import System.IO (hClose, openFile, IOMode (ReadMode), hGetContents)

statsDirectory = "/home/bob/git/oc-data/stats"


data Score = Score { s_map      :: String
                   , s_layout   :: String
                   , s_type     :: ScoreType
                   , s_num      :: Int
                   , s_maxCount :: Int
                   , s_count    :: Int
                   , s_time     :: Int
                   , s_name     :: String
                   , s_date     :: String
                   , s_guid     :: String
                   , s_ip       :: String
                   } deriving (Eq)

data ScoreType = ArmType | MediType deriving (Eq)

instance Ord Score where
    (Score {s_map = am, s_layout = al, s_type = at, s_num = an}) `compare ` (Score {s_map = bm, s_layout = bl, s_type = bt, s_num = bn})
        | am /= bm  = am `compare` bm
        | al /= bl  = al `compare` bl
        | at /= bt  = at `compare` bt
        | an /= bn  = an `compare` bn
        | otherwise = EQ

instance Ord ScoreType where
    -- Win scores first
    (MediType) `compare` (ArmType)  = GT
    (ArmType)  `compare` (MediType) = LT
    _          `compare` _          = EQ

putStrNl = putStrLn

msec   :: Int   -> Int
secs   :: Int   -> Int
mins   :: Int   -> Int
msec'  :: Int   -> String
secs'  :: Int   -> String
mins'  :: Int   -> String
msec'' :: Score -> String
secs'' :: Score -> String
mins'' :: Score -> String
msec t = ((t) - (((t) `div` 1000) * 1000))
secs t = (((t) - (((t) `div` 60000) * 60000)) `div` 1000)
mins t = ((t) `div` 60000)
msec'  = show . msec
secs'  = show . secs
mins'  = show . mins
msec'' = show . msec . s_time
secs'' = show . secs . s_time
mins'' = show . mins . s_time

colour :: Char -> String
colour c = case ((c2w c) - (c2w '0')) .&. 0x07 of
                0 -> "Black"
                1 -> "Red"
                2 -> "Lime"
                3 -> "Yellow"
                4 -> "Blue"
                5 -> "Aqua"
                6 -> "Fuchsia"
                7 -> "White"

colourSpan :: Char -> String
colourSpan c = "<span style=\"color:" ++ colour' ++ "\">"
               where colour' = case ((c2w c) - (c2w '0')) .&. 0x07 of
                                    0 -> "Black"
                                    1 -> "Red"
                                    2 -> "Lime"
                                    3 -> "Yellow"
                                    4 -> "Blue"
                                    5 -> "Aqua"
                                    6 -> "Fuchsia"
                                    7 -> "White"

clean :: String -> String
clean []        = ""
clean ('^':c:s) = colour c ++ clean s
clean ('^':_)   = ""
clean (f:l)     = f:l

repeatList :: Int -> [a] -> [a]
repeatList n xs = take (length xs) $ cycle xs

colourString :: String -> String
colourString = colourString' 0

colourString' :: Int -> String -> String
colourString' n []        = repeatList n "</span>"
colourString' n ('^':c:s) = colourSpan c ++ colourString' (succ n) s
colourString' n ('^':_)   = repeatList n "</span>"
colourString' n (f:l)     = f:l ++ repeatList n "</span>"

listTruncate :: Int -> [a] -> [a]
listTruncate n = reverse . drop n . reverse

resolve :: FilePath -> IO FilePath
resolve filename = do
    status <- getFileStatus filename
    if isSymbolicLink status
        then do
            realFilename <- readSymbolicLink filename
            resolve realFilename
        else do
            return filename

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' filename = do
    realFilename <- resolve filename
    getDirectoryContents realFilename

doesDirectoryExist' :: FilePath -> IO Bool
doesDirectoryExist' filename = do
    realFilename <- resolve filename
    doesDirectoryExist realFilename

writeHeader :: Map.Map String String -> IO ()
writeHeader vars = do
    putStrNl "Content-type: text/html"
    putStrNl ""
    putStrNl "<html>"

writeFooter :: Map.Map String String -> IO ()
writeFooter vars = do
    putStrNl "</html>"

score :: Map.Map String String -> String -> String -> ScoreType -> String -> (Int, String) -> Maybe Score
score vars mapname layoutname scoreType numLine (id, scoreLine) = do
    let numArms   = read $ head $ words numLine :: Int
        numMedis  = read $ last $ words numLine :: Int
        num       = case scoreType of
                         (ArmType)  -> numArms
                         (MediType) -> numMedis

        score     = unintercalate "\x01" scoreLine
        count     = read $ score !! 0 :: Int
        time      = read $ score !! 1 :: Int
        name      = score !! 2 :: String
        date      = score !! 3 :: String
        guid      = score !! 4 :: String
        ip        = score !! 5 :: String

    if (length scoreLine > 0 && length numLine > 0 && length (unintercalate "\x01" scoreLine) > 5 && length (words scoreLine) > 1)
        then do
            Just $ Score mapname layoutname scoreType id num count time name date guid ip
        else do
            Nothing

layoutDir :: Map.Map String String -> String -> String -> IO [Score]
layoutDir vars mapname layoutname = do
    let filename = statsDirectory </> mapname </> layoutname

    let search = case Map.lookup "search" vars of
                      (Just s)  -> List.map toLower s
                      (Nothing) -> ""
        don'tSearch = search `isInfixOf` mapname || search `isInfixOf` layoutname

    layoutIsDirectory <- doesDirectoryExist filename

    if ((head layoutname /= '.') && (layoutIsDirectory)) then do
        let winFilename  = filename </> "win.dat"
            mediFilename = filename </> "med.dat"

        winFileExists  <- doesFileExist winFilename
        mediFileExists <- doesFileExist mediFilename
        if winFileExists && mediFileExists
            then do
                winHandle  <- openFile winFilename ReadMode
                winData    <- hGetContents winHandle
                mediHandle <- openFile winFilename ReadMode
                mediData   <- hGetContents mediHandle

                let wAllLines   = List.lines winData
                    wNumLine    = if length wAllLines > 0 then head wAllLines else []
                    wScoreLines = if length wAllLines > 0 then tail wAllLines else [[]]
                let mAllLines   = List.lines mediData
                    mNumLine    = if length mAllLines > 0 then head mAllLines else []
                    mScoreLines = if length mAllLines > 0 then tail mAllLines else [[]]

                let wScoresMaybes = (flip List.map) (zip [1..] wScoreLines) $ score vars mapname layoutname ArmType  wNumLine
                let mScoresMaybes = (flip List.map) (zip [1..] mScoreLines) $ score vars mapname layoutname MediType mNumLine
                let wScores = catMaybes wScoresMaybes
                let mScores = catMaybes mScoresMaybes
                return $ wScores ++ mScores

                --hClose winHandle
                --hClose minHandle
            else do
                if winFileExists
                    then do
                        winHandle  <- openFile winFilename ReadMode
                        winData    <- hGetContents winHandle

                        let wAllLines   = List.lines winData
                            wNumLine    = if length wAllLines > 0 then head wAllLines else []
                            wScoreLines = if length wAllLines > 0 then tail wAllLines else [[]]

                        let wScoresMaybes = (flip List.map) (zip [1..] wScoreLines) $ score vars mapname layoutname ArmType wNumLine
                        let wScores = catMaybes wScoresMaybes
                        return wScores

                        --hClose winHandle
                    else do
                        if winFileExists
                            then do
                                mediHandle <- openFile winFilename ReadMode
                                mediData   <- hGetContents mediHandle

                                let mAllLines   = List.lines mediData
                                    mNumLine    = if length mAllLines > 0 then head mAllLines else []
                                    mScoreLines = if length mAllLines > 0 then tail mAllLines else [[]]

                                let mScoresMaybes = (flip List.map) (zip [1..] mScoreLines) $ score vars mapname layoutname MediType mNumLine
                                let mScores = catMaybes mScoresMaybes
                                return mScores

                                --hClose minHandle
                            else do
                                return []

        else
            return []

mapDir :: Map.Map String String -> String -> IO [Score]
mapDir vars mapname = do
    let filename = statsDirectory </> mapname

    mapIsDirectory <- doesDirectoryExist filename

    if (head mapname /= '.')
        then do
            contents <- getDirectoryContents' filename
            if List.null contents
                then do
                    return []
                else do
                    rawScores <- forM contents $ layoutDir vars mapname
                    return $ List.concat rawScores
        else do
            return []

-- the scores need to be sorted already
sortScores :: Map.Map String String -> [Score] -> IO ()
sortScores vars scores = undefined

sortAllStats :: Map.Map String String -> IO ()
sortAllStats vars = do
    putStr "<head><title>"
    putStr "Viewing Stats"
    putStr "</title></head>"

    let search = case Map.lookup "search" vars of
                      (Just s)  -> List.map toLower s
                      (Nothing) -> ""

    exists <- doesDirectoryExist' statsDirectory
    if exists
        then do
            contents <- getDirectoryContents' statsDirectory
            if List.null contents
                then do
                    putStrNl "<p>No maps were found</p>"
                else do
                    rawScores <- forM contents $ mapDir vars
                    let scores = List.concat rawScores

                    let l = (++ ", ") . List.map toLower
                        f (Score {s_count = count, s_maxCount = max, s_name = name, s_date = date, s_time = time}) =
                            let countStr  = show count ++ " / " ++ show max
                                timeStr   = mins' time ++ "m:" ++ secs' time ++ "s:" ++ msec' time ++ "ms"
                                nameStr   = clean name

                                l         = (++ ", ") . List.map toLower

                                searchStr = l countStr ++ l timeStr ++ l date ++ l nameStr
                            in search `isInfixOf` searchStr
                        scores' = sort . List.filter f $ scores

                    -- print scores
                    when (not $ List.null search) $ do
                        putStrNl $ "<p>Found " ++ (show $ length scores') ++ " results with map, layout, score, time, or name matching \"" ++ search ++ "\"</p>"

                    sortScores vars scores'
        else do
            putStrNl $ "stats directory doesn't exist! (" ++ statsDirectory ++ ")"

tailAlways :: [a] -> [a]
tailAlways [] = []
tailAlways xs = tail xs

splitTwo' :: String -> [(String, String)] -> String -> [(String, String)]
splitTwo' _ acc [] = acc
splitTwo' delimiters acc string = splitTwo' delimiters ((former, latter):acc) rest
    where former = takeWhile isNotDelimiter string
          latter = takeWhile isNotDelimiter $ nextPart string
          rest   = takeWhile isNotDelimiter $ nextPart . nextPart $ string
          isNotDelimiter = (\x -> not $ x `elem` delimiters)
          nextPart = tailAlways . dropWhile isNotDelimiter

splitTwo :: String -> String -> [(String, String)]
splitTwo delimiters = splitTwo' delimiters []

queryVars :: String -> Map.Map String String
queryVars = Map.fromList . splitTwo "=&"

main = do
    currentDirectory <- getCurrentDirectory
    putStr "*All* non-hidden .dat files in \""
    putStr currentDirectory
    putStr "\" will be sorted.  Type the name of the directory to continue: \n"
    confirm <- getLine
    if confirm == currentDirectory then
        do
            sortAllStats Map.empty
    else
        do
            putStr "You typed \""
            putStr confirm
            putStr "\", which doesn't match \""
            putStr currentDirectory
            putStr "\".  Stopping.\n"
