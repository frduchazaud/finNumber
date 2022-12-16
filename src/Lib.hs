module Lib
    ( game
    ) where


import Control.Monad.Trans.Maybe
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.String as String
import System.IO

--import qualified Data.Text as T
import Data.Char
import Data.String

--import Data.Text
import Data.Void
import Flow

import qualified Data.Time.Clock.System as SysTime
import qualified System.Random as Random

import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M


promptInt :: String -> IO (Maybe Int)
promptInt msg =
    let
        tryTextToInt :: String -> IO (Maybe Int)
        tryTextToInt str =
            if all Char.isDigit str then
                return <| Just <| read str

            else do
                writeWarning <| "Impossible de lire \"" ++ str
                    ++ "\" comme un nombre entier !"
                return Nothing
    in do
        putStr <| msg ++ " (nombre entier positif) > "
        hFlush stdout
        str <- getLine
        if null str then
            return Nothing

        else
            (str
                |> trimFirstWord
                |> tryTextToInt
            ) -- => IO (Maybe Int)
                >>= maybe (promptInt msg) (return <. Just)


trimLeft :: String -> String
trimLeft =
    dropWhile Char.isSpace


trimRightAfterWord :: String -> String
trimRightAfterWord =
    takeWhile (not <. Char.isSpace)


trimFirstWord :: String -> String
trimFirstWord =
    trimLeft .> trimRightAfterWord


-- >>> trimRight ""
-- >>> trimRight "  1   "
-- >>> trimRight " 1 sdf sdf \n"
-- >>> trimRight " "
-- ""
-- "  1"
-- " 1 sdf sdf"
-- ""
trimRight :: String -> String
trimRight =
    List.dropWhileEnd Char.isSpace


-- >>> trim "   aaaaa  bbbbb     "
-- "aaaaa  bbbbb"
trim :: String -> String
trim =
    trimLeft .> trimRight


askFirstName :: Int -> IO String
askFirstName i =
    let
        select :: Int -> [a] -> a
        select i xs =
            xs !! (i `mod` length xs)

        exclams =
            ["Oulaaaah", "Outch", "Oléééééé", "Ouhahouuuuu"]

        adverbs =
            ["", "assez ", "plutôt ", "encore ", "très ", "toujours "]

        adjs =
            [ "bizarre"
            , "étrange"
            , "curieux"
            , "étonnant"
            , "suspect"
            , "fallacieux"
            , "dingue"
            ]
    in do
        putStr "Quel est ton prénom ? > "
        hFlush stdout
        str <- getLine
        let trimmed = trim str
        if
            null trimmed || not (any Char.isAlpha trimmed)
                || not (pronouncable trimmed)
        then do
            writeWarning <|
                select i exclams
                ++ " ! Ce prénom est "
                ++ select i adverbs
                ++ select i adjs
                ++ "...   (???)"
            askFirstName <| i + 1

        else
            return trimmed


pronouncable :: String -> Bool
pronouncable str =
    True


{-- }
type MParser =
    Parsec Void Text


nameParser :: MParser Bool
nameParser =
    M.many subNameParser M.lastSubNameParser



subNameParser :: Mparser Bool
subNameParser =
    

lastSubNameParser :: Mparser Bool
lastSubNameParser =
    

consonantLowParser :: Mparser Bool
consumnLowParser =
    M.satisfy (\c -> Char.isLetter c && Char.isLower c )

consonantUpParser:: Mparser Bool
consumnUpParser =
    M.satisfy (\c -> Char.isLetter c && Char.isUpper c & c)

voyellLowParser:: Mparser Bool
voyellLowParser =       
    M.satisfy (\c -> Char.isLetter c && Char.isLower c )

voyellUpParser

{ --}
isVoyell :: Char -> Bool
isVoyell c =
    c `elem` "aàâeéèêëiîïoôuûùüyAÀÂEÉÈÊËIÎÏOÔUÛÙÜY"


isConsonant :: Char -> Bool
isConsonant c =
    c `elem` "bcçdfghjklmnpqrstvwxzBCÇDFGHJKLMNPQRSTVWXZ"


game :: IO ()
game = do
    firstName <- askFirstName 0
    putStrLn <| "\nBienvenue " ++ firstName
        ++ " dans le jeu \"TROUVE LE NOMBRE SECRET\" !\n"
    putStrLn
        "Le but de ce jeu est de trouver un nombre secret compris entre deux bornes."
    putStrLn "Pour commencer, je vais te demander de choisir l'intervalle."
    putStrLn
        "Ensuite je choisirai un nombre secret et tu essaieras de le deviner le plus vite possible.\n"
    maybeInterval <-
        runMaybeT <| do
            isub <- MaybeT askIntervalSub
            isup <- MaybeT <| askIntervalSup isub
            return (isub, isup)
    case maybeInterval of
        Nothing ->
            stopGame

        Just (isub, isup) -> do
            secret <- generateSecretNumber isub isup
            putStrLn <|
                "\nÇa y est, j'ai choisi le nombre secret dans l'intervalle ["
                ++ show isub
                ++ ", "
                ++ show isup
                ++ "]"
            trytoFind isub isup secret
    putStrLn <| "\nAu revoir " ++ firstName ++ " !\n"


trytoFind :: Int -> Int -> Int -> IO ()
trytoFind isub isup secret = do
    putStrLn "À ton avis, quel est ce nombre secret ?\n"
    tryAgain 1
    where
        tryAgain :: Int -> IO ()
        tryAgain nbTriesAcc = do
            maybeTry <- promptInt <| iemeFeminin nbTriesAcc ++ " proposition"
            maybeTry
                |> maybe stopGame (check nbTriesAcc)

        check :: Int -> Int -> IO ()
        check nbTriesAcc i
            | i == secret = bravo nbTriesAcc
            | i < secret = do
                putStrLn <| show i ++ " est TROP PETIT\n"
                tryAgain <| nbTriesAcc + 1
            | otherwise = do
                putStrLn <| show i ++ " est TROP GRAND\n"
                tryAgain <| nbTriesAcc + 1

        bravo :: Int -> IO ()
        bravo nbTries = do
            putStrLn line
            putStrLn msg
            putStrLn line
            where
                msg :: String
                msg =
                    "| Bravo, tu as trouvé en " ++ show nbTries
                        ++ pluriel nbTries " coup"
                        ++ " ! |"

                line :: String
                line =
                    repeat '-'
                        |> take (length msg - 2)
                        |> (++ "+")
                        |> ('+' :)


iemeMasculin :: Int -> String
iemeMasculin i =
    case i of
        1 ->
            "1er"

        2 ->
            "2nd"

        _ ->
            show i ++ "e"


iemeFeminin :: Int -> String
iemeFeminin i
    | i == 1 = "1re"
    | i == 2 = "2de"
    | otherwise = show i ++ "e"


pluriel :: Int -> String -> String
pluriel i
    | i > 1 = (++ "s")
    | otherwise = id


askIntervalSub :: IO (Maybe Int)
askIntervalSub = do
    isub <- promptInt "limite INFÉRIEURE de l'intervalle"
    isub
        |> maybe (return Nothing) (return <. Just)


askIntervalSup :: Int -> IO (Maybe Int)
askIntervalSup isub = do
    maybeIsup <- promptInt "limite SUPÉRIEURE de l'intervalle"
    case maybeIsup of
        Just isup ->
            if isub < isup then
                return <| Just isup

            else do
                writeWarning <|
                    "La limite supérieure doit être supérieure à la limite inférieure ("
                    ++ show isub
                    ++ ") !"
                askIntervalSup isub

        Nothing ->
            return Nothing


generateSecretNumber :: Int -> Int -> IO Int
generateSecretNumber isub isup = do
    sysTime <- SysTime.getSystemTime
    sysTime
        |> SysTime.systemNanoseconds
        |> toInteger
        |> fromInteger
        |> Random.mkStdGen
        |> Random.uniformR (isub, isup)
        |> fst
        |> return


writeWarning :: String -> IO ()
writeWarning msg = do
    putStrLn "    *"
    putStrLn <| "   /!\\  " ++ msg
    putStrLn "  -----"


stopGame :: IO ()
stopGame =
    putStrLn
        "\nTu n'as pas voulu répondre. Tu vas donc quitter le jeu. Dommage..."
