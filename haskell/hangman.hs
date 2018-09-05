import Prelude
import System.IO
import Control.Monad
import Control.Exception
import Data.Char
import Data.List

echo :: IO String
echo = do
          x <- getLine
          putStrLn x
          return x

-- Simple primitives
mGetLine :: IO String
mGetLine = do
              x <- getChar
              if x == '\n' then 
                return []
              else
                  do 
                    xs <- mGetLine
                    return (x:xs)

secretGetLine :: IO String
secretGetLine = do
                  hSetEcho stdin False
                  x <- getChar
                  if x == '\n' then
                    return []
                  else
                    do
                      -- putChar '_'
                      xs <- secretGetLine
                      return (x:xs)

mPutStr :: String -> IO ()
mPutStr [] = return ()
mPutStr (x:xs) = do
                    putChar x
                    mPutStr xs
                    return ()

mPutStrLn :: String -> IO ()
mPutStrLn cs = do
                  mPutStr cs
                  putChar '\n'
                  return ()

getYesOrNo   :: String -> IO Bool
getYesOrNo q  = do 
                  putStrLn q
                  ans <- getLine
                  if (((toUpper . head) ans) == 'Y')
                    then return True
                    else return False

hangman :: IO ()
hangman = do
            putStrLn "Type the secret word below"
            putStr "> "
            word <- withEcho False getLine
            putChar '\n'
            -- guess word 10
            guessAcc word "" 10
            tryAgain <- getYesOrNo "Do you want to play again?"
            if tryAgain then hangman else return ()

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

mdiff          :: String -> String -> String
mdiff [] []     = ""
mdiff t1 []     = fmap (\x -> '_') t1
mdiff [] t2     = ""
mdiff (x:xs) t2 = ((if x `elem` t2 then x else '_'):(mdiff xs t2))

-- this looks a lot like omlette making recipe
-- cant find anything functional here
guess     :: String -> Int -> IO ()
guess word times 
            = do
                putStr "Make your guess: > "
                input <- getLine
                if (word == input) 
                  then do
                    putStrLn "You have guessed it correctly!"
                    return ()
                  else do
                    if ((times-1) == 0) 
                      then do
                        putStrLn "You have exceeded your attempts."
                        return ()
                      else do
                        putStrLn (mdiff word input)
                        guess word (times-1)

guessAcc       :: String -> String -> Int -> IO ()
guessAcc secret inputs attempts
                = do
                    putStr "Make your guess: > "
                    input <- getChar
                    putChar '\n'
                    if all (\x -> x `elem` (input:inputs)) secret
                      then do
                        putStrLn "You have guessed all letters"
                        return ()
                      else do
                        if attempts == 1
                          then do
                            putStrLn $ "Sorry, you no longer can guess. Secret is: " ++ secret
                            return ()
                          else do
                            putStrLn (mdiff secret (input:inputs))
                            guessAcc secret (input:inputs) (attempts-1)

