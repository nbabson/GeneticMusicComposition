module Composer where
import Haskore
import Data.Ratio((%))
import Data.List
import Data.Ord
import Music
import System.Random
import System.IO
import Control.Monad
import Control.Monad.State
import System.Process
import System.Directory
import System.Environment
import System.Exit
import Data.Array.IO
--import IOActions
--------------------------------------------------------------------------
-- Taken from : wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
               where
                 n = length xs
                 newArray :: Int -> [a] -> IO (IOArray Int a)
                 newArray n xs = newListArray (1,n) xs

inIO  :: (a -> b) -> a -> IO b
inIO f = return . f
---------------------------------------------------------------------------


-- 2 octaves of C Major scale used to generate random musical measures
notes = [(B,3),(C,4),(D,4),(E,4),(F,4),(G,4),(A,4),(B,4),
         (C,5),(D,5),(E,5),(F,5),(G,5),(A,5),(B,5),(C,6)]

-- Starting time of a note in a measure         
start = [(0%16), sn, en, den,qn, (5%16), dqn,
         (7%16), hn, (9%16), (5%8), (11%16),
         dhn, (13%16), (7%8), (15%16)]

-- First second and third positions in 3-tuple         
fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,x,_) = x

thrd' :: (a,b,c) -> c
thrd' (_,_,x) = x

-- Duration of a note.  Dotted half notes and whole notes are half as likely as other durations
dur' = [sn,sn,dsn,dsn,en,en,den,den,qn,qn,dqn,dqn,hn,hn,dhn,wn]

-- Takes an integer seed and creates an infinite list of random numbers between 0 and 15
randNums :: Int -> [Int]
randNums gen = randomRs (0,15) (mkStdGen gen)

-- Make a new random measure with between 0 and 15 notes   
initMeasure rands = makeTones (1*(head rands)) (drop 1 rands)

-- Make a list of 3-tuples, each defining a note, a start time, and a duration 
makeTones 0 _ = []
makeTones n rands = ((notes !! (head rands)), (start !! (head(tail (take 2 rands)))), (dur' !! (head ( reverse ( take 3 rands))))) : makeTones (n-1) (drop 3 rands)
   
-- Turn a list of 3-tuples into Music class object                     
makeMusic (x:[]) = Rest (snd' x) :+: Note (fst' x) (thrd' x) []
makeMusic (x:xs) = (Rest (snd' x) :+: Note (fst' x) (thrd' x) []) :=: makeMusic xs    
makeMusic [] = Rest (0%4) 

-- Create a list of n lists of 3-tuples defining random measures                          
initPopulation 0 rands = []
initPopulation n rands = (initMeasure rands): initPopulation (n-1) (drop 100 rands)

-- Turn list of lists of 3-tuples into list of Music objects 
makePopulation pop = map makeMusic pop

-- Get fitness ranking from the user
getInt :: IO Int
getInt = do line <- getLine
            if (length line) /= 1 || head line < '0' || head line > '9'
              then do
                putStr "Invalid input.  Try again: "
                getInt
            else do   
              return (read line :: Int)

-- Play all the Music objects in a list
--play :: [Music] -> IO [Int]
play []  = return [] 
play (x:xs) = do playWin x     -- Removed during testing
                 putStr "Rate from 0 (lowest) to 9 : "
                 do
                   a <- getInt
                   theRest <- play xs
                   return (a :theRest)

-- Play eight measure song
playSongs [] = return []
playSongs ((m0,m1,m2,m3,m4,m5,m6,m7):ms) = do
        playWin ((makeMusic m0) :=: (Rest (1%1) :+: (makeMusic m1))
                                :=: (Rest (2%1) :+: (makeMusic m2))
                                :=: (Rest (3%1) :+: (makeMusic m3))
                                :=: (Rest (4%1) :+: (makeMusic m4))
                                :=: (Rest (5%1) :+: (makeMusic m5))
                                :=: (Rest (6%1) :+: (makeMusic m6))
                                :=: (Rest (7%1) :+: (makeMusic m7)))
        putStr "Rate from 0 (lowest) to 9 : "
        do
          a <- getInt
          theRest <- playSongs ms
          return (a :theRest)

-- This function is a relic, currently unused
-- Play two measure pieces encoded as a list of Music tuples
playDouble [] _ = return []
playDouble _ [] = return []
playDouble (x:xs) (y:ys) = do
                 playWin ((makeMusic x) :=: (Rest (1%1) :+: (makeMusic y)))
                 putStr "Rate from 0 (lowest) to 9 : "
                 do
                   a <- getInt
                   theRest <- playDouble xs ys
                   return (a :theRest)

-- Turn list of Music objects into a single object   
connectMusic (x:xs) = x :+: connectMusic xs
connectMusic [] = Rest (0%4)
   
-- Create initial population of random measures
pop = initPopulation 16 rands
measures = makePopulation pop
rands' = randNums 612 
rands'' = randNums 611
rands = randNums 615 
--fitness = play measures
--measures2 = readFile "GA\\init.txt" 


--mutate :: Int -> Music -> [Int] -> Music  
mutate 10 x rand = x
mutate rank x (y:ys) | y < 5     = mutate (rank+1) (dropNote x ys) (drop 2 ys)
                     | y < 10    = mutate (rank+1) (addNote x ys) (drop 7 ys)
                     | otherwise = mutate (rank+1) (changeNote x ys) (drop 7 ys)

--dropNote :: Music -> [Int] -> Music   
dropNote x (y:ys) | (length x) == 0  = x
                  | otherwise = take num x ++ drop (num+1) x
                      where num = (y * 7) `mod` (length x)  

--addNote :: Music -> [Int] -> Music
addNote x y = x ++ (makeTones 1 y)

--changeNote :: Music -> [Int] -> Music    
changeNote  x (y:ys) = addNote (dropNote x (y:ys)) ys    
 
--makeNextGen :: [(Int, Music)] -> [Int] -> [Music]
makeNextGen [] rand = []   
--makeNextGen ((rank,x):xs) rand = (mutate (rank*5) x rand):(makeNextGen xs rand)
makeNextGen ((rank,x):xs) rand = (mutate 0 x rand):(makeNextGen xs rand)

--removeRank :: [(Int,Music)] -> [Music]
removeRank [] = [] 
removeRank ((_,x):xs) = x: (removeRank xs)

--remove worst 5 measures, returning remaining measures zipped with their rankings
removeDead fitness measures = do drop 8 (sortBy (comparing fst) (zip fitness measures))

removeDead2 fitness songs = do drop 4 (sortBy (comparing fst) (zip fitness songs))

--evolAlg :: [Int] -> [Music] -> [Music]                  
evolAlg rand measures pop gen = do
         putStr "Generation: "
         print (gen)
         fitness <- play measures
         let survivors = removeDead fitness pop
         let nextgen = (makeNextGen survivors rand) ++ removeRank survivors
         shuffled <- shuffle nextgen
         if (minimum (drop 8 (sort fitness)) > 8) then return (removeRank (removeDead fitness pop)) 
         else evolAlg (drop 100 rand) (makePopulation shuffled) shuffled (gen+1)

-- Two songs combine to make two new children   
mate (m0,m1,m2,m3,m4,m5,m6,m7) (n0,n1,n2,n3,n4,n5,n6,n7) (r:rs)    
      | n == 0 = mate (m0,m1,m2,m3,m4,m5,m6,m7) (n0,n1,n2,n3,n4,n5,n6,n7) rs -- try again
      | n == 1 = (m0,n1,n2,n3,n4,n5,n6,n7) : (n0,m1,m2,m3,m4,m5,m6,m7) : []
      | n == 2 = (m0,m1,n2,n3,n4,n5,n6,n7) : (n0,n1,m2,m3,m4,m5,m6,m7) : []
      | n == 3 = (m0,m1,m2,n3,n4,n5,n6,n7) : (n0,n1,n2,m3,m4,m5,m6,m7) : []
      | n == 4 = (m0,m1,m2,m3,n4,n5,n6,n7) : (n0,n1,n2,n3,m4,m5,m6,m7) : []
      | n == 5 = (m0,m1,m2,m3,m4,n5,n6,n7) : (n0,n1,n2,n3,n4,m5,m6,m7) : []
      | n == 6 = (m0,m1,m2,m3,m4,m5,n6,n7) : (n0,n1,n2,n3,n4,n5,m6,m7) : []
      | n == 7 = (m0,m1,m2,m3,m4,m5,m6,n7) : (n0,n1,n2,n3,n4,n5,n6,m7) : []
      | otherwise = []
            where n = r `div` 2

-- Mutate song by shifting n positions to the left        
shift (m0,m1,m2,m3,m4,m5,m6,m7) r
      | n == 0 = (m0,m1,m2,m3,m4,m5,m6,m7)
      | n == 1 = (m1,m2,m3,m4,m5,m6,m7,m0)
      | n == 2 = (m2,m3,m4,m5,m6,m7,m0,m1)
      | n == 3 = (m3,m4,m5,m6,m7,m0,m1,m2)
      | n == 4 = (m4,m5,m6,m7,m0,m1,m2,m3)
      | n == 5 = (m5,m6,m7,m0,m1,m2,m3,m4)
      | n == 6 = (m6,m7,m0,m1,m2,m3,m4,m5)
      | n == 7 = (m7,m0,m1,m2,m3,m4,m5,m6)
            where n = r `div` 2

reproduce [] _ = []   
reproduce xs r = (mate (xs !! 0)  (xs !! 3) r) ++ (mate (xs !! 2) (xs !! 1) (drop 5 r))  

-- New songs have 1/3 chance of mutating by shifting some number of measures to the left
shifter [] _ = []         
shifter (x:xs) (r:rs) | r < 5     = (shift x (head rs)) : (shifter xs (drop 2 rs))
                      | otherwise = x : (shifter xs rs)                                                      

--genAlg1 :: [Int] -> [[((a,b),c,d)]] -> [[((a,b),c,d)]] -> Int -> [[((a,b),c,d)]]   
genAlg rand songs gen = do
         putStr "Generation: "
         print (gen)
         fitness <- playSongs songs
         let survivors = removeDead2 fitness songs
         let children = (reproduce (removeRank survivors) rand)
         let nextgen = (shifter children (drop 20 rand))  ++ (removeRank survivors)
         shuffled <- shuffle nextgen
         if (head (reverse (drop 4 (sort fitness))) == 9) then return (head (reverse (removeRank (removeDead2 fitness songs))))
         else genAlg (drop 100 rand) shuffled (gen+1)

--  Randomly assemble measures into 8 measure songs  
m n r p = p !! ((head (drop n r)) `div` 2)

-- r = rands, p = measures, n = count   
makeSongs _ _ 8 = []   
makeSongs r p n = ((m 0 r p),(m 1 r p),(m 2 r p),(m 3 r p),(m 4 r p),(m 5 r p),(m 6 r p),(m 7 r p)):(makeSongs (drop 10 r) p (n+1))
       --   where m n r = measures !! ((head (drop n r)) div 2)

-- Turn 8-tuple of measures into music object
makeSong (m0,m1,m2,m3,m4,m5,m6,m7) = do
        (makeMusic m0) :=: (Rest (1%1) :+: (makeMusic m1))
                       :=: (Rest (2%1) :+: (makeMusic m2))
                       :=: (Rest (3%1) :+: (makeMusic m3))
                       :=: (Rest (4%1) :+: (makeMusic m4))
                       :=: (Rest (5%1) :+: (makeMusic m5))
                       :=: (Rest (6%1) :+: (makeMusic m6))
                       :=: (Rest (7%1) :+: (makeMusic m7))
                                   

main = do 
         putStrLn "COMPOSER genetic music composition program"
         putStrLn ""
    --     outFile <- openFile "GA\\init.txt" WriteMode
      --   hPrint outFile (measures)
        -- writeMidi (connectMusic measures) "GA\\init"  -- Write MIDI file of original random measures
      --   hClose outFile
         putStrLn "Entering evolutionary algorithm: evolution will continue"
         putStrLn "until half of the musical fragments have a fitness ranking of 9."
         putStrLn ""
         goodMeasures <- evolAlg rands' measures pop 1
       --  writeMidi (connectMusic (makePopulation goodMeasures)) "GA\\evol"
         putStrLn ""
         putStrLn ("Entering genetic algorithm.  Genetic combination and mutation")
         putStrLn ("will continue until any piece of music attains a fitness ranking of 9.")
         putStrLn ""
         goodSong <- genAlg (drop 100 rands'') (makeSongs rands'' goodMeasures 0) 1 
         writeMidi (makeSong goodSong) "finishedSong4"
  --       outFile2 <- openFile "GA\\song.txt" WriteMode
    --     hPrint outFile2 (goodSong)
      --   hClose outFile2 
         putStrLn ""
         putStrLn "Exiting composer program"

   
