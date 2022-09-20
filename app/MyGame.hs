{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module MyGame where
import Data.List
import Data.Ord ()
import Data.Function (on)
import Data
import Data.List.ZigZag ( diagonals)


initialBoardState :: Row -> Col -> BoardState
initialBoardState r c = [ Slot x y Empty | x <- [1..r], y <- [1..c]]

changeBoardState :: BoardState -> Slot -> BoardState
changeBoardState bs (Slot x y slot)
 | validateMove (Slot x y slot) bs       = map (\s -> if (x == rPos s) && (y == cPos s) 
                                                    then Slot x y slot
                                                    else s) bs
 | otherwise  = bs

validateMove :: Slot -> BoardState -> Bool
validateMove (Slot x y s) bs       =  f bs == 1   -- if (f gs) == 1 then True else False
 where 
    validatePos e                   = rPos e == x && cPos e == y && slotState e == Empty  
    f                               = length . filter validatePos

makeMove :: BoardState -> Col -> Slot 
makeMove bs c = Slot (rPos whichRow) c (Player $ whoseTurn bs) 
    where 
        checkCol e   = cPos e == c && slotState e == Empty
        whichRow     = last $ filter checkCol bs

-- MY CHECKER FUNCTIONS:
whoseTurn :: BoardState -> Player
whoseTurn bs
 | red_count > yellow_count = Yellow
 | otherwise = Red
           where
      red_count = length $ filter (\(Slot _ _ p) ->  p == Player Red) bs
      yellow_count = length $ filter (\(Slot _ _ p) ->  p == Player Yellow) bs

anyColWins :: BoardState -> Bool
anyColWins bs = 
    let x = checkGameState  (mySliding 4 . fst $ columnSort bs) (mySliding 4 . snd $ columnSort bs)
 in x /= Running && x /= Draw

anyRowWins :: BoardState -> Bool
anyRowWins bs = 
    let x = checkGameState  (mySliding 4 $ slotState <$> bs) (mySliding 4 $ rPos <$> bs)
 in x /= Running && x /= Draw

anyDiagWins :: BoardState -> Bool 
anyDiagWins bs = 
    let diags = concat $ myReplace . snd $ diagSort bs
        states = concat . fst $ diagSort bs
        x = checkGameState  (mySliding 4 $ states) (mySliding 4 $ diags)
 in x /= Running && x /= Draw       

earlyTie :: BoardState -> Bool
earlyTie bs =
    let rows = checkGameState  (mySliding 4 $ slotState <$> bs) (mySliding 4 $ rPos <$> bs)
        cols = checkGameState  (mySliding 4 . fst $ columnSort bs) (mySliding 4 . snd $ columnSort bs)
    in rows == Draw && cols == Draw 
        
checkGameState :: [[SlotState]] -> [[Int]] -> GameState
checkGameState [] [] = Running
checkGameState lst@(s:ss) (p:ps) = 
    if foo lst s p /= Running 
        then foo lst s p
    else checkGameState ss ps
  where
 foo entire x y
  | all (== Player Red) x && isConsecutive y =        Won Red 
  | all (== Player Yellow) x && isConsecutive y =     Won Yellow
  | all ( \four ->  elem (Player Red) four && 
    elem (Player Yellow) four ) entire =              Draw  -- if all of them contain at least a Y and a R
  | otherwise =                                       Running


-- MY DRAWING FUNCTIONS:

showBoard :: BoardState -> String
showBoard bs = 
 let x = makeTuple bs in 
    x >>= \x ->        
        if fst x < 0 && snd x == Empty 
            then ". " 
        else if fst x < 0 && snd x == Player Red 
            then "R "
        else if fst x < 0 && snd x == Player Yellow 
            then "Y "
        else if fst x > 0 && snd x == Player Red 
            then "R" ++ "\n"
        else if fst x > 0 && snd x == Player Yellow   
            then "Y" ++ "\n"
        else "." ++ "\n"
 
showTrailer :: BoardState -> String
showTrailer bs = let n = checkNumCol $ makeTuple bs 
    in concat $ replicate n "- " ++ ["\n"] 
    ++ ((++" ") <$>  map show (take n [1 .. ])) 
 where 
    checkNumCol (x:xs) = 
        if fst x > 0 
        then fst x + 1 
        else checkNumCol xs
    checkNumCol [] = 0  


-- MY HELPER FUNCTIONS:

isConsecutive :: Eq a => [a] -> Bool
isConsecutive (x:xs) = all (==x) xs
isConsecutive [] = undefined 

mySliding :: Int -> [a] -> [[a]]
mySliding window [] = []
mySliding window ls@(x:xs) = 
    if length ls >= window 
    then take window ls : mySliding window xs 
    else mySliding window xs

columnSort :: BoardState -> ([SlotState], [Int])
columnSort bs = unzip . mySort $ zip (slotState <$> bs) (cPos <$> bs)
    where 
        mySort :: Ord b => [(a, b)] -> [(a, b)]
        mySort = sortBy (compare `on` snd) -- Sort by secnd element

makeTuple :: BoardState -> [(Col, SlotState)]
makeTuple bs = zip (dif (cPos <$> bs) ++ [1]) (slotState <$> bs)
    where dif l = zipWith (-) l (tail l)

diagSort :: BoardState -> ([[SlotState]], [[Int]]) -- get all diagonals longer than 3 elements
diagSort bs = 
    let cols = (cPos <$> bs)
        states = (slotState <$> bs)
        in
     ( filter (\x -> length x > 3) $ 
        (diagonals $ mySplit (maximum cols) states) ++ (diagonals $ reverse $ mySplit (maximum cols) states),

      filter (\x -> length x > 3) $ 
        (diagonals $ mySplit (maximum cols) cols) ++ ( diagonals $ reverse $ mySplit (maximum cols) cols) )
   
    where mySplit :: Int -> [a] -> [[a]]
          mySplit _ [] = []
          mySplit n xs = take n xs:(mySplit n $ drop n xs) 

myReplace :: [[b]] -> [[Int]]
myReplace (x:xs) = replicate (length x) (length xs) : myReplace xs
myReplace [] = []   




