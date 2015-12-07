import Doodle
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Time as T

data Mydoodle date = MD { title :: String
							, timeslots :: [Mytimeslot date]
							} 

data Mytimeslot date = MTS { startTimeSlot :: date
								, endTimeSlot :: date
								, persones :: [String]
								} 

data PoolInstance key doodle =  PI {keys :: [key], doodles :: [doodle]} deriving (Show)


instance (Show date) => Show (Mytimeslot date) where
	show (MTS startTimeSlot endTimeSlot persones) = printTimeSlot (MTS startTimeSlot endTimeSlot persones) 

instance (Show date) => Show (Mydoodle date) where
	show (MD title timeSlots) = printDoodle (MD title timeSlots)

instance Doodle Mydoodle where
	initialize  title = (MD title [])
	add (newBeginTimeSlot, newEndTimeSlot) (MD title []) = (MD title [(MTS newBeginTimeSlot newEndTimeSlot [])])
	add (newBeginTimeSlot, newEndTimeSlot) (MD title timeslots)
         | null ys  = (MD title (xs ++ [newTimeSlot]))
         | otherwise = let (MTS startTimeNext _ _) = (head ys) in if newEndTimeSlot < startTimeNext then (MD title (xs ++ (newTimeSlot : ys))) else (MD title timeslots)
         where newTimeSlot = (MTS newBeginTimeSlot newEndTimeSlot [])
               (xs,ys) = break (\(MTS _ endTimeSlot _ ) -> newBeginTimeSlot < endTimeSlot) timeslots
             --  (MTS startTimeNext _ _) = (head ys)  I can't be sure that ys is empty, therefor i can't take the head. For this reason i'm writing this ugly code above
              									-- if i would know a way to do (head ys) while accessing startTimeNext i would be able to remove the let and the if clause

	remove index (MD title timeslots) =  let (ys,zs) = splitAt index timeslots   
	                                     in  (MD title (ys ++ (tail zs)))
	toogle name index (MD title timeslots) = let list@(xs,ys) = splitAt index timeslots
	                                             (MTS startTimeSlot endTimeSlot persones) = (head ys)
	                                             newPersonsList = removeElemOrAdd name persones
	                                             newTimeSlots = (xs ++((MTS startTimeSlot endTimeSlot newPersonsList) : (tail ys)))
		                                      in (MD title  newTimeSlots)

removeElemOrAdd :: Eq a => a -> [a] -> [a]
removeElemOrAdd x (y:ys) | x == y = ys
                         | otherwise = y : removeElemOrAdd x ys
removeElemOrAdd x [] = [x]


instance Pool PoolInstance where
	freshKey (PI [] _) =  toEnum 0
	freshKey (PI (k:_) _) = succ k
	set key doodle (PI keys doodles) =	(PI (key:keys) (doodle:doodles))
	get key (PI keys doodles) = getDoodleFromPool key (PI keys doodles)

getDoodleFromPool :: Ord k => k -> PoolInstance k (d t) -> Maybe (d t)
getDoodleFromPool k pool 
                         | index == (-1) = Nothing
                         | otherwise = Just (doodles pool !! index)
                         where index = Maybe.fromMaybe (-1) (List.elemIndex k (keys pool))

printTimeSlot :: Show a => Mytimeslot a -> [Char]
printTimeSlot (MTS startTimeSlot endTimeSlot persones) = "| " ++ show startTimeSlot ++ " | " ++ show endTimeSlot ++ " | "++  (unwords $ map show  persones)++ "|"

printDoodle :: Show t => Mydoodle t -> [Char]
printDoodle (MD title timeSlots) = "| " ++ show title ++ " |\n" ++ (unlines $ map show timeSlots)


legepool :: PoolInstance Int (Mydoodle T.UTCTime)
legepool = (PI [] [])

main = run  legepool