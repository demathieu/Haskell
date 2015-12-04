--import doodle
import qualified Data.Dates as Dates
import qualified Data.HashMap as HashMap
import qualified Data.List	as List



class Doodle d where
  initialize :: String -> d t
  add :: Ord t => (t,t) -> d t -> d t
  remove :: Int -> d t -> d t
  toogle :: String -> Int -> d t -> d t

class Pool p where
	freshKey :: (Ord k, Enum k) => p k (d t) -> k
 	--get :: Ord k => k -> p k (d t) -> Maybe (d t)
 	set :: Ord k => k -> (d t) -> p k (d t) -> p k (d t)

data Mydoodle date = MD { title :: String
							, timeslots :: [Mytimeslot date]
							} deriving (Show)

data Mytimeslot date = MTS { startTimeSlot :: date
								, endTimeSlot :: date
								, persones :: [String]
								} deriving (Show)

--data PoolInstance key doodle =  PI {keys :: [key], doodles :: [doodle]} deriving (Show)
data PoolInstance key doodle = PI {pool ::HashMap.HashMap key doodle} deriving (Show)

instance Doodle Mydoodle where
	initialize  title = (MD title [])
	add (beginTimeSlot, endTimeSlot) (MD title timeslots) = let newTimeSlot = (MTS beginTimeSlot endTimeSlot [])
	                                                        in (MD title (newTimeSlot:timeslots))
	remove index (MD title timeslots) =  let (ys,zs) = splitAt index timeslots   
	                                     in  (MD title (ys ++ (tail zs)))
	toogle name index (MD title timeslots) = let list@(xs,ys) = splitAt index timeslots
	                                             (MTS startTimeSlot endTimeSlot persones) = (head ys)
	                                             newPersonsList = removeElemOrAdd name persones
	                                             newTimeSlots = ((MTS startTimeSlot endTimeSlot newPersonsList) : ys)
		                                      in (MD title  newTimeSlots)
removeElemOrAdd :: Eq a => a -> [a] -> [a]
removeElemOrAdd elem list = if (List.elem elem list)
	                        then (filter(\x -> x /= elem) list)
	                        else (elem:list)

instance Pool PoolInstance where
	freshKey (PI [] _) =  toEnum 0
	freshKey (PI (k:_) _) = succ k
	set key doodle (PI keys doodles) =	(PI (key:keys) (doodle:doodles))
	--get key (Pi keys doodles) = let 

loop       = let timeSlot  = (MTS (Dates.Time 5 6 4) (Dates.Time 6 7 8) ["Hans"])
                 doodle = (MD "test" [timeSlot])
                 poolInstance  = (PI [] [])
          in freshKey (set 0 doodle poolInstance)::Int
		--in freshKey poolInstance ::Int
