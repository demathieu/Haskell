
import qualified Data.List as List
--removeElemOrAdd y list@(y:_) = filter (\x -> x /= y) list
--removeElemOrAdd elem list@(x:[]) = (list:elem)


removeElemOrAdd :: Eq a => a -> [a] -> [a]
removeElemOrAdd elem list = if (List.elem elem list)
	                        then (filter(\x -> x /= elem) list)
	                        else (elem:list)


loop = removeElemOrAdd "test" ["hans","ernie"]
loop1 = removeElemOrAdd "test" ["hans","ernie","test"]



toggle name index (MD title timeslots) = let (MTS startTimeSlot endTimeSlot persones) = timeslots !! index
	                                              newPersonsList = removeElemOrAdd name persones
	                                              newTimeSlots = timeslots
		                                     in (MD title 

toggle name index (MD title timeslots) = let list@(xs:ys) = splitAt index timeslots
                                             (MTS startTimeSlot endTimeSlot persones) = (head ys)
	                                         newPersonsList = removeElemOrAdd name persones
	                                         newTimeSlots = ((MTS startTimeSlot endTimeSlot newPersonsList) : list)
		                                     in (MD title  newTimeSlots)