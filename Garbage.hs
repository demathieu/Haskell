--loop = 
--	show (Dates.Time 5 6 7)

loop  =
	let myDoodle = (Doodle "test" (Dates.Time 5 6 7))
	in show myDoodle

	
--instance Show (Mydoodle a) where 
--	show doodle = printDoodle doodle


--printDoodle :: Mydoodle a -> String
--printDoodle doodle = show (title doodle)


--loop = 
--	let newDoodle = (initialize "test")::Mydoodle a
--	in show newDoodle