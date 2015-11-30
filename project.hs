--import doodle
import qualified Data.Dates as Dates


--loop = 
--	show (Dates.Time 5 6 7)

class Doodle d where
  initialize :: String -> d t
 -- add :: Ord t => (t,t) -> d t -> d t
 --remove :: Int -> d t -> d t
 --toogle :: String -> Int -> d t -> d t



data Mydoodle date = Doodle { title :: String
						, date :: date} deriving (Show)

loop  =
	let myDoodle = (Doodle "test" (Dates.Time 5 6 7))
	in show myDoodle

instance Doodle Mydoodle where
	initialize  title = 
		let newDoodle = (Doodle title (Dates.Time 5 6 7))::Mydoodle a
		in newDoodle

--instance Show (Mydoodle a) where 
--	show doodle = printDoodle doodle


--printDoodle :: Mydoodle a -> String
--printDoodle doodle = show (title doodle)


--loop = 
--	let newDoodle = (initialize "test")::Mydoodle a
--	in show newDoodle