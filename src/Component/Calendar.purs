module Calendar where

import Prelude

import Data.Array (cons, filter, last, length, mapMaybe, mapWithIndex, replicate, singleton, splitAt)
import Data.Date (Date, Month(..), Weekday(..), month, weekday, year)
import Data.Date as Date
import Data.DateTime (DateTime, date)
import Data.DateTime as Time
import Data.Enum (enumFromTo, fromEnum, pred, succ)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (take)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (css)

type Matrix a = Array (Array a)

data Padded a = Padding | Data a

weekdays ∷ Array Date.Weekday
weekdays = enumFromTo bottom top

type Input = DateTime

data Action = Next | Previous | Pick Date

type State = { currentDate ∷ Date, time ∷ Time.Time }

type Output = Date

-- type Slots = (formParent ∷ ∀ q. slot q FP.Output)

calendar ∷ ∀ q m. H.Component q Input Output m
calendar =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState ∷ Input → State
initialState input = { currentDate: (date input), time: (Time.time input) }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state =
  HH.section [ css "calendar-wrapper" ]
    [ HH.table [ css "calendar-body" ]
        [ HH.caption [ css "calendar__caption" ]
            [ HH.text $ show (Date.month state.currentDate) <> " " <> show (fromEnum $ Date.year state.currentDate)
            ]
        , HH.thead [ css "calendar__thead" ] tableHeads
        , tableBody
        ]
    , HH.div [ css "calendar__controls" ]
        [ HH.button
            [ css "button", HE.onClick \_ → Previous ]
            [ HH.text "<" ]
        , HH.button
            [ css "button", HE.onClick \_ → Next ]
            [ HH.text ">" ]
        ]
    ]
  where
  tableBody ∷ ∀ w. HH.HTML w Action
  tableBody = HH.tbody [ css "calendar__body" ] (map (HH.tr [ css "calendar__row" ]) (dayMatrix (_.currentDate state)))

tableHeads ∷ ∀ w. Array (HH.HTML w Action)
tableHeads = cons (th []) $ map (th <<< singleton <<< HH.text) (map (take 3 <<< show) weekdays)
  where
  th = HH.th [ css "calendar__head" ]

dataRow ∷ ∀ w. Date → Array (HH.HTML w Action)
dataRow = datesOfMonth >=> Data >>> dataCell >>> pure

dataCell ∷ ∀ w. Padded Date → HH.HTML w Action
dataCell m =
  case m of
    Data d → HH.td [ css "calendar__day", HE.onClick \_ → Pick d ] $ mkText (fromEnum $ Date.day d)
    Padding → HH.td [ css "calendar__day--empty" ] []

dayMatrix ∷ ∀ w. Date → Matrix (HH.HTML w Action)
dayMatrix date = mapWithIndex addWeek (chunks (length weekdays) (paddedDays))
  where
  addWeek ∷ Int → Array (HH.HTML w Action) → Array (HH.HTML w Action)
  addWeek i =
    -- gymnastics for the first month of the year
    -- when the first days aren't actually part of 
    -- the first year (but the last of the previous year)
    if currentBaseWeek > 51 then
      addWeekHeader (isOutside i) (max ((currentBaseWeek + i) `mod` (currentBaseWeek + 1)) i)
    else
      addWeekHeader (isOutside i) (currentBaseWeek + i)

  currentBaseWeek = week (firstDateOfMonth date)

  isOutside ∷ Int → Boolean
  isOutside i = i * (length weekdays) < length start

  addWeekHeader ∷ Boolean → Int → Array (HH.HTML w Action) → Array (HH.HTML w Action)
  addWeekHeader b w = cons (HH.th [ css "calendar__head" ] [ HH.text $ if b then show w else "" ])

  paddedDays ∷ Array (HH.HTML w Action)
  paddedDays = start <> replicate (42 - length start) paddedCell

  start ∷ Array (HH.HTML w Action)
  start = insertMany (fromEnum (firstWeekDay date) - 1) paddedCell (dataRow date)

  paddedCell ∷ HH.HTML w Action
  paddedCell = (dataCell (Padding ∷ Padded Date))

insertMany ∷ ∀ a. Int → a → Array a → Array a
insertMany n x xs = replicate n x <> xs

mkText ∷ ∀ w i a. Show a ⇒ a → Array (HH.HTML w i)
mkText = singleton <<< HH.text <<< show

datesOfMonth ∷ Date → Array Date
datesOfMonth date = enumFromTo (firstDateOfMonth date) (lastDateOfMonth date)

lastDateOfMonth ∷ Date → Date
lastDateOfMonth date = fromMaybe date (last daysOfMonth)
  where
  -- map an array of all days [1, 31] to an exact date, if there is one,
  -- and keep only the successful dates
  daysOfMonth = mapMaybe (Date.exactDate (year date) (month date)) (enumFromTo bottom top)

chunks ∷ ∀ a. Int → Array a → Matrix a
chunks _ [] = []
chunks n xs =
  let
    { before, after } = splitAt n xs
  in
    cons before (chunks n after)

nextMonth ∷ Date → Date
nextMonth d = case succ (month d) of
  Just m → adjustToMonth d m
  Nothing → Date.canonicalDate (fromMaybe (year d) (succ $ year d)) January bottom

prevMonth ∷ Date → Date
prevMonth d = case pred (month d) of
  Just m → adjustToMonth d m
  Nothing → Date.canonicalDate (fromMaybe (year d) (pred $ year d)) December bottom

adjustToMonth ∷ Date → Month → Date
adjustToMonth date m = Date.canonicalDate (year date) m bottom

firstDateOfMonth ∷ Date → Date
firstDateOfMonth = adjustToMonth <*> month

firstWeekDay ∷ Date → Weekday
firstWeekDay = weekday <<< firstDateOfMonth

handleAction ∷ ∀ cs m. Action → H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  Next → do
    H.modify_ \s → s { currentDate = nextMonth s.currentDate }
  Previous → do
    H.modify_ \s → s { currentDate = prevMonth s.currentDate }
  Pick d → do
    H.raise d

week ∷ Date → Int
week d = if nbrMondaysUpUntilDate == 0 then week lastDateOfLastYear else nbrMondaysUpUntilDate
  where
  lastDateOfLastYear = (Date.canonicalDate (fromMaybe (year d) (pred $ year d)) top top)
  firstDayOfYear = (firstDateOfMonth (adjustToMonth d January))
  nbrMondaysUpUntilDate = length $ filter (weekday >>> (==) Monday) (enumFromTo firstDayOfYear d)
