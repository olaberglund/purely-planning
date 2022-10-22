module DayPicker where

import Prelude

import Data.Array (cons, delete, elem, length, replicate, singleton, splitAt, take)
import Data.Date (Date, Month(..), Weekday(..), month, weekday, year)
import Data.Date as Date
import Data.Enum (enumFromTo, fromEnum, pred, succ)
import Data.Maybe (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Matrix a = Array (Array a)

data Padded a = Padding | Data a

weekdays ∷ Array Date.Weekday
weekdays = [ Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]

type Input = Date

data Action = Next | Previous | Pick Date

type State = { picks ∷ Array Date, currentDate ∷ Date }

component ∷ ∀ q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState ∷ Input → State
initialState input = { picks: [], currentDate: input }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state =
  HH.section_
    [ HH.h1_ (mkText $ month (_.currentDate state))
    , HH.table_
        [ tableBody
        , HH.tfoot_
            [ HH.button
                [ HE.onClick \_ → Previous ]
                [ HH.text "Previous" ]
            , HH.button
                [ HE.onClick \_ → Next ]
                [ HH.text "Next" ]
            ]
        ]
    , HH.p_ (mkText state.picks)
    ]
  where
  tableBody ∷ ∀ w. HH.HTML w Action
  tableBody = HH.tbody_ (map HH.tr_ (cons tableHeads (dayMatrix (_.currentDate state))))

tableHeads ∷ ∀ w. Array (HH.HTML w Action)
tableHeads = map (HH.th_ <<< mkText) weekdays

dataRow ∷ ∀ w. Date → Array (HH.HTML w Action)
dataRow = datesOfMonth >=> Data >>> dataCell >>> pure >>> take (length weekdays)

dataCell ∷ ∀ w. Padded Date → HH.HTML w Action
dataCell m =
  case m of
    Data d → HH.td [ HE.onClick \_ → Pick d ] $ mkText (Date.day d)
    Padding → HH.td_ []

dayMatrix ∷ ∀ w. Date → Matrix (HH.HTML w Action)
dayMatrix date = chunks (length weekdays) paddedDays
  where
  paddedDays ∷ Array (HH.HTML w Action)
  paddedDays = paddedStart <> replicate (length weekdays * 6 - (length paddedStart)) paddedCell

  paddedStart ∷ Array (HH.HTML w Action)
  paddedStart = insertMany (fromEnum (firstWeekDay date) - 1) paddedCell (dataRow date)

  paddedCell ∷ HH.HTML w Action
  paddedCell = (dataCell (Padding ∷ Padded Date))

insertMany ∷ ∀ a. Int → a → Array a → Array a
insertMany n x xs = replicate n x <> xs

mkText ∷ ∀ w i a. Show a ⇒ a → Array (HH.HTML w i)
mkText = singleton <<< HH.text <<< show

datesOfMonth ∷ Date → Array Date
datesOfMonth date = map (Date.canonicalDate (year date) (month date)) (enumFromTo bottom lastDay)
  where
  lastDay ∷ Date.Day
  lastDay = (Date.lastDayOfMonth (Date.year date) (Date.month date))

chunks ∷ ∀ a. Int → Array a → Matrix a
chunks _ [] = []
chunks n xs =
  let
    { before, after } = splitAt n xs
  in
    cons before (chunks n after)

nextMonth ∷ Date → Date
nextMonth = adjustToMonth <*> fromMaybe January <<< succ <<< month

prevMonth ∷ Date → Date
prevMonth = adjustToMonth <*> fromMaybe December <<< pred <<< month

adjustToMonth ∷ Date → Month → Date
adjustToMonth date m = Date.canonicalDate (year date) m bottom

firstDateOfMonth ∷ Date → Date
firstDateOfMonth = adjustToMonth <*> month

firstWeekDay ∷ Date → Weekday
firstWeekDay = weekday <<< firstDateOfMonth

handleAction ∷ ∀ cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Next → do
    H.modify_ \s → s { currentDate = nextMonth s.currentDate }
  Previous → do
    H.modify_ \s → s { currentDate = prevMonth s.currentDate }
  Pick d → do
    H.modify_ \s → s { picks = if d `elem` s.picks then delete d s.picks else cons d s.picks }
