module Main where

import Prelude

import Effect (Effect)

import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements as HH
import Halogen.HTML.Properties as HP
import Halogen.Aff.Util (awaitBody, runHalogenAff)
--import Halogen.Aff.Driver (runUI)
import Halogen.VDom.Driver (runUI)

import Data.Array
import Data.Tuple
import Data.Maybe

map2 :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
map2 f li1 li2 = map (\x -> f (fst x) (snd x)) $ zip li1 li2

--define State
type State = { curText :: String, items :: Array String }

initialState :: State
initialState = { curText: "", items: [] }

data Query a = AddItem a | UpdateText String a | DeleteItem Int a
--define Query (Action)

ui :: forall g. Applicative g => H.Component HTML Query State Void g
ui = H.component { initialState : initialState', render, eval, receiver }
  where
  initialState' :: State -> State
  initialState' = const initialState
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.ul_ $
        --map (\x -> HH.li_ [HH.text x]) state.items,
        --this is not modular - which is why we do stuff with parent/children!
          map2 (\n x -> HH.li_ [HC.text x, HH.button [HE.onClick (HE.input_ (DeleteItem n))] [HC.text "Delete"]]) (0..((length (state.items))-1)) state.items,
        HH.input
          [ HP.value (state.curText)
          , HE.onValueChange (HE.input UpdateText)],
        HH.button
          [ HE.onClick (HE.input_ AddItem)] [ HC.text "Add"]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void g
  eval q = case q of
    AddItem next -> do
      _ <- H.modify (\state -> state { curText = "", items = state.curText:state.items })
      pure next
    UpdateText str next -> do
      _ <- H.modify (\state -> state { curText = str})
      pure next
    DeleteItem n next -> do
      _ <- H.modify (\state -> state {items = fromMaybe state.items (deleteAt n state.items)})
      pure next
  receiver = const Nothing

--(H.HalogenEffects ())
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody -- ?
  runUI ui initialState body

{-
                                  , formValueHTML state.email
                                  , E.onValueChange (E.input UpdateEmail)
-}
