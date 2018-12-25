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
import Halogen.VDom.Driver (runUI)

import Data.Array
import Data.Tuple
import Data.Maybe

map2 :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
map2 f li1 li2 = map (\x -> f (fst x) (snd x)) $ zip li1 li2

--define State
type State = { curText :: String, items :: Array (Tuple String String) }

initialState :: State
initialState = { curText: "", items: [] }

dispPair :: Tuple String String -> Array String
dispPair (Tuple txt resp) = ["> " <> txt, resp]

--define Query (Action)
data Query a = AddItem a | UpdateText String a

ui :: forall g. Applicative g => H.Component HTML Query State Void g
ui = H.component { initialState : initialState', render, eval, receiver }
  where
  initialState' :: State -> State
  initialState' = const initialState

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.div_ $ map (\x -> HH.p_ [HC.text x]) $ concat $ map dispPair $ reverse state.items,
        HH.input
          [ HP.value (state.curText)
          , HE.onValueChange (HE.input UpdateText)],
        HH.button
          [ HE.onClick (HE.input_ AddItem)] [ HC.text "Say!"]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void g
  eval q = case q of
    AddItem next -> do
      _ <- H.modify (\state -> state { curText = "", items = let x=state.curText in (Tuple x (respond x)):state.items })
      pure next
    UpdateText str next -> do
      _ <- H.modify (\state -> state { curText = str})
      pure next
  receiver = const Nothing

--response function here
respond :: String -> String
respond = identity

--main
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody -- ?
  runUI ui initialState body
