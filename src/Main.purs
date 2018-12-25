module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (awaitBody, runHalogenAff)

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

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.ul_ $
        --map (\x -> HH.li_ [HH.text x]) state.items,
        --this is not modular - which is why we do stuff with parent/children!
          map2 (\n x -> HH.li_ [HH.text x, HH.button [HE.onClick (HE.input_ (DeleteItem n))] [HH.text "Delete"]]) (0..((length (state.items))-1)) state.items,
        HH.input
          [ HP.value (state.curText)
          , HE.onValueChange (HE.input UpdateText)],
        HH.button
          [ HE.onClick (HE.input_ AddItem)] [ HH.text "Add"]
      ]

  eval :: Query ~> H.ComponentDSL State Query g
  eval q = case q of
    AddItem next -> do
      H.modify (\state -> state { curText = "", items = state.curText:state.items })
      pure next
    UpdateText str next -> do
      H.modify (\state -> state { curText = str})
      pure next
    DeleteItem n next -> do
      H.modify (\state -> state {items = fromMaybe state.items (deleteAt n state.items)})
      pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody -- ?
  H.runUI ui initialState body

{-
                                  , formValueHTML state.email
                                  , E.onValueChange (E.input UpdateEmail)
-}
