module Main where

import Prelude

import Data.Array (filter, (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Spork.Html (Html, InputType(..))
import Spork.Html as H
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp

type Todo = { id :: Int, content :: String, isDone :: Boolean, isEditing :: Boolean}

type Model = {currentId :: Int, current:: String, elements :: Array Todo }

data Action = Add Todo | Rem Int | Current String | Update Todo

update ∷ Model -> Action -> Model
update model (Add t) = model {currentId= model.currentId + 1, elements= t : model.elements, current=""}
update model (Rem t) =  model {elements= filter (\x -> x.id /= t) model.elements }
update model (Current c) = model {current= c}
update model (Update t) = model {elements= map (\x -> if x.id == t.id then x {isDone=t.isDone, isEditing=t.isEditing, content=t.content} else x) model.elements}

render ∷ Model -> Html Action
render i =
  H.div [ H.classes $ "container-fluid" : []][
      H.div[][
        H.input [
          H.onValueInput $ \x -> Just $ Current x,
          H.value i.current
        ],
        H.button [
          H.onClick $ \x -> Just $ Add {id: i.currentId, content: i.current, isDone: false, isEditing: false },
          H.classes $ "btn" : "btn-primary": []
        ] [
          H.text "Add"
        ]
      ],
      H.ul [] $ map (\x -> element x) i.elements
 ]

element :: Todo -> Html Action
element todo = H.div [] [
  text todo,
  H.input [H.type_ InputCheckbox, H.onChecked $ \x -> Just $ Update todo {isDone= x} ],
  H.span [
    H.onClick $ \x -> Just $ Rem (todo.id),
    H.classes $ "btn" : "btn-danger" : []
  ] [ H.text "x"],
  H.span[
    H.onClick $ \x -> Just $ Update todo {isEditing = not todo.isEditing},
    H.classes $ "btn" : "btn-primary" : []
  ] [ H.text "edit"]
]

text :: Todo -> Html Action
text todo = 
  if todo.isEditing then 
    H.input[
      H.onValueInput $ \x -> Just $ Update todo { content= x },
      H.value todo.content
    ]
  else 
    H.p [H.style $ "display: inline;" <> if todo.isDone then "text-decoration: line-through;" else ""][ H.text todo.content] 

app ∷ PureApp Model Action
app = { update, render, init: {currentId: 0, current: "", elements: []} }

main ∷ Effect Unit
main = void $ PureApp.makeWithSelector app "#app"
