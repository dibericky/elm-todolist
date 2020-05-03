module TodoList exposing (main)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view >> toUnstyled
        , update = update
        }


type alias TodoItem =
    { name : String
    , isDone : Bool
    }


type alias Model =
    { items : List TodoItem
    , itemToAdd : TodoItem
    }


initialModel : Model
initialModel =
    { items = [ TodoItem "item1" False, TodoItem "item2" True ]
    , itemToAdd = TodoItem "" False
    }


type Msg
    = SetAsTodo String
    | SetAsDone String
    | AddItem
    | OnAddInputChange String


updateItem : Msg -> TodoItem -> TodoItem
updateItem msg item =
    case msg of
        SetAsTodo name ->
            if name /= item.name then
                item

            else
                { item | isDone = False }

        SetAsDone name ->
            if name /= item.name then
                item

            else
                { item | isDone = True }

        _ ->
            item


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddItem ->
            { model
                | items = model.itemToAdd :: model.items
                , itemToAdd = TodoItem "" False
            }

        OnAddInputChange name ->
            { model
                | itemToAdd = TodoItem name False
            }

        _ ->
            { model
                | items =
                    List.map (updateItem msg) model.items
            }


onDoneClick name =
    onClick (SetAsDone name)


onToDoClick name =
    onClick (SetAsTodo name)


viewItem : TodoItem -> Html Msg
viewItem item =
    if item.isDone == True then
        listItem [ onToDoClick item.name ]
            [ spanDone []
                [ text item.name ]
            ]

    else
        listItem [ onDoneClick item.name ]
            [ spanToDo []
                [ text item.name ]
            ]


viewItemList : List TodoItem -> Html Msg
viewItemList items =
    listDiv []
        (List.map viewItem items)


viewAddItem : TodoItem -> Html Msg
viewAddItem itemToAdd =
    addItemDiv []
        [ styledInput [ type_ "text", onInput OnAddInputChange, value itemToAdd.name ]
            []
        , submitButton
            [ onClick AddItem ]
            [ text "Add item" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewAddItem model.itemToAdd
        , viewItemList model.items
        ]


styledInput : List (Attribute msg) -> List (Html msg) -> Html msg
styledInput =
    styled Html.Styled.input
        [ padding2 (px 12) (px 20)
        , margin2 (px 8) (px 0)
        , border (px 0)
        , borderRadius (px 4)
        ]


submitButton : List (Attribute msg) -> List (Html msg) -> Html msg
submitButton =
    styled Html.Styled.button
        [ Css.width (px 300)
        , backgroundColor (hex "#397cd5")
        , color (hex "#fff")
        , padding2 (px 14) (px 20)
        , border (px 0)
        , borderRadius (px 4)
        , fontSize (px 16)
        , cursor pointer
        ]


spanDone : List (Attribute msg) -> List (Html msg) -> Html msg
spanDone =
    styled Html.Styled.span
        [ textDecoration lineThrough ]


spanToDo : List (Attribute msg) -> List (Html msg) -> Html msg
spanToDo =
    styled Html.Styled.span
        []


addItemDiv : List (Attribute msg) -> List (Html msg) -> Html msg
addItemDiv =
    styled Html.Styled.div
        [ Css.property "display" "grid"
        , Css.property "grid-template-columns" "1fr min-content"
        , Css.property "grid-gap" "10px"
        , borderRadius (px 10)
        , border3 (px 1) solid (hex "#cbcfd2")
        , alignItems center
        , justifyContent spaceBetween
        , padding (px 5)
        , backgroundColor (hex "#d0edff")
        ]


listItem : List (Attribute msg) -> List (Html msg) -> Html msg
listItem =
    styled Html.Styled.div
        [ padding (px 10)
        , backgroundColor (hex "#f0f8ff")
        , cursor pointer
        , displayFlex
        , justifyContent center
        , hover
            [ backgroundColor (hex "#d5eaff")
            ]
        ]


listDiv : List (Attribute msg) -> List (Html msg) -> Html msg
listDiv =
    styled Html.Styled.div
        [ Css.property "display" "grid"
        , Css.property "grid-gap" "10px"
        , padding (px 5)
        ]
