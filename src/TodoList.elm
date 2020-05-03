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
    , error : Maybe AddItemError
    }


initialModel : Model
initialModel =
    { items = [ TodoItem "item1" False, TodoItem "item2" True ]
    , itemToAdd = TodoItem "" False
    , error = Nothing
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


type AddItemError
    = EmptyName
    | AlreadyExists


addNewItem : Model -> Result AddItemError Model
addNewItem model =
    let
        newItemName =
            model.itemToAdd.name

        isNewItemAlreadyExist =
            List.filter (\item -> item.name == newItemName) model.items
                |> List.isEmpty
                |> not

        isNewItemNameEmpty =
            String.isEmpty newItemName
    in
    case ( isNewItemAlreadyExist, isNewItemNameEmpty ) of
        ( False, False ) ->
            Ok
                { model
                    | items = model.itemToAdd :: model.items
                    , itemToAdd = TodoItem "" False
                    , error = Nothing
                }

        ( _, True ) ->
            Err EmptyName

        ( True, _ ) ->
            Err AlreadyExists


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddItem ->
            let
                itemAddingResponse =
                    addNewItem model
            in
            case itemAddingResponse of
                Ok newModel ->
                    newModel

                Err errorType ->
                    { model | error = Just errorType }

        OnAddInputChange name ->
            { model
                | itemToAdd = TodoItem name False
            }

        _ ->
            { model
                | items =
                    List.map (updateItem msg) model.items
            }


onDoneClick : String -> Attribute Msg
onDoneClick name =
    onClick (SetAsDone name)


onToDoClick : String -> Attribute Msg
onToDoClick name =
    onClick (SetAsTodo name)



-- View


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


viewError : Maybe AddItemError -> List (Html Msg)
viewError error =
    case error of
        Nothing ->
            []

        Just AlreadyExists ->
            [ spanError []
                [ text "Item with this name already exists" ]
            ]

        Just EmptyName ->
            [ spanError []
                [ text "Name cannot be empty" ]
            ]


viewAddItem : TodoItem -> Maybe AddItemError -> Html Msg
viewAddItem itemToAdd error =
    let
        errorComponent =
            viewError error
    in
    addItemWrapperDiv []
        (addItemDiv
            []
            [ styledInput
                [ type_ "text", onInput OnAddInputChange, value itemToAdd.name ]
                []
            , submitButton
                [ onClick AddItem ]
                [ text "Add item" ]
            ]
            :: errorComponent
        )


view : Model -> Html Msg
view model =
    div []
        [ title [] [ text "Elm ToDo List" ]
        , viewAddItem model.itemToAdd model.error
        , viewItemList model.items
        ]



-- Styled Components


title : List (Attribute msg) -> List (Html msg) -> Html msg
title =
    styled Html.Styled.h2
        [ textAlign center ]


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
        , alignItems center
        , justifyContent spaceBetween
        ]


addItemWrapperDiv : List (Attribute msg) -> List (Html msg) -> Html msg
addItemWrapperDiv =
    styled Html.Styled.div
        [ borderRadius (px 10)
        , border3 (px 1) solid (hex "#cbcfd2")
        , padding (px 5)
        , backgroundColor (hex "#d0edff")
        , displayFlex
        , flexDirection column
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


spanError : List (Attribute msg) -> List (Html msg) -> Html msg
spanError =
    styled Html.Styled.span
        [ color (hex "#ff4444")
        , fontWeight bold
        ]
