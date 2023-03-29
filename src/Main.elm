module Main exposing (..)
import ZipperAST exposing (..)

import Browser
import Html exposing (Html, text)


import Browser.Events exposing (onKeyDown)

import Json.Decode as Decode exposing (Decoder)
import Html.Events exposing (on)
import Html.Attributes exposing (class, style)


type alias Model =
    { zipper : Maybe Zipper
    , display : String
    }


type Msg
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | NoOp

expressionToString : Expression -> String
expressionToString expr =
    case expr of
        Variable x ->
            x

        Literal i ->
            String.fromInt i

        BinaryOp op left right ->
            "("
                ++ expressionToString left
                ++ " "
                ++ binaryOperatorToString op
                ++ " "
                ++ expressionToString right
                ++ ")"


binaryOperatorToString : BinaryOperator -> String
binaryOperatorToString op =
    case op of
        Add ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"

exprInit : Expression
exprInit =
    BinaryOp Add
        (BinaryOp Multiply
            (Literal 2)
            (Variable "x"))
        (Literal 3)


init : Model
init =
    { zipper = Just <| Zipper exprInit []
    , display = expressionToString exprInit
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        ArrowLeft ->
            { model
                | zipper = model.zipper |> Maybe.andThen goLeft
                , display = maybeDisplay model.zipper expressionWithCursorToString
            }

        ArrowRight ->
            { model
                | zipper = model.zipper |> Maybe.andThen goRight
                , display = maybeDisplay model.zipper expressionWithCursorToString
            }

        ArrowUp ->
            { model
                | zipper = model.zipper |> Maybe.andThen goUp
                , display = maybeDisplay model.zipper expressionWithCursorToString
            }

        ArrowDown ->
            { model
                | zipper = model.zipper |> Maybe.andThen goDown
                , display = maybeDisplay model.zipper expressionWithCursorToString
            }

        NoOp ->
            model

maybeDisplay : Maybe Zipper -> (Zipper -> String) -> String
maybeDisplay zipper f =
    case zipper of
        Just zipperP ->
            f zipperP

        Nothing ->
            "Invalid operation"

keyToMsg : String -> Msg
keyToMsg key =
    case key of
        "ArrowLeft" ->
            ArrowLeft

        "ArrowRight" ->
            ArrowRight

        "ArrowUp" ->
            ArrowUp

        "ArrowDown" ->
            ArrowDown

        _ ->
            NoOp

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "style" [] [ Html.text """
body {
    font-family: Arial, sans-serif;
    font-size: 14px;
}
.editor {
    padding: 20px;
    border: 1px solid #ddd;
    border-radius: 5px;
    background-color: #f5f5f5;
    max-width: 600px;
    margin: 40px auto;
}
.cursor {
    background-color: #FFEB3B;
    padding: 2px 4px;
    border-radius: 3px;
    display: inline;
}
        """ ]
        , Html.div [ class "editor" ]
            [ Html.div []
                [ Html.text <| maybeDisplay model.zipper expressionWithCursorToString ]
            ]
        ]


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string 
        |> Decode.map keyToMsg

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> onKeyDown keyDecoder
        }

expressionWithCursorToString : Zipper -> String
expressionWithCursorToString (Zipper expr focus) =
    let
        renderWithCursor exprP =
            case focus of
                [] ->
                    expressionToString exprP

                LeftOf op right :: crumbs ->
                    let
                        renderedRight =
                            expressionToString right
                    in
                    if focus == LeftOf op right :: crumbs then
                        expressionToString exprP
                            ++ " " ++ binaryOperatorToString op
                            ++ " <span class=\"cursor\">" ++ renderedRight ++ "</span>"
                    else
                        expressionToString exprP
                            ++ " " ++ binaryOperatorToString op
                            ++ " " ++ renderedRight

                RightOf op left :: crumbs ->
                    let
                        renderedLeft =
                            expressionToString left
                    in
                    if focus == RightOf op left :: crumbs then
                        "<span class=\"cursor\">" ++ renderedLeft ++ "</span>"
                            ++ " " ++ binaryOperatorToString op
                            ++ " " ++ expressionToString exprP
                    else
                        renderedLeft ++ " " ++ binaryOperatorToString op
                            ++ " " ++ expressionToString exprP
    in
    case focus of
        [] ->
            expressionToString expr

        LeftOf op right :: crumbs ->
            expressionWithCursorToString (Zipper (BinaryOp op expr right) crumbs)
                |> (\s -> "(" ++ s ++ ")")

        RightOf op left :: crumbs ->
            expressionWithCursorToString (Zipper (BinaryOp op left expr) crumbs)
                |> (\s -> "(" ++ s ++ ")")