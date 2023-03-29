module Main4 exposing (..)
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

-- update : Msg -> Model -> Model
-- update msg model =
--     case msg of
--         ArrowLeft ->
--             { model
--                 | zipper = model.zipper |> Maybe.andThen goLeft
--                 , display = maybeDisplay model.zipper goLeft
--             }

--         ArrowRight ->
--             { model
--                 | zipper = model.zipper |> Maybe.andThen goRight
--                 , display = maybeDisplay model.zipper goRight
--             }

--         ArrowUp ->
--             { model
--                 | zipper = model.zipper |> Maybe.andThen goUp
--                 , display = maybeDisplay model.zipper goUp
--             }

--         ArrowDown ->
--             { model
--                 | zipper = model.zipper |> Maybe.andThen goDown
--                 , display = maybeDisplay model.zipper goDown
--             }

--         NoOp ->
--             model

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

-- maybeDisplay : Maybe Zipper -> (Zipper -> Maybe Zipper) -> String
-- maybeDisplay zipper f =
--     case zipper |> Maybe.andThen f of
--         Just (Zipper expr _) ->
--             expressionToString expr

--         Nothing ->
--             "Invalid operation"

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

-- view : Model -> Html Msg
-- view model =
--     Html.div
--         [ on "keydown" keyDecoder ]
--         [ text model.display ]

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
        render exprP =
            if expr == exprP then
                "<span class=\"cursor\">" ++ expressionToString exprP ++ "</span>"
            else
                expressionToString exprP
    in
    case expr of
        Variable x ->
            render <| Variable x

        Literal i ->
            render <| Literal i

        BinaryOp op left right ->
            "("
                ++ render left
                ++ " "
                ++ binaryOperatorToString op
                ++ " "
                ++ render right
                ++ ")"

