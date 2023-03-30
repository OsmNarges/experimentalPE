module Main exposing (..)
import ZipperAST exposing (..)

import Browser
import Html exposing (Html, text)


import Browser.Events exposing (onKeyDown)

import Json.Decode as Decode exposing (Decoder)
import Html.Events exposing (on)
import Html.Attributes exposing (class, style)


-- type alias Model =
--     { zipper : Maybe Zipper
--     , display : String
--     }


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

        BoolLiteral b ->
            boolToString b

        BinaryOp op left right ->
            "("
                ++ expressionToString left
                ++ " "
                ++ binaryOperatorToString op
                ++ " "
                ++ expressionToString right
                ++ ")"
        
        IfThenElse cond then_ else_ ->
            "if "
                ++ expressionToString cond
                ++ " then "
                ++ expressionToString then_
                ++ " else "
                ++ expressionToString else_

        UnaryOp op exprP ->
            unaryOperatorToString op
                ++ " "
                ++ expressionToString exprP



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

        GreaterThan ->
            ">"

        And ->
            "&&"

        Or ->
            "||"

unaryOperatorToString : UnaryOperator -> String
unaryOperatorToString op =
    case op of
        Not ->
            "!"

boolToString : Bool -> String
boolToString b =
    if b then
        "True"
    else
        "False"

-- exprInit : Expression
-- exprInit =
--     BinaryOp Add
--         (BinaryOp Multiply
--             (Literal 2)
--             (Variable "x"))
--         (Literal 3)

-- exprInit : Expression
-- exprInit =
--     BinaryOp Add
--         (IfThenElse
--             (BinaryOp GreaterThan
--                 (Variable "x")
--                 (Literal 10))
--             (BinaryOp Multiply
--                 (Literal 2)
--                 (Variable "x"))
--             (Literal 3))
--         (Literal 5)

exprInit : Expression
exprInit =
    IfThenElse
        (BinaryOp And
            (BinaryOp GreaterThan (Variable "x") (Literal 3))
            (UnaryOp Not (BinaryOp GreaterThan (Variable "y") (Literal 10)))
        )
        (BinaryOp Multiply (Variable "x") (Literal 2))
        (BinaryOp Add (Variable "y") (Literal 5))

-- Change the Model definition
type alias Model =
    { zipper : Maybe Zipper
    , display : Html Msg
    }

-- Update the init function
init : Model
init =
    { zipper = Just <| Zipper exprInit []
    , display = maybeDisplay (Just <| Zipper exprInit []) expressionWithCursorToString
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

-- maybeDisplay : Maybe Zipper -> (Zipper -> String) -> String
-- maybeDisplay zipper f =
--     case zipper of
--         Just zipperP ->
--             f zipperP

--         Nothing ->
--             "Invalid operation"

maybeDisplay : Maybe Zipper -> (Zipper -> Html Msg) -> Html Msg
maybeDisplay zipper f =
    case zipper of
        Just zipperP ->
            f zipperP

        Nothing ->
            Html.text "Invalid operation"

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
            [ maybeDisplay model.zipper expressionWithCursorToString ]
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

expressionWithCursorToString : Zipper -> Html Msg
expressionWithCursorToString (Zipper expr crumbs) =
    let
        renderWithCursor exprP focus =
            case focus of
                [] ->
                    [ Html.text <| expressionToString exprP ]

                LeftOf op right :: rest ->
                    let
                        renderedRight =
                            expressionToString right
                    in
                    if crumbs == LeftOf op right :: rest then
                        [ Html.text (expressionToString exprP ++ " " ++ binaryOperatorToString op ++ " ")
                        , Html.span [ class "cursor" ] [ Html.text renderedRight ]
                        ]
                    else
                        [ Html.text (expressionToString exprP ++ " " ++ binaryOperatorToString op ++ " " ++ renderedRight) ]

                RightOf op left :: rest ->
                    let
                        renderedLeft =
                            expressionToString left
                    in
                    if crumbs == RightOf op left :: rest then
                        [ Html.span [ class "cursor" ] [ Html.text renderedLeft ]
                        , Html.text (" " ++ binaryOperatorToString op ++ " " ++ expressionToString exprP)
                        ]
                    else
                        [ Html.text (renderedLeft ++ " " ++ binaryOperatorToString op ++ " " ++ expressionToString exprP) ]

                CondOf then_ else_ :: rest ->
                    if crumbs == CondOf then_ else_ :: rest then
                        [ Html.span [ class "cursor" ] [ Html.text <| expressionToString exprP ]
                        , Html.text (" then " ++ expressionToString then_ ++ " else " ++ expressionToString else_)
                        ]
                    else
                        [ Html.text (expressionToString exprP ++ " then " ++ expressionToString then_ ++ " else " ++ expressionToString else_) ]

                ThenOf cond else_ :: rest ->
                    if crumbs == ThenOf cond else_ :: rest then
                        [ Html.text ("if " ++ expressionToString cond ++ " ")
                        , Html.span [ class "cursor" ] [ Html.text <| expressionToString exprP ]
                        , Html.text (" else " ++ expressionToString else_)
                        ]
                    else
                        [ Html.text ("if " ++ expressionToString cond ++ " then " ++ expressionToString exprP ++ " else " ++ expressionToString else_) ]

                ElseOf cond then_ :: rest ->
                    if crumbs == ElseOf cond then_ :: rest then
                        [ Html.text ("if " ++ expressionToString cond ++ " then " ++ expressionToString then_ ++ " ")
                        , Html.span [ class "cursor" ] [ Html.text <| expressionToString exprP ]
                        ]
                    else
                        [ Html.text ("if " ++ expressionToString cond ++ " then " ++ expressionToString then_ ++ " else " ++ expressionToString exprP) ]

                RightOfUnary op :: rest ->
                    case exprP of
                        UnaryOp _ innerExpr ->
                            let
                                renderedExpr =
                                    expressionToString innerExpr
                            in
                            if crumbs == [RightOfUnary op] then
                                [ Html.span [ class "cursor" ] [ Html.text <| unaryOperatorToString op ]
                                , Html.text (" " ++ renderedExpr)
                                ]
                            else
                                [ Html.text <| unaryOperatorToString op ++ " " ++ renderedExpr ]
                            
                        _ ->
                            [ Html.text <| expressionToString exprP ]





    in
    case crumbs of
        [] ->
            Html.div [] <| renderWithCursor expr crumbs

        _ ->
            Html.div []
                ( [ Html.text "(" ]
                    ++ renderWithCursor expr crumbs
                    ++ [ Html.text ")" ]
                )