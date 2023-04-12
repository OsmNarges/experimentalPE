module Main exposing (..)

import ZipperAST exposing (..)

import Browser
import Html exposing (Html, text)


import Browser.Events exposing (onKeyDown)

import Json.Decode as Decode exposing (Decoder)
import Html.Events exposing (on)
import Html.Attributes exposing (class, style)

-- Model definition
type alias Model =
    { zipper : Maybe Zipper
    , display : Html Msg
    }

type Msg
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | NoOp

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

-- Update the init function
init : Model
init =
    { zipper = Just <| Zipper exprInit []
    , display = maybeDisplay (Just <| Zipper exprInit []) prettyCursor
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        ArrowLeft ->
            { model
                | zipper = model.zipper |> Maybe.andThen goUp  -- goLeft
                , display = maybeDisplay model.zipper prettyCursor
            }

        ArrowRight ->
            { model
                | zipper = model.zipper |> Maybe.andThen goDown  -- goRight
                , display = maybeDisplay model.zipper prettyCursor
            }

        ArrowUp ->
            { model
                | zipper = model.zipper |> Maybe.andThen goNextSibling  -- goUp
                , display = maybeDisplay model.zipper prettyCursor
            }

        ArrowDown ->
            { model
                | zipper = model.zipper |> Maybe.andThen goLeft  -- goDown
                , display = maybeDisplay model.zipper prettyCursor
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

maybeDisplay : Maybe Zipper -> (Zipper -> Html Msg) -> Html Msg
maybeDisplay zipper f =
    case zipper of
        Just zipperP ->
            f zipperP

        Nothing ->
            Html.text "Invalid operation"

view : Model -> Html Msg
view model =
    let
        dummy = Debug.log "model.zipper " (model.zipper)
    in
    
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
    /* padding: 2px 4px; */
    border-radius: 3px;
    display: inline;
}
.horizontal {
    display: flex;
    align-items: center;
}
        """ ]
        , Html.div [ class "editor" ]
            [ maybeDisplay model.zipper prettyCursor ]
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

prettyWrapCursor : Html Msg -> List Crumb -> Html Msg
prettyWrapCursor innerHtml crumbs =
    case crumbs of
        [] ->
            innerHtml

        LeftOf op right :: rest ->
            let
                rightExprText =
                    " "
                    ++ binaryOperatorToString op
                    ++ " "
                    ++ expressionToString right
                    ++ ")"
            in
                prettyWrapCursor (Html.div [class "horizontal"] 
                                        <| [Html.text <| "("
                                            , innerHtml 
                                            , Html.text <| rightExprText ]) rest
        
        RightOf op left :: rest ->
            let
                leftExprText =
                    "("
                    ++ expressionToString left
                    ++ " "
                    ++ binaryOperatorToString op
                    ++ " "
            in
                prettyWrapCursor (Html.div [class "horizontal"]
                                        <| [ Html.text <| leftExprText
                                            , innerHtml
                                            , Html.text <| ")" ]) rest
        
        CondOf then_ else_ :: rest ->
            let
                thenElseExprText =
                    " then " 
                    ++ expressionToString then_ 
                    ++ " else " 
                    ++ expressionToString else_
            in
                prettyWrapCursor (Html.div [class "horizontal"]
                                        <| [ Html.text "if "
                                            , innerHtml
                                            , Html.text <| thenElseExprText ]) rest
          
        ThenOf cond else_ :: rest ->
            let
                condExprText =
                    "if " 
                    ++ expressionToString cond
                    ++ " then "
                elseExprText =
                    " else " 
                    ++ expressionToString else_  
            in
                prettyWrapCursor (Html.div [class "horizontal"]
                                        <| [ Html.text condExprText
                                            , innerHtml
                                            , Html.text <| elseExprText ]) rest
            
        ElseOf cond then_ :: rest ->
            let
                condThenExprText =
                    "if " 
                    ++ expressionToString cond 
                    ++ " then " 
                    ++ expressionToString then_ 
                    ++ " else "
            in
                prettyWrapCursor (Html.div [class "horizontal"]
                                        <| [ Html.text condThenExprText
                                            , innerHtml]) rest

        RightOfUnary op :: rest ->
            let
                leftExprText =
                    unaryOperatorToString op
                    ++ " "
            in
                prettyWrapCursor (Html.div [class "horizontal"]
                                        <| [ Html.text leftExprText
                                            , innerHtml]) rest


prettyCursor : Zipper -> Html Msg
prettyCursor (Zipper expr crumbs) =
    case expr of
        Variable x ->
            prettyWrapCursor (Html.span [ class "cursor" ] <| [ Html.text <| x ]) crumbs

        Literal i ->
            prettyWrapCursor (Html.span [ class "cursor" ] <| [ Html.text <| (String.fromInt i) ]) crumbs

        BoolLiteral b ->
            prettyWrapCursor (Html.span [ class "cursor" ] <| [ Html.text <| (boolToString b) ]) crumbs

        BinaryOp op left right ->
            let
                exprText =
                    "("
                    ++ expressionToString left
                    ++ " "
                    ++ binaryOperatorToString op
                    ++ " "
                    ++ expressionToString right
                    ++ ")"
            in
                prettyWrapCursor (Html.span [ class "cursor" ] <| [ Html.text <| exprText ]) crumbs
            
        IfThenElse cond then_ else_ ->
            let
                exprText =
                    "if "
                    ++ expressionToString cond
                    ++ " then "
                    ++ expressionToString then_
                    ++ " else "
                    ++ expressionToString else_
            in
                prettyWrapCursor (Html.span [ class "cursor" ] <| [ Html.text <| exprText ]) crumbs
            
        UnaryOp op exprP ->
            let
                exprText =
                    unaryOperatorToString op
                    ++ " "
                    ++ expressionToString exprP
            in
                prettyWrapCursor (Html.span [ class "cursor" ] <| [ Html.text <| exprText ]) crumbs
