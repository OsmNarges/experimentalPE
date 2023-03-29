module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events exposing (onInput)

import Element.Input as Input

import Element exposing (Element, column, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


import Json.Decode as Decode
import Json.Encode as Encode


type Key
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight


type Event
    = KeyDown Key
    | KeyUp Key


decodeKey : Decode.Decoder Key
decodeKey =
    Decode.oneOf
        [ Decode.map (\_ -> ArrowUp) (Decode.field "arrowUp" Decode.bool)
        , Decode.map (\_ -> ArrowDown) (Decode.field "arrowDown" Decode.bool)
        , Decode.map (\_ -> ArrowLeft) (Decode.field "arrowLeft" Decode.bool)
        , Decode.map (\_ -> ArrowRight) (Decode.field "arrowRight" Decode.bool)
        ]


decodeEvent : Decode.Decoder Event
decodeEvent =
    Decode.oneOf
        [ Decode.map KeyDown (Decode.field "keyDown" decodeKey)
        , Decode.map KeyUp (Decode.field "keyUp" decodeKey)
        ]


encodeKey : Key -> Encode.Value
encodeKey key =
    case key of
        ArrowUp ->
            Encode.object [ ( "arrowUp", Encode.bool True ) ]

        ArrowDown ->
            Encode.object [ ( "arrowDown", Encode.bool True ) ]

        ArrowLeft ->
            Encode.object [ ( "arrowLeft", Encode.bool True ) ]

        ArrowRight ->
            Encode.object [ ( "arrowRight", Encode.bool True ) ]


encodeEvent : Event -> Encode.Value
encodeEvent event =
    case event of
        KeyDown key ->
            Encode.object [ ( "keyDown", encodeKey key ) ]

        KeyUp key ->
            Encode.object [ ( "keyUp", encodeKey key ) ]


onKeyDown : (Key -> msg) -> Json.Decode.Value -> msg
onKeyDown tagger json =
    case Decode.decodeValue decodeKey json of
        Ok key ->
            tagger key

        Err _ ->
            -- Ignore invalid key events
            Cmd.none


onKeyUp : (Key -> msg) -> Json.Decode.Value -> msg
onKeyUp tagger json =
    case Decode.decodeValue decodeKey json of
        Ok key ->
            tagger key

        Err _ ->
            -- Ignore invalid key events
            Cmd.none

type Expr
    = Var String
    | IntLit Int
    | BoolLit Bool
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | If Expr Expr Expr





type Zipper a
    = Zipper (List a) a (List a)


focus : Zipper a -> a
focus (Zipper _ x _) =
    x


type Movement
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown

move : Movement -> Zipper a -> Maybe (Zipper a)
move movement zipper =
    case movement of
        MoveLeft ->
            left zipper

        MoveRight ->
            right zipper

        MoveUp ->
            up zipper

        MoveDown ->
            down zipper
                    
up : Zipper a -> Maybe (Zipper a)
up z =
    move Up z

down : Zipper a -> Maybe (Zipper a)
down z =
    move Down z

left : Zipper a -> Maybe (Zipper a)
left z =
    move Left z

right : Zipper a -> Maybe (Zipper a)
right z =
    move Right z



modify : (a -> a) -> Zipper a -> Zipper a
modify f (Zipper ls x rs) =
    Zipper ls (f x) rs





type alias Editor =
    { value : String
    , cursor : Zipper Expr
    }


type Msg
    = SetValue String
    | MoveCursor CursorDirection


type CursorDirection
    = Up
    | Down
    | Left
    | Right


update : Msg -> Editor -> Editor
update msg model =
    case msg of
        SetValue value ->
            { model | value = value }

        MoveCursor direction ->
            { model | cursor = moveCursor direction model.cursor }


view : Editor -> Element Msg
view model =
    let
        cursorNode =
            focus model.cursor |> toString

        updateCursorNode : String -> Editor -> Editor
        updateCursorNode value model =
            let
                newExpr =
                    case focus model.cursor of
                        Var _ ->
                            Var value

                        IntLit _ ->
                            IntLit (String.toInt value |> Result.withDefault 0)

                        BoolLit _ ->
                            BoolLit (String.toBool value |> Result.withDefault False)

                        Add l r ->
                            Add (modify (const <| Var value) <| down Add.left Add.right l) r

                        Sub l r ->
                            Sub (modify (const <| Var value) <| down Sub.left Sub.right l) r

                        Mul l r ->
                            Mul (modify (const <| Var value) <| down Mul.left Mul.right l) r

                        Div l r ->
                            Div (modify (const <| Var value) <| down Div.left Div.right l) r

                        If p t e ->
                            let
                                newP =
                                    modify (const <| Var value) <| down If.predicate If.thenBranch p

                                newT =
                                    modify (const <| Var value) <| down If.thenBranch If.elseBranch t

                                newE =
                                    modify (const <| Var value) <| down If.elseBranch If.predicate e
                            in
                            If newP newT newE
            in
            { model | cursor = modify (const newExpr) model.cursor }

        handleKeyPress : Int -> Editor -> Editor
        handleKeyPress keyCode model =
            let
                direction =
                    case keyCode of
                        38 ->
                            Up

                        40 ->
                            Down

                        37 ->
                            Left

                        39 ->
                            Right

                        _ ->
                            error "Invalid key"
            in
            { model | cursor = moveCursor direction model.cursor }
    in
    column [ spacing 10 ]
        [ Input.textArea [ onInput <| SetValue ] model.value
        , Input.input [ onKeyDown <| \event -> handleKeyPress event.keyCode >> pure ] cursorNode (updateCursorNode >> pure) |> Ui.Element.map (\_ -> model)
        ]


main : Program () Editor Msg
main =
    Browser.sandbox { init = { value = "", cursor =  down (\_ -> []) (\_ -> []) (Var "x") }
                     , view = view
                     , update = update
                     }