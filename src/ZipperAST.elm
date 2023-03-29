module ZipperAST exposing (..)

type Expression
    = Variable String
    | Literal Int
    | BinaryOp BinaryOperator Expression Expression

type BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide

type Crumb
    = LeftOf BinaryOperator Expression
    | RightOf BinaryOperator Expression

type Zipper
    = Zipper Expression (List Crumb)

-- Navigation
goLeft : Zipper -> Maybe Zipper
goLeft (Zipper expr crumbs) =
    case (expr, crumbs) of
        (BinaryOp op left right, rest) ->
            Just <| Zipper left (LeftOf op right :: rest)

        _ ->
            Nothing


goRight : Zipper -> Maybe Zipper
goRight (Zipper expr crumbs) =
    case (expr, crumbs) of
        (BinaryOp op left right, rest) ->
            Just <| Zipper right (RightOf op left :: rest)

        _ ->
            Nothing


goUp : Zipper -> Maybe Zipper
goUp (Zipper expr crumbs) =
    case crumbs of
        LeftOf op right :: rest ->
            Just <| Zipper (BinaryOp op expr right) rest

        RightOf op left :: rest ->
            Just <| Zipper (BinaryOp op left expr) rest

        _ ->
            Nothing

goDown : Zipper -> Maybe Zipper
goDown =
    goUp

-- Editing
replace : Expression -> Zipper -> Zipper
replace newExpr (Zipper _ crumbs) =
    Zipper newExpr crumbs