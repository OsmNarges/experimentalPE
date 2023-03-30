module ZipperAST exposing (..)

type Expression
    = Variable String
    | Literal Int
    | BoolLiteral Bool
    | BinaryOp BinaryOperator Expression Expression
    | IfThenElse Expression Expression Expression
    | UnaryOp UnaryOperator Expression
    --| Definition String Expression

type BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide
    | GreaterThan
    | And
    | Or

type UnaryOperator
    = Not

type Crumb
    = LeftOf BinaryOperator Expression
    | RightOf BinaryOperator Expression
    | RightOfUnary UnaryOperator
    | CondOf Expression Expression
    | ThenOf Expression Expression
    | ElseOf Expression Expression
    --| DefExpr String

type Zipper
    = Zipper Expression (List Crumb)

-- Navigation
goLeft : Zipper -> Maybe Zipper
goLeft (Zipper expr crumbs) =
    case (expr, crumbs) of
        (BinaryOp op left right, rest) ->
            Just <| Zipper left (LeftOf op right :: rest)
        
        (IfThenElse cond then_ else_, rest) ->
            Just <| Zipper cond (CondOf then_ else_ :: rest)

        _ ->
            Nothing


goRight : Zipper -> Maybe Zipper
goRight (Zipper expr crumbs) =
    case (expr, crumbs) of
        (BinaryOp op left right, rest) ->
            Just <| Zipper right (RightOf op left :: rest)

        (UnaryOp op innerExpr, rest) ->
            Just <| Zipper innerExpr (RightOfUnary op :: rest)

        (IfThenElse cond then_ else_, rest) ->
            Just <| Zipper then_ (ThenOf cond else_ :: rest)

        _ ->
            Nothing


goUp : Zipper -> Maybe Zipper
goUp (Zipper expr crumbs) =
    case crumbs of
        LeftOf op right :: rest ->
            Just <| Zipper (BinaryOp op expr right) rest

        RightOf op left :: rest ->
            Just <| Zipper (BinaryOp op left expr) rest

        CondOf then_ else_ :: rest ->
            Just <| Zipper (IfThenElse expr then_ else_) rest

        ThenOf cond else_ :: rest ->
            Just <| Zipper (IfThenElse cond expr else_) rest

        ElseOf cond then_ :: rest ->
            Just <| Zipper (IfThenElse cond then_ expr) rest

        _ ->
            Nothing

goDown : Zipper -> Maybe Zipper
goDown (Zipper expr crumbs) =
    case expr of
        IfThenElse _ _ _ ->
            goRight <| Zipper expr crumbs

        _ ->
            goUp <| Zipper expr crumbs

-- Editing
replace : Expression -> Zipper -> Zipper
replace newExpr (Zipper _ crumbs) =
    Zipper newExpr crumbs