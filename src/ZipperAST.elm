module ZipperAST exposing (..)
--import Main2 exposing (CursorDirection(..))

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

-- goRight : Zipper -> Maybe Zipper
-- goRight (Zipper expr crumbs) =
--     case (expr, crumbs) of
--         (BinaryOp op left right, rest) ->
--             Just <| Zipper right (RightOf op left :: rest)

--         (UnaryOp op innerExpr, rest) ->
--             Just <| Zipper innerExpr (RightOfUnary op :: rest)

--         (IfThenElse cond then_ else_, rest) ->
--             Just <| Zipper then_ (ThenOf cond else_ :: rest)

--         _ ->
--             Nothing


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

-- last working before changing the arrow msgs
-- goDown : Zipper -> Maybe Zipper
-- goDown (Zipper expr crumbs) =
--     case expr of
--         IfThenElse _ _ _ ->
--             goRight <| Zipper expr crumbs

--         _ ->
--             goUp <| Zipper expr crumbs

goDown : Zipper -> Maybe Zipper
goDown (Zipper expr crumbs) =
    case expr of
        BinaryOp op left right ->
            Just <| Zipper left (LeftOf op right :: crumbs)

        IfThenElse cond then_ else_ ->
            Just <| Zipper cond (CondOf then_ else_ :: crumbs)

        UnaryOp op innerExpr ->
            Just <| Zipper innerExpr (RightOfUnary op :: crumbs)

        _ ->
            goUp <| Zipper expr crumbs


-- goDown : Zipper -> Maybe Zipper
-- goDown (Zipper expr crumbs) =
--     case expr of
--         IfThenElse _ _ _ ->
--             goRight <| Zipper expr crumbs

--         _ ->
--             goUp <| Zipper expr crumbs

-- New helper function to navigate to the n-th child of an expression
goChild : Zipper -> Int -> Maybe Zipper
goChild (Zipper expr crumbs) n =
    case expr of
        BinaryOp op left right ->
            if n == 0 then
                Just <| Zipper left (LeftOf op right :: crumbs)
            else if n == 1 then
                Just <| Zipper right (RightOf op left :: crumbs)
            else
                Nothing

        IfThenElse cond then_ else_ ->
            if n == 0 then
                Just <| Zipper cond (CondOf then_ else_ :: crumbs)
            else if n == 1 then
                Just <| Zipper then_ (ThenOf cond else_ :: crumbs)
            else if n == 2 then
                Just <| Zipper else_ (ElseOf cond then_ :: crumbs)
            else
                Nothing

        UnaryOp op innerExpr ->
            if n == 0 then
                Just <| Zipper innerExpr (RightOfUnary op :: crumbs)
            else
                Nothing

        _ ->
            Nothing

goNextSibling : Zipper -> Maybe Zipper
goNextSibling (Zipper expr crumbs) =
    case crumbs of
        LeftOf op right :: rest ->
            Just <| Zipper right (RightOf op expr ::rest)

        RightOf op left :: rest ->
            Just <| Zipper left (LeftOf op expr ::rest)

        CondOf then_ else_ :: rest ->
            Just <| Zipper then_ (ThenOf expr else_ ::rest)

        ThenOf cond else_ :: rest ->
            Just <| Zipper else_ (ElseOf cond expr ::rest)

        ElseOf cond then_ :: rest ->
            Just <| Zipper cond (CondOf then_ expr ::rest)

        _ ->
            Nothing


-- Editing
replace : Expression -> Zipper -> Zipper
replace newExpr (Zipper _ crumbs) =
    Zipper newExpr crumbs