-- import Comonad exposing (Comonad(..))


-- type Zipper a
--     = Zipper (List a) a (List a)


-- focus : Zipper a -> a
-- focus (Zipper _ x _) =
--     x


-- up : Zipper a -> Maybe (Zipper a)
-- up (Zipper [] _ _) =
--     Nothing
-- up (Zipper (l :: ls) x rs) =
--     Just <| Zipper ls l (x :: rs)


-- down : (a -> List a) -> (a -> List a) -> a -> Zipper a
-- down left right x =
--     Zipper (left x) x (right x)


-- left : Zipper a -> Maybe (Zipper a)
-- left (Zipper [] _ _) =
--     Nothing
-- left (Zipper (l :: ls) x rs) =
--     Just <| Zipper ls l (x :: rs)


-- right : Zipper a -> Maybe (Zipper a)
-- right (Zipper _ _ []) =
--     Nothing
-- right (Zipper ls x (r :: rs)) =
--     Just <| Zipper (x :: ls) r rs


-- modify : (a -> a) -> Zipper a -> Zipper a
-- modify f (Zipper ls x rs) =
--     Zipper ls (f x) rs