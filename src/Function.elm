module Function exposing (andMap, andThen, applyN, choose, curry, dimap, filterM, fix, flip, forM, ignore, join, lift2, lift3, lift4, lift5, lift6, map, mapAlways, mapM, on, optional, pure, sequence, uncurry, void)


map : (a -> b) -> (e -> a) -> (e -> b)
map =
    (<<)


mapAlways : a -> (e -> b) -> e -> a
mapAlways =
    always >> map


dimap : (a -> b) -> (c -> d) -> (b -> c) -> a -> d
dimap f g h =
    f >> h >> g


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


on : (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g x y =
    f (g x) (g y)


applyN : (a -> a) -> Int -> a -> a
applyN f n a =
    if n <= 0 then
        a

    else
        applyN f (n - 1) (f a)


ignore : a -> b -> b
ignore =
    flip always


join : (a -> a -> b) -> a -> b
join f a =
    f a a


andThen : (e -> a) -> (a -> e -> b) -> e -> b
andThen g f e =
    f (g e) e


andMap : (e -> a) -> (e -> a -> b) -> e -> b
andMap g f e =
    f e (g e)


pure : (a -> b) -> e -> a -> b
pure =
    always


lift2 : (a -> b -> c) -> (e -> a) -> (e -> b) -> e -> c
lift2 f g h e =
    f (g e) (h e)



lift3 : (a -> b -> c -> d) -> (e -> a) -> (e -> b) -> (e -> c) -> e -> d
lift3 f g h i e =
    f (g e) (h e) (i e)


lift4 : (a -> b -> c -> d -> f) -> (e -> a) -> (e -> b) -> (e -> c) -> (e -> d) -> e -> f
lift4 f g h i j e =
    f (g e) (h e) (i e) (j e)


lift5 : (a -> b -> c -> d -> f -> g) -> (e -> a) -> (e -> b) -> (e -> c) -> (e -> d) -> (e -> f) -> e -> g
lift5 f g h i j k e =
    f (g e) (h e) (i e) (j e) (k e)


lift6 : (a -> b -> c -> d -> f -> g -> h) -> (e -> a) -> (e -> b) -> (e -> c) -> (e -> d) -> (e -> f) -> (e -> g) -> e -> h
lift6 f g h i j k l e =
    f (g e) (h e) (i e) (j e) (k e) (l e)


fix : ((a -> b) -> a -> b) -> a -> b
fix f x =
    f (fix f) x


choose : a -> a -> Bool -> a
choose a b c =
    if c then
        a

    else
        b


mapM : (a -> e -> b) -> List a -> e -> List b
mapM f l e =
    List.map (flip f e) l


forM : List a -> (a -> e -> b) -> e -> List b
forM =
    flip mapM


sequence : List (e -> a) -> e -> List a
sequence =
    mapM identity


optional : (a -> b) -> a -> Maybe b
optional f =
    f >> Just


void : (e -> a) -> e -> ()
void _ =
    always ()


filterM : (a -> e -> Bool) -> List a -> e -> List a
filterM f a e =
    List.filter (flip f e) a
