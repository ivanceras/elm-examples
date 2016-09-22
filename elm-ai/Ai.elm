import Html exposing (text,div)

type Words = Sky | Is | Blue | Atmosphere
 
inTruth words =
    let len = List.length words
    in
        if len == 3 then
            List.any (\e -> e == words) truthdb3
        else
            False

truthdb3 =
   [
    [Sky,Is,Blue]
   ,[Sky,Is,Atmosphere]
   ]



synonym = 
    [(Sky, [Atmosphere])
    ,(Atmosphere, [Sky])
    ]

otherWords: Words -> Maybe (List Words)
otherWords x = 
    List.filterMap(
        \(w,s) -> 
            if (w == x) then Just s else Nothing ) synonym 
        |> List.head


truth x y z =
    inTruth [x,y,z]

canBeTrue x y z =
    case otherWords x of
        Just otherWords' ->
            List.any (
                \x' ->
                   inTruth [x', y, z] 
            ) otherWords'
        Nothing ->
            False


main = 
    div []
        [
         showDiv "thruth sky is blue?" ( truth Sky Is Blue)
        ,showDiv "thruth atmoshere is blue?" (truth Atmosphere Is Blue)
        ,showDiv "inTruth [Atmosphere,Is,Blue]?" (inTruth [Atmosphere, Is, Blue])
        ,showDiv "canBeTrue [Atmosphere,Is,Blue]?" (canBeTrue Atmosphere Is Blue)
        ,showDiv "otherWords Sky?" (otherWords Sky)
        ,showDiv "otherWords Atmosphere?" (otherWords Atmosphere)
        ,showDiv "canBeTrue [Atmosphere,Is,Sky]?" (canBeTrue Atmosphere Is Sky)
        ]

showDiv question answer =
    div [] 
    [text question
    ,text (toString answer)
    ]

{--|
a very simple AI concept.

addWords <word> = rewrite the whole program then see if it compiles

addRule <elm function> = add a function to the program, must be a valid elm function

addTruth List <words> = add truths into the program automatically add workds that has not been encountered before
    the order of words in truths is sensitive

addSynonyms <word> List <words>
 
how to add more truths that is longer than 2 arguments

truth a
truth2 a b
truth3 a b c
truth4 a b c d
truth5 a b c d
truth6 a b c d e
    
--}
