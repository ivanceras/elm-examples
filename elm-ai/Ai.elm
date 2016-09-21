import Html exposing (text,div)

type Words = Sky | Is | Blue | Atmosphere
is = Is
blue = Blue
sky = Sky
atmosphere = Atmosphere

truth x y z = 
    (x == Sky && y == Is && z == Blue)
    ||(canSubstitute x Sky && y == Is && z == Blue) 
 

synonym = [(Sky, [Atmosphere])]

otherWords x = 
    List.filterMap(
        \(w,s) -> 
            if (w == x) then Just s else Nothing ) synonym 
         |> List.head

canSubstitute x y = 
    case otherWords y of 
        Just list -> 
            List.any (\e -> e == x) list 
        Nothing -> False



main = 
    div []
        [
         text "\ncanSubstitude atmosphere sky?"
        ,text (toString <| canSubstitute atmosphere sky)
        ,text "\nthruth sky is blue?"
        ,text (toString <| truth sky is blue)
        ,text "\nthruth atmoshere is blue?"
        ,text (toString <| truth atmosphere is blue)
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
