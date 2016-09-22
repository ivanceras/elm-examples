import Html exposing (text,div)
import Time exposing (Time)

type Word = Sky | Is | Blue | Atmosphere | Ozone
           | I | Have | Magic
           | Skies | Are | Atmospheres
           | Yes | Ok | Good | Great | Exactly | True'
           | No | Not | Nope | Bad | Isn't
  

wordEquivalent =
   [(Sky,"sky")
   ,(Is,"is")
   ,(Blue,"blue")
   ,(Atmosphere, "atmosphere")
   ,(Ozone,"ozone")
   ,(I,"i")
   ,(Have,"have")
   ,(Magic,"magic")
   ,(Skies,"skies")
   ,(Are,"are")
   ,(Atmospheres, "atmospheres")
   ,(Yes,"yes")
   ,(Ok,"ok")
   ,(Good,"good")
   ,(Great, "great")
   ]

toWord: Word -> Maybe String
toWord x =
    List.filterMap (
        \(w, e) ->
            if w == x then
                Just e
            else
                Nothing
    )wordEquivalent
    |> List.head

inTruth words =
    let len = List.length words
    in
    List.any (\e -> e == words) truthdb

truthdb =
   [
    [Sky,Is,Blue]
   ,[Sky,Is,Atmosphere]
   ]


synonym: List (Word, List Word)
synonym = 
    [(Sky, 
        [Atmosphere,Ozone] -- TODO: 2 words that can substitude 1 (Sky -> Ozone Layer)
     )
    ,(Atmosphere, [Sky])
    ]


otherWord: Word -> Maybe (List Word)
otherWord x = 
    List.filterMap(
        \(w,s) -> 
            if (w == x) then 
                Just s 
            else 
                Nothing 
    ) synonym 
        |> List.head


truth x y z =
    inTruth [x,y,z]

canBeTrue x y z =
    case otherWord x of
        Just otherWord' ->
            List.any (
                \x' ->
                   inTruth [x', y, z] 
            ) otherWord'
        Nothing ->
            False

   

plural =
    [(Sky,Skies)
    ,(Is,Are)
    ,(Atmosphere,Atmospheres)
    ]

pluralOf x =
    List.filterMap(
        \ (s,p)->
            if s == x then
                Just p
            else
                Nothing
    )plural
    |> List.head

pluralOfWithGuess: Word -> Maybe String
pluralOfWithGuess x =
    case pluralOf x of
        Just x' ->
            toWord x'
        Nothing ->
            case toWord x of
                Just wordX ->
                    Just (wordX ++ "s")

                Nothing ->
                    Just "anything(s)"

singularOf x =
    List.filterMap(
        \ (s,p)->
        if p == x then
            Just s
        else
            Nothing
    )plural

spoken: List (User, List Word, Time)
spoken =
    [
     (User41532, [Sky,Is,Blue],231231.122)
    ,(User41532, [Atmosphere,Is,Blue], 12838912.77)
    ,(MrRubeck812, [I, Have, Magic], 23142002.89)
    ]

positiveWords =
    [Yes, Good, Ok, True', Great, Exactly]

negativeWords =
    [No, Not, Nope, Isn't, Bad]

type User = User41532 | MrRubeck812

main = 
    div []
        [
         showDiv "thruth sky is blue?" ( truth Sky Is Blue)
        ,showDiv "thruth atmoshere is blue?" (truth Atmosphere Is Blue)
        ,showDiv "inTruth [Atmosphere,Is,Blue]?" (inTruth [Atmosphere, Is, Blue])
        ,showDiv "canBeTrue [Atmosphere,Is,Blue]?" (canBeTrue Atmosphere Is Blue)
        ,showDiv "otherWord Sky?" (otherWord Sky)
        ,showDiv "otherWord Atmosphere?" (otherWord Atmosphere)
        ,showDiv "canBeTrue [Atmosphere,Is,Sky]?" (canBeTrue Atmosphere Is Sky)
        ,showDiv "plural of Sky?" (pluralOf Sky)
        ,showDiv "plural of Ozone?" (pluralOfWithGuess Ozone)
        ]

showDiv question answer =
    div [] 
    [text question
    ,text (toString answer)
    ]

{--|
a very simple AI concept.

addWord <word> = rewrite the whole program then see if it compiles

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
