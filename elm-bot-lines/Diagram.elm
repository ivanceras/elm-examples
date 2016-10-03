module Diagram exposing (..)
import String
import Svg exposing (Svg,svg,path,line,marker,defs)
import Svg.Attributes exposing (
    x,y,x1,y1,x2,y2,height,
    width,d,markerHeight,
    markerWidth,orient,markerEnd,
    markerUnits,refX,refY,viewBox,id,
    stroke,strokeWidth,fill,strokeLinecap,
    strokeLinejoin
    )
import Char
import Color
import Array exposing (Array)

type alias Model =
    {rows: Int
    ,columns: Int
    ,lines: Array (Array Char)
    }

fontSize = 14.0
lineWidth = 1.0
textWidth = 8.0
textHeight = 16.0
arcRadius = textWidth / 2
color = Color.rgb 0 0 0


type Location
    = Top
    | Bottom
    | Left
    | Right
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight



-- plain simple elements
type Element
    = Vertical
    | Horizontal
    | LowHorizontal
    | SlantRight
    | SlantLeft
    | Arc Location Chunk
    | FractionLine Chunk
    | Bullet -- * when only 1 line is connected


type Action
    = Extend
    | Trim

type Outline
    = Smooth  -- . '
    | Sharp   -- + *

type Stroke
    = Dashed  --  : = ... 
    | Solid   --  | - ___

type Component
    = Text Char
    | Element Elevation Element Stroke 
    | Arrow Location
    | Corner Location Outline
    | Action Elevation Element Action Location Chunk
    | Curve Location Elevation Chunk
    | Junction Elevation (List Location) Outline  -- + * . '

type Elevation
    = Low
    | Mid
    | High

type Chunk 
    = Full 
    | Half 
    | Quarter 
    | Quarter3


vertical = ['|']
verticalDashed = [':']
horizontal = ['-']
horizontalDouble = ['=']
lowHorizontal = ['_']
intersections = ['+']
round = ['.','\'']
arrowRight = ['>']
arrowDown = ['V','v']
arrowLeft = ['<']
arrowUp = ['^','î']
slantRight = ['/']
slantLeft = ['\\']
openCurve = ['(']
closeCurve = [')']


get: Int -> Int -> Model -> Maybe Char
get x y model =
    let
        row = y

        line: Maybe (Array Char)
        line = Array.get y model.lines

        char: Maybe Char
        char =
            case line of
                Just l ->
                    Array.get x l
                Nothing ->
                    Nothing
    in 
        char

isOpenCurve char = 
    List.member char openCurve

--close parenthesis
isCloseCurve char =
    List.member char closeCurve

isVertical char =
    List.member char vertical

isVerticalDashed char =
    List.member char verticalDashed

isAlphaNumeric char =
    Char.isDigit char || Char.isUpper char || Char.isLower char

isHorizontal char =
    List.member char horizontal

isLowHorizontal char =
    List.member char lowHorizontal

isIntersection char =
    List.member char intersections

isLine char =
    isVertical char || isHorizontal char || isLowHorizontal char

isRound char =
    List.member char round

isChar: Maybe Char -> (Char -> Bool) -> Bool
isChar char check =
    case char of
        Just char ->
            check char
        Nothing ->
            False

isArrowRight char =
    List.member char arrowRight

isArrowLeft char =
    List.member char arrowLeft

isArrowDown char =
    List.member char arrowDown

isArrowUp char =
    List.member char arrowUp

isSlantRight char =
    List.member char slantRight

isSlantLeft char =
    List.member char slantLeft


leftOf x y model = 
    get (x-1) y model

rightOf x y model =
    get (x+1) y model

topOf x y model =
    get x (y-1) model

bottomOf x y model =
    get x (y+1) model

topLeftOf x y model =
    get (x-1) (y-1) model

topRightOf x y model =
    get (x+1) (y-1) model

bottomLeftOf x y model =
    get (x-1) (y+1) model

bottomRightOf x y model =
    get (x+1) (y+1) model


isNeighbor neighbor check =
    case neighbor of
        Just neighbor ->
            check neighbor
        Nothing ->
            False

-- conditions to match and the corresponding component
componentMatchList: Int -> Int -> Model -> List (Bool, Component)
componentMatchList x y model =
    let
        char = get x y model
        top = topOf x y model
        bottom = bottomOf x y model
        left = leftOf x y model
        right = rightOf x y model
        topLeft = topLeftOf x y model
        topRight = topRightOf x y model
        bottomLeft = bottomLeftOf x y model
        bottomRight = bottomRightOf x y model
    in
        [
            -- char |
            (isChar char isVertical
            ,Element Mid Vertical Solid
            )
            ,
            -- char -
            (isChar char isHorizontal
            ,Element Mid Horizontal Solid
            )
            ,
            -- char :
            (isChar char isVerticalDashed
            ,Element Mid Vertical Dashed
            )
            ,
            {--    
                   /
                  .-
                 /
            --}
            (isChar char isRound
             && isNeighbor topRight isSlantRight
             && isNeighbor bottomLeft isSlantRight
             && isNeighbor right isHorizontal
            ,Junction Mid [TopRight, BottomRight, Right] Smooth
            )

        ]

matchComponent: Int -> Int -> Model -> Maybe Component
matchComponent x y model =
    componentMatchList x y model
        |> List.filterMap
            (\(match, comp) ->
               if match then
                    Just comp
               else
                    Nothing
            )
         |> List.head


