module Diagram exposing (..)
import String
import Svg exposing (Svg,svg,path,line,marker,defs,pattern,rect)
import Svg.Attributes exposing (
    x,y,x1,y1,x2,y2,height,
    width,d,markerHeight,
    markerWidth,orient,markerEnd,
    markerUnits,refX,refY,viewBox,id,
    stroke,strokeWidth,fill,strokeLinecap,
    strokeLinejoin,strokeDasharray,
    patternUnits
    
    )
import Char
import Color
import Array exposing (Array)

type alias Model =
    {rows: Int
    ,columns: Int
    ,lines: Array (Array Char)
    }


init: String -> Model
init str =
    let
        lines = String.lines str 
        max = 
            List.map
            (\line -> 
                String.length line
            )lines
                |> List.maximum
        lineArr = Array.fromList lines
        lineChar = 
            Array.map (
                \line ->
                    (String.toList <| String.trimRight line)
                    |> Array.fromList
            ) lineArr
    in
    {rows = Array.length lineChar
    ,columns = Maybe.withDefault 0 max
    ,lines = lineChar
    }

type Density = Compact | Medium | Expanded

fontSize = 14.0
lineWidth = 1.0
textWidth = 8.0
textHeight = 16.0
arcRadius = textWidth / 2
color = Color.rgb 0 0 0
optimizeSvg = False
onePath = False --one path for all or per component path
density = Expanded

measureX: Int -> Float
measureX x =
    toFloat x * textWidth

measureY: Int -> Float
measureY y =
    toFloat y * textHeight


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
type Piece
    {--
        | :
        | :
    --}
    = Vertical
    {--
        --- ===
    --}
    | Horizontal
    {--
        ___ ....
    --}
    | LowHorizontal

    {--
        /
    --}
    | SlantRight
    {--
        \\
     --}
    | SlantLeft


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
    | Piece Elevation Piece Stroke 
    | Arrow Location
    | Corner Location Outline
    | Action Elevation Piece Action Location Chunk
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


type alias Point = 
    { x : Float
    , y: Float
    }

type Path
    = Line (Point, Point)
    | ArrowLine (Point, Point)
    | Arc (Point, Point, Float, Bool)
    | DashedLine (Point, Point)

-- corresponding paths for each component
componentPathList: Int -> Int -> List (Component, List Path)
componentPathList x y =
    let
        -- block start/quarter/mid/quarter3/end x y
        sx = measureX x
        sy = measureY y
        qx = measureX x + textWidth / 4
        qy = measureY y + textHeight / 4
        mx = measureX x + textWidth / 2
        my = measureY y + textHeight / 2
        q3x = measureX x + textWidth * 3 / 4
        q3y = measureY y + textHeight * 3 /4
        ex = measureX x + textWidth
        ey = measureY y + textHeight
    in

    [
     (Piece Mid Horizontal Solid
     ,[Line (Point sx my, Point ex my)]
     )
     ,
     (Piece Mid Horizontal Dashed
     ,[DashedLine (Point sx my, Point ex my)]
     )
    ,
    (Piece Low Horizontal Solid
     ,[Line (Point sx ey, Point ex ey)]
     )
    ,
    (Piece Low Horizontal Dashed
      ,[DashedLine (Point sx ey, Point ex ey)]
     )
    ,
    (Piece Mid Vertical Solid
     ,[Line (Point mx sy, Point mx ey)]
     )
    ,
    (Piece Mid Vertical Dashed 
     ,[DashedLine (Point mx sy, Point mx ey)]
     )
    ,
    (Piece Mid SlantLeft Solid
    ,[Line (Point sx sy, Point ex ey)]
    )
    ,
    (Piece Mid SlantRight Solid
    ,[Line (Point sx ey, Point ex sy)]
    )
    ,
    (Arrow Top
    ,[ArrowLine (Point mx ey, Point mx sy)]
    )
    ,
    (Arrow Bottom
    ,[ArrowLine (Point mx sy, Point mx ey)]
    )
    ,
    (Arrow Left
    ,[ArrowLine (Point ex my, Point mx my)]
    )
    ,
    (Arrow Right
    ,[ArrowLine (Point sx my, Point mx my)]
    )
    ,
    (Arrow TopLeft
    ,[ArrowLine (Point ex ey, Point qx qy)]
    )
    ,(Arrow TopRight
    ,[ArrowLine (Point sx ey, Point q3x qy)]
    )
    ,
    (Arrow BottomLeft
    ,[ArrowLine (Point ex sy, Point qx q3y)]
    )
    ,
    (Arrow BottomRight
    ,[ArrowLine (Point sx sy, Point q3x q3y)]
    )
    ,
    (Junction Mid [Bottom, Right] Sharp
    ,[Line (Point mx my, Point ex my)
     ,Line (Point mx my, Point mx ey)
     ]
    )
    ,
    (Junction Mid [Bottom, Left] Sharp
    ,[Line (Point sx my, Point mx my)
     ,Line (Point mx my, Point mx ey)
     ]
    )
    ,
    (Junction Mid [Top, Right] Sharp
    ,[Line (Point mx sy, Point mx my)
     ,Line (Point mx my, Point ex my)
     ]
    )
    ,
    (Junction Mid [Top, Left] Sharp
    ,[Line (Point sx my, Point mx my)
     ,Line (Point mx my, Point mx sy)
     ]
    )
    ,
    (Junction Mid [Top, Bottom, Right] Sharp
    ,[Line (Point mx sy, Point mx ey)
     ,Line (Point mx my, Point ex my)
     ]
    )
    ,
    (Junction Mid [Top, Bottom, Left] Sharp
    ,[Line (Point mx sy, Point mx ey)
     ,Line (Point sx my, Point mx my)
     ]
    )
    ,
    (Junction Mid [Top, Left, Right] Sharp
    ,[Line (Point mx sy, Point mx my)
     ,Line (Point sx my, Point ex my)
     ]
    )
    ,
    (Junction Mid [Left, Right, Bottom] Sharp
    ,[Line (Point sx my, Point ex my)
     ,Line (Point mx my, Point mx ey)
     ]
    )
    ,
    (Junction Mid [Right, BottomLeft] Smooth
    ,[Arc (Point ex my, Point qx q3y, arcRadius * 2, False)
     ,Line (Point sx ey, Point qx q3y)
     ]
    )
    ,
    (Junction Mid [Left, BottomRight] Smooth
    ,[Arc (Point q3x q3y, Point sx my, arcRadius * 2, False)
     ,Line (Point ex ey, Point q3x q3y)
     ]
    )
    ,
    (Junction Mid [Right, TopLeft] Smooth
    ,[Arc (Point qx qy, Point ex my, arcRadius * 2, False)
     ,Line (Point sx sy, Point qx qy)
     ]
    )
    ,
            {--
                   /
                 -'
            --}
    (Junction Mid [Left, TopRight] Smooth
    ,[Arc (Point sx my, Point q3x qy, arcRadius * 2, False)
     ,Line (Point q3x qy, Point ex sy)
     ]
    )
    ,
    {--
           /
          '-
    --}
    (Junction Mid [Right, TopRight] Smooth
    ,[Arc (Point q3x qy, Point ex my, arcRadius, False)
     ,Line (Point q3x qy, Point ex sy)
     ]
    )
    ,
    {--
           -.
           /
    --}
    (Junction Mid [Left, BottomLeft] Smooth
    ,[Arc (Point qx q3y, Point sx my, arcRadius, False)
     ,Line (Point sx ey, Point qx q3y)
     ]
    )
    ,
    (Junction Mid [Top, Left, Bottom, Right] Sharp
    ,[Line (Point sx my, Point ex my)
     ,Line (Point mx sy, Point mx ey)
     ]
    )
    ,(Junction Mid 
        [Top, Left, Bottom, Right
        ,TopLeft, TopRight, BottomLeft, BottomRight
        ] Sharp
    ,[Line (Point sx my, Point ex my)
     ,Line (Point mx sy, Point mx ey)
     ,Line (Point sx sy, Point ex ey)
     ,Line (Point sx ey, Point ex sy)
     ]
    )
    {--
       .-
       |
    --}
    ,
    (Junction Mid [Bottom, Right] Smooth
     ,[ Arc  (Point ex my, Point mx q3y, arcRadius, False)
       ,Line (Point mx q3y, Point mx ey)
      ]
     )
    {--
       .-
       |
    --}
    ,(Junction Mid [Bottom, Left] Smooth
     ,[ Arc  (Point mx q3y, Point sx my, arcRadius, False)
       ,Line (Point mx q3y, Point mx ey)
      ]
     )
    ,
    {--
      |  
      '- 
    --}
     (Junction Mid [Top, Right] Smooth
     ,[ Arc  (Point mx qy, Point ex my, arcRadius, False)
       ,Line (Point mx sy, Point mx qy)
      ]
     )
    {--
        |
       -' 
    --}
    ,(Junction Mid [Top, Left] Smooth
     ,[ Arc  (Point sx my, Point mx qy, arcRadius, False)
       ,Line (Point mx sy, Point mx qy)
      ]
     )
     ,
    {--
        /
       .-
      /
    --}
    (Junction Mid [Right, TopRight, BottomLeft] Smooth
    ,[ Line (Point sx ey, Point ex sy)
      ,Arc (Point ex my, Point qx q3y, arcRadius * 2, False)
     ]
    )
    ,
    {--
      \  
       .-
        \
    --}
    (Junction Mid [Right, TopLeft, BottomRight] Smooth
    , [Line (Point sx sy, Point ex ey)
      ,Line (Point ex my, Point q3x my)
      ,Arc (Point qx qy, Point q3x my, arcRadius * 2, False)
      ] 
    )
    ,
    {--
       | 
       .
      /|
         
    --}
    (Junction Mid [Top, Bottom, BottomLeft] Smooth
    ,[Line (Point mx sy, Point mx ey)
     ,Line (Point sx ey, Point qx q3y)
     ,Arc (Point qx q3y, Point mx qy, arcRadius * 4, False)
     ]
    )
    
    ]
    

--check if the 3 points lie on the same line
collinear: Point -> Point -> Point -> Bool
collinear p1 p2 p3 =
    let
        ax = p1.x
        ay = p1.y
        bx = p2.x
        by = p2.y
        cx = p3.x
        cy = p3.y
    in
    ax * (by - cy) + bx * (cy - ay) + cx * (ay - by) == 0

-- simply check is path1 end == path2 start
canConcat: Path -> Path -> Bool
canConcat path1 path2 =
    case path1 of
        Line (s, e) ->
            case path2 of
                Line (s2, e2) ->
                    e == s2
                Arc (s2, e2, r2, sweep) ->
                    e == s2
                _ ->
                    False

        Arc (s, e, r, sweep) ->
            case path2 of
                Line (s2, e2) ->
                    e == s2
                Arc (s2, e2, r2, sweep) ->
                    e == s2
                _ ->
                    False
        
        DashedLine (s, e) ->
            case path2 of
                DashedLine (s2, e2) ->
                    e == s2
                _ ->
                    False
        ArrowLine (s, e) ->
            False 


-- only lines can eat another line
-- can eat if path1 ends = path2 start and the lie on the same line
-- returns a new Path with path1 start and path2 end
eat: Path -> Path -> Maybe Path
eat path1 path2 =
    case path1 of
        Line (s, e) ->
            case path2 of
                Line (s2, e2) ->
                    if e == s2 && collinear s e e2 then
                        Just <| Line (s, e2)
                    else
                        Nothing
                _ ->
                    Nothing

        DashedLine (s, e) ->
            case path2 of
                DashedLine (s2, e2) ->
                    if e == s2 && collinear s e e2 then
                        Just <| DashedLine (s, e2)
                    else
                        Nothing
                _ ->
                    Nothing
        _ ->
            Nothing 

canReduce: (Int, Int) -> (Int, Int) -> Model -> Bool
canReduce (x1, y1) (x2, y2) model =
    case tryReduce (x1, y1) (x2, y2) model of
        Just can ->
            True
        Nothing ->
            False

tryReduce: (Int, Int) -> (Int, Int) -> Model -> Maybe Path
tryReduce (x1, y1) (x2, y2) model =
    let
        comp1 = matchComponent x1 y1 model
        comp2 = matchComponent x2 y2 model
    in
    case comp1 of
        Just comp1 ->
            case comp2 of
                Just comp2 ->
                    let
                        path1 = firstPathOnly x1 y1 comp1
                        path2 = firstPathOnly x2 y2 comp2
                    in
                    case path1 of
                        Just path1 ->
                            case path2 of
                                Just path2 ->
                                    reduce path1 path2
                                Nothing ->
                                    Nothing
                        Nothing ->
                            Nothing
                Nothing ->
                    Nothing
        Nothing ->
            Nothing

-- eat with trial on which arrangement
reduce: Path -> Path -> Maybe Path
reduce path1 path2 =
    case eat path1 path2 of
        Just path12 ->
            Just path12
        Nothing ->
            case eat path1 (reversePath path2) of
                Just path1Rev2 ->
                    Just path1Rev2
                Nothing ->
                    Nothing

-- if has only 1 path return it
firstPathOnly: Int -> Int -> Component -> Maybe Path
firstPathOnly x y comp =
    let paths = getComponentPaths x y comp
    in
    if List.length paths == 1 then
        List.head paths
    else
        Nothing


-- if this component is edible, then don't mind plotting it
-- since it something would eat it along the way
-- if it is in between component that could eat it
-- deal only with simple elements
edible: Int -> Int -> Model -> Bool
edible x y model =
    let
        center = (x,y)
        top = (x, y-1)
        bottom = (x, y+1)
        left = (x-1, y)
        topLeft = (x-1, y-1)
        bottomLeft = (x-1, y+1)
        match =
            [canReduce top center model
            ,canReduce left center model
            ,canReduce topLeft center model
            ,canReduce bottomLeft center model
            ]
    in
        List.any (\a -> a) match


vertical = ['|']
verticalDashed = [':']
horizontal = ['-']
horizontalDashed = ['=']
lowHorizontal = ['_']
intersection = ['+']
asterisk = ['*']
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

isHorizontalDashed char =
    List.member char horizontalDashed

isAlphaNumeric char =
    Char.isDigit char || Char.isUpper char || Char.isLower char

isHorizontal char =
    List.member char horizontal

isLowHorizontal char =
    List.member char lowHorizontal

isIntersection char =
    List.member char intersection

isAsterisk char =
    List.member char asterisk

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
-- arrange from simple to most complex
-- low priority to higher priority, the list is reveres before using for matching
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
            {--
                |
            --}
            (isChar char isVertical
            ,Piece Mid Vertical Solid
            )
            ,
            {--
                -
            --}
            (isChar char isHorizontal
            ,Piece Mid Horizontal Solid
            )
            ,
            {--
                _
             --}
            (isChar char isLowHorizontal
            ,Piece Low Horizontal Solid
            )
            ,
            (isChar char isSlantLeft
            ,Piece Mid SlantLeft Solid
            )
            ,
            (isChar char isSlantRight
            ,Piece Mid SlantRight Solid
            )
            ,
            {--
             :
             --}
            (isChar char isVerticalDashed
            ,Piece Mid Vertical Dashed
            )
            ,
            {--
             :
             --}
            (isChar char isHorizontalDashed
            ,Piece Mid Horizontal Dashed
            )
            ,
            {--
             |_
             --}
            (isChar char isLowHorizontal
             && isNeighbor left isVertical
            ,Action Low Horizontal Extend Left Half
            )
            ,
            {--
                ^
                |
            --}
            (isChar char isArrowUp
             &&isNeighbor bottom isVertical
            ,Arrow Top
            )
            ,
            {--
                <-
            --}
            (isChar char isArrowLeft
             &&isNeighbor right isHorizontal
            ,Arrow Left
            )
            ,
            {--
                ->
            --}
            (isChar char isArrowRight
             &&isNeighbor left isHorizontal
            ,Arrow Right
            )
            ,
            {--
                |
                V
            --}
            (isChar char isArrowDown
             &&isNeighbor top isVertical
            ,Arrow Bottom
            )
            ,
            {--
                ^
                 \
            --}
            (isChar char isArrowUp
             &&isNeighbor bottomRight isSlantLeft
            ,Arrow TopLeft
            )
            ,
            {--
                /
               v  
            --}
            (isChar char isArrowDown
             &&isNeighbor topRight isSlantRight
            ,Arrow BottomLeft
            )
            ,
            {--
                \
                 v  
            --}
            (isChar char isArrowDown
             &&isNeighbor topLeft isSlantLeft
            ,Arrow BottomRight
            )
            ,
            {--
                ^
               / 
            --}
            (isChar char isArrowUp
             &&isNeighbor bottomLeft isSlantRight
            ,Arrow TopRight
            )
            ,
            {--
                +-
                |
            --}
            (isChar char isIntersection
             && isNeighbor right isHorizontal
             && isNeighbor bottom isVertical
            ,Junction Mid [Bottom, Right] Sharp
            )
            ,
            {--
               -+
                |
            --}
            (isChar char isIntersection
             && isNeighbor left isHorizontal
             && isNeighbor bottom isVertical
            ,Junction Mid [Bottom, Left] Sharp
            )
            ,
            {--
                |
                +-
            --}
            (isChar char isIntersection
             && isNeighbor right isHorizontal
             && isNeighbor top isVertical
            ,Junction Mid [Top, Right] Sharp
            )
            ,
            {--
                |
               -+
            --}
            (isChar char isIntersection
             && isNeighbor left isHorizontal
             && isNeighbor top isVertical
            ,Junction Mid [Top, Left] Sharp
            )
            ,
            {--
               .- 
              /  
            --}
            (isChar char isRound
             && isNeighbor right isHorizontal
             && isNeighbor bottomLeft isSlantRight
            ,Junction Mid [Right, BottomLeft] Smooth
            )
            ,
            {--
                -.
                  \   
            --}
            (isChar char isRound
             && isNeighbor left isHorizontal
             && isNeighbor bottomRight isSlantLeft
            ,Junction Mid [Left, BottomRight] Smooth
            )
            ,
            {--
               \ 
                '- 
            --}
            (isChar char isRound
             && isNeighbor right isHorizontal
             && isNeighbor topLeft isSlantLeft
            ,Junction Mid [Right, TopLeft] Smooth
            )
            ,
            {--
                   /
                 -'
            --}
            (isChar char isRound
             && isNeighbor left isHorizontal
             && isNeighbor topRight isSlantRight
            ,Junction Mid [Left, TopRight] Smooth
            )
            ,
            {--
                 / 
                '- 
            --}
            (isChar char isRound
             && isNeighbor right isHorizontal
             && isNeighbor topRight isSlantRight
            ,Junction Mid [Right, TopRight] Smooth
            )
            ,
            {--
                 -. 
                 /   
            --}
            (isChar char isRound
             && isNeighbor left isHorizontal
             && isNeighbor bottomLeft isSlantRight
            ,Junction Mid [Left, BottomLeft] Smooth
            )
            ,
            (isChar char isRound
             && isNeighbor right isHorizontal
             && isNeighbor bottom isVertical
            ,Junction Mid [Bottom, Right] Smooth
            )
            ,
            (isChar char isRound
             && isNeighbor left isHorizontal
             && isNeighbor bottom isVertical
            ,Junction Mid [Bottom, Left] Smooth
            )
            {--
               |
               ._
            --}
            ,
            (isChar char isRound
             && isNeighbor right isHorizontal
             && isNeighbor top isVertical
            ,Junction Mid [Top, Right] Smooth
            )
            {--
               |
              -' 
            --}
            ,
            (isChar char isRound
             && isNeighbor left isHorizontal
             && isNeighbor top isVertical
            ,Junction Mid [Top, Left] Smooth
            )
            ,
            {--
                |
                +-
                |
            --}
            (isChar char isIntersection
             && isNeighbor right isHorizontal
             && isNeighbor top isVertical
             && isNeighbor bottom isVertical
            ,Junction Mid [Top, Bottom, Right] Sharp
            )
            ,
            {--
                |
               -+
                |
            --}
            (isChar char isIntersection
             && isNeighbor left isHorizontal
             && isNeighbor top isVertical
             && isNeighbor bottom isVertical
            ,Junction Mid [Top, Bottom, Left] Sharp
            )
            ,
            {--
                |
               -+-
            --}
            (isChar char isIntersection
             && isNeighbor left isHorizontal
             && isNeighbor top isVertical
             && isNeighbor right isHorizontal
            ,Junction Mid [Top, Left, Right] Sharp
            )
            ,
            {--
               -+-
                |
            --}
            (isChar char isIntersection
             && isNeighbor left isHorizontal
             && isNeighbor right isHorizontal
             && isNeighbor bottom isVertical
            ,Junction Mid [Left, Right, Bottom] Sharp
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
            ,Junction Mid [Right, TopRight, BottomLeft] Smooth
            )
            ,
            {--
              \  
               .-
                \
            --}
            (isChar char isRound
             && isNeighbor topLeft isSlantLeft
             && isNeighbor bottomRight isSlantLeft
             && isNeighbor right isHorizontal
            ,Junction Mid [Right, TopLeft, BottomRight] Smooth
            )
            ,
            {--
               | 
               .
              /|
                 
            --}
            (isChar char isRound
             && isNeighbor top isVertical
             && isNeighbor bottom isVertical
             && isNeighbor bottomLeft isSlantRight
            ,Junction Mid [Top, Bottom, BottomLeft] Smooth
            )
            ,
            {--
                |
               -+-
                |
            --}
            (isChar char isIntersection
             && isNeighbor top isVertical
             && isNeighbor left isHorizontal
             && isNeighbor bottom isVertical
             && isNeighbor right isHorizontal
            ,Junction Mid [Top, Left, Bottom, Right] Sharp
            )
            ,
            {--
               \|/  \|/  \|/ 
               -+-  -*-  -.-
               /|\  /|\  /|\ 
            --}
            ((isChar char isIntersection || isChar char isRound || isChar char isAsterisk)
             && isNeighbor top isVertical
             && isNeighbor left isHorizontal
             && isNeighbor bottom isVertical
             && isNeighbor right isHorizontal
             && isNeighbor topLeft isSlantLeft
             && isNeighbor topRight isSlantRight
             && isNeighbor bottomLeft isSlantRight
             && isNeighbor bottomRight isSlantLeft
            ,Junction Mid [Top, Left, Bottom, Right, TopLeft, TopRight, BottomLeft, BottomRight] Sharp
            )

        ]

matchComponent: Int -> Int -> Model -> Maybe Component
matchComponent x y model =
    componentMatchList x y model
        |> List.reverse
        |> List.filterMap
            (\(match, comp) ->
               if match then
                    Just comp
               else
                    Nothing
            )
         |> List.head



                                

getSvg model =
    let 
        gwidth = toString <| (measureX model.columns)
        gheight = toString <| (measureY model.rows)
        svgPaths = 
            case density of
                Compact ->
                    let
                        pathDefs = allPathDefs model
                        onePath = drawPathDef pathDefs Solid None
                     in
                        [onePath]
                Medium ->
                    perComponentPathDefs model
                        |>List.map (
                            \pathDefs ->
                                if String.isEmpty pathDefs then
                                    Nothing 
                                else
                                    Just <| drawPathDef pathDefs Solid None 
                               
                        ) 
                        |> List.filterMap (\a -> a)
                Expanded ->
                    expandedPaths model
                      |> List.map
                      (\path ->
                        svgPath path
                      )
    in
    svg [height gheight, width gwidth]
        ([
         gridFill
        ,[defs [] [arrowMarker]]
        ,svgPaths
        ] |> List.concat
        )
        


allPathDefs: Model -> String
allPathDefs model =
    perComponentPathDefs model
    |> String.join " "

perComponentPathDefs: Model -> List String
perComponentPathDefs model =
    Array.indexedMap
    (\y line ->
       Array.indexedMap
        (\ x char->
            componentPathDefs x y model
        ) line
        |> Array.toList
    ) model.lines
    |> Array.toList
    |> List.concat
    |> List.filterMap (\a->a)

expandedPaths: Model -> List Path
expandedPaths model =
    Array.indexedMap
    (\y line ->
       Array.indexedMap
        (\ x char->
            componentPaths x y model
        ) line
        |> Array.toList
    ) model.lines
    |> Array.toList
    |> List.concat
    |> List.concat

--reduce path to whatever is located in x y
reduceTo: Path -> (Int, Int) -> Model -> Maybe Path
reduceTo path (x, y) model =
    case matchComponent x y model of
        Just comp ->
            case firstPathOnly x y comp of
                Just firstPath ->
                    reduce path firstPath
                Nothing ->
                    Nothing
        Nothing ->
            Nothing

--trace a path starting at this point
--and return the long eating path until a non-edible path is encoutered, and eat it as well
-- reduce only from left to right, top to bottom, topLeft to bottomRight
traceEatEdiblePaths: Path -> (Int, Int) -> Model -> Maybe Path
traceEatEdiblePaths path (x,y) model =
    let
        top = (x, y-1)
        bottom = (x, y+1)
        left = (x-1, y)
        right = (x+1,y)
        topLeft = (x-1, y-1)
        topRight = (x+1, y-1)
        bottomLeft = (x-1, y+1)
        bottomRight = (x+1, y+1)
    in
        case reduceTo path right model of
            Just reducedRight ->
                traceEatEdiblePaths reducedRight right model
            Nothing ->
                case reduceTo path bottom model of
                    Just reducedBottom ->
                        traceEatEdiblePaths reducedBottom bottom model
                    Nothing ->
                        case reduceTo path bottomRight model of
                            Just reducedBottomRight ->
                                traceEatEdiblePaths reducedBottomRight bottomRight model
                            Nothing ->
                                case reduceTo path topRight model of
                                    Just reducedTopRight ->
                                        traceEatEdiblePaths reducedTopRight topRight model
                                    Nothing ->
                                        Just path
                                        
-- check first if there is component before testing if edible
getOptimizedPath x y model =
    let center =
        matchComponent x y model
    in
    case center of
        Just center ->
            if edible x y model then
                []
            else
                case firstPathOnly x y center of
                    Just firstPath ->
                        case traceEatEdiblePaths firstPath (x, y) model of
                            Just path ->
                                [path]
                            Nothing ->
                                []
                    Nothing ->
                         getComponentPaths x y center
        Nothing ->
            []

startEnd: Path -> (Point, Point)
startEnd path =
    case path of
        Line se -> se
        Arc (s, e, r, sw) -> (s, e)
        ArrowLine se -> se
        DashedLine se -> se

-- merge the paths into 1 path definition
toPathDefs: List Path -> String
toPathDefs paths =
    let (prev', pathDefs') =
        List.foldl 
            (\next (prev, str) ->
                let (start, end) = startEnd next
                    continue =
                        case next of
                            Line (s,e) ->
                                ["L", toString e.x, toString e.y]
                            Arc (s,e,r,sw) ->
                                let
                                    sweep = if sw then "1" else "0"
                                in
                                ["A", toString r, toString r, "0" ,"0", sweep, toString e.x, toString e.y]
                            DashedLine (s,e) ->
                                ["L", toString e.x, toString e.y]
                            ArrowLine (s,e) ->
                                ["L", toString e.x, toString e.y]

                    movePen = ["M", toString start.x, toString start.y]

                    pathDefs =
                        case prev of
                            Just prev ->
                                if canConcat prev next then
                                    continue |> String.join " "
                                else
                                    (movePen ++ continue ) |> String.join " "
                            Nothing ->
                                (movePen ++ continue) |> String.join " "
                        
                in
                (Just next
                ,str ++ pathDefs
                )
            ) (Nothing, "")  paths
     in
        pathDefs'

drawPathDef: String -> Stroke -> Feature -> Svg a
drawPathDef pathDefs lineStroke feature =
    let 
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
        dashed =
            case lineStroke of
                Solid ->
                    Svg.Attributes.string ""
                Dashed ->
                    strokeDasharray "3 3"
        arrow =
            case feature of
                Arrowed -> 
                    markerEnd "url(#triangle)"
                None ->
                    Svg.Attributes.string ""
    in
        path [d pathDefs, stroke colorText
             ,strokeWidth <| toString lineWidth
             ,fill "transparent"
             ,dashed
             ,arrow
             ]
             []


componentPathDefs: Int -> Int -> Model -> Maybe String
componentPathDefs x y model =
    let
        pathList = componentPaths x y model
    in
        if List.isEmpty pathList then
            Nothing
        else
           Just <| toPathDefs pathList


componentPaths: Int -> Int -> Model -> List Path
componentPaths x y model =
    case matchComponent x y model  of 
        Just component ->
            let
                paths = 
                    if optimizeSvg then
                        getOptimizedPath x y model
                    else
                        getComponentPaths x y component
             in
                paths
        Nothing ->
           [] 

getComponentPaths: Int -> Int -> Component -> List Path
getComponentPaths x y component =
    List.filterMap(
        \ (comp, path) ->
            if component == comp then
                Just path
            else
                Nothing
    ) ( componentPathList x y)
       |> List.head
       |> Maybe.withDefault [] 

type Feature 
    = Arrowed
    | None

svgPath: Path -> Svg a
svgPath path =
    case path of
        Line (s, e) ->
            drawPathLine s e Solid None
        ArrowLine (s, e) ->
            drawPathLine s e Solid Arrowed
        Arc (s, e, r, sweep) ->
            drawArc s e r sweep
        DashedLine (s, e) ->
            drawPathLine s e Dashed None

reversePath: Path -> Path
reversePath path =
    case path of
        Line (s, e) ->
            Line (e, s)
        ArrowLine (s, e) ->
            ArrowLine (e, s)
        Arc (s, e, r, sweep) ->
            Arc (e, s, r, not sweep)
        DashedLine (s, e) ->
            DashedLine (e, s)
        
    

drawLine: Point -> Point ->  Stroke -> Feature -> Svg a
drawLine start end lineStroke feature =
    let 
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
        sx = start.x
        sy = start.y
        ex = end.x
        ey = end.y
    in
        line
            [x1 <| toString sx
            ,y1 <| toString sy
            ,x2 <| toString ex
            ,y2 <| toString ey
            ,stroke colorText
            ,strokeWidth <| toString lineWidth
            ,strokeLinecap "round"
            ,strokeLinejoin "mitter"
            ,case lineStroke of
                Solid ->
                    strokeDasharray ""
                Dashed ->
                    strokeDasharray "3 3"

            ,case feature of
                Arrowed -> 
                    markerEnd "url(#triangle)"
                None ->
                    markerEnd ""
            ]
            []

drawPathLine start end lineStroke feature =
    let 
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
        sx = start.x
        sy = start.y
        ex = end.x
        ey = end.y
        paths =
            ["M", toString sx, toString sy
            ,"L", toString ex, toString ey
            ] |> String.join " "

        dashed =
            case lineStroke of
                Solid ->
                    strokeDasharray ""
                Dashed ->
                    strokeDasharray "3 3"
        arrow =
            case feature of
                Arrowed -> 
                    markerEnd "url(#triangle)"
                None ->
                    markerEnd ""
    in
        path [d paths, stroke colorText
             ,strokeWidth <| toString lineWidth
             ,fill "transparent"
             ,dashed
             ,arrow
             ]
             []

drawArc start end radius sweep =
    let
        rx = radius
        ry = radius
        sx = start.x
        sy = start.y
        ex = end.x
        ey = end.y
        sweepFlag = 
            if sweep then
                "1"
            else
                "0"

        paths = 
            ["M", toString sx, toString sy
            ,"A", toString rx, toString ry, "0" ,"0", sweepFlag
            ,toString ex, toString ey
            ] |> String.join " "
    in
       path [d paths, stroke "black", strokeWidth <| toString lineWidth, fill "transparent"] []

gridFill =
    [
    defs []
        [  pattern [id "smallGrid", height <|toString <| textHeight / 4, patternUnits "userSpaceOnUse", width <| toString <| textWidth / 4 ]
            [ path [ d "M 8 0 L 0 0 0 8", fill "none", stroke "gray",  strokeWidth "0.1" ]
                []
            ]
        ,  pattern [ id "grid", height <| toString textHeight, patternUnits "userSpaceOnUse", width <| toString textWidth ]
            [  rect [ fill "url(#smallGrid)", height "80", width "80" ]
                []
            , path [ d "M 80 0 L 0 0 0 80", fill "none", stroke "gray", strokeWidth "0.25" ]
                []
            ]
        ]
    ,rect [width "100%", height "100%", fill "url(#grid)"] []
    ]


arrowMarker: Svg a
arrowMarker =
    marker [id "triangle"
           ,viewBox "0 0 14 14"
           ,refX "0"
           ,refY "5"
           ,markerUnits "strokeWidth"
           ,markerWidth "10"
           ,markerHeight "10"
           ,orient "auto"
           ]
        [path [d "M 0 0 L 10 5 L 0 10 z"]
            []
        ]
