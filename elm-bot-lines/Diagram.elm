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


fontSize = 14.0
lineWidth = 1.0
textWidth = 8.0
textHeight = 16.0
arcRadius = textWidth / 2
color = Color.rgb 0 0 0

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
    | Arc (Point, Point, Float)
    | DashedLine (Point, Point)

-- corresponding paths for each component
componentPaths: Int -> Int -> List (Component, List Path)
componentPaths x y =
    let
        -- block start/quarter/mid/quarter3/end x y
        sx = measureX x
        sy = measureY y
        qx = measureX x + textWidth / 4
        qy = measureY y + textHeight / 3
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
     ,[ Arc  (Point ex my, Point mx q3y, arcRadius)
       ,Line (Point mx q3y, Point mx ey)
      ]
     )
    {--
       .-
       |
    --}
    ,(Junction Mid [Bottom, Left] Smooth
     ,[ Arc  (Point mx q3y, Point sx my, arcRadius)
       ,Line (Point mx q3y, Point mx ey)
      ]
     )
    ,
    {--
      |  
      '- 
    --}
     (Junction Mid [Top, Right] Smooth
     ,[ Arc  (Point mx qy, Point ex my, arcRadius)
       ,Line (Point mx sy, Point mx qy)
      ]
     )
    {--
        |
       -' 
    --}
    ,(Junction Mid [Top, Left] Smooth
     ,[ Arc  (Point sx my, Point mx qy, arcRadius)
       ,Line (Point mx sy, Point mx qy)
      ]
     )
    ]
    



canMerged: Path -> Path -> Bool
canMerged elem1 elem2 =
    case elem1 of
        Line (s, e) ->
            case elem2 of
                Line (s2, e2) ->
                    s == s2 || e == e2
                Arc (s2, e2, r2) ->
                    s == s2 || e == e2
                _ ->
                    False

        Arc (s, e, r) ->
            case elem2 of
                Line (s2, e2) ->
                    s == s2 || e == e2
                Arc (s2, e2, r2) ->
                    s == s2 || e == e2
                _ ->
                    False
        
        DashedLine (s, e) ->
            case elem2 of
                DashedLine (s2, e2) ->
                    s == s2 || e == e2
                _ ->
                    False
        ArrowLine (s, e) ->
            False  --arrow line can't merge

                        


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
                |
            --}
            (isChar char isArrowUp
             &&isNeighbor bottomRight isSlantLeft
            ,Arrow TopLeft
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
    in
    svg [height gheight, width gwidth]
        ([
         gridFill
        ,[defs [] [arrowMarker]]
        ,drawPaths model
        ] |> List.concat
        )
        


drawPaths: Model -> List (Svg a)
drawPaths model =
    Array.indexedMap
    (\y line ->
       Array.indexedMap
        (\ x char->
            componentSvg x y model
        ) line
        |> Array.toList
    ) model.lines
    |> Array.toList
    |> List.concat
    |> List.concat



componentSvg: Int -> Int -> Model -> List (Svg a)
componentSvg x y model =
    let 
        paths = 
            case matchComponent x y model  of 
                Just component ->
                    getComponentPaths x y component
                Nothing ->
                    []

        svgPaths: List (Svg a)
        svgPaths = List.map (
            \ p ->
                svgPath p
        ) paths
     in
        svgPaths

getComponentPaths: Int -> Int -> Component -> List Path
getComponentPaths x y component =
    List.filterMap(
        \ (comp, path) ->
            if component == comp then
                Just path
            else
                Nothing
    ) ( componentPaths x y)
       |> List.head
       |> Maybe.withDefault [] 

type Feature 
    = Arrowed
    | None

svgPath: Path -> Svg a
svgPath elem =
    case elem of
        Line (s, e) ->
            drawLine s e Solid None
        ArrowLine (s, e) ->
            drawLine s e Solid Arrowed
        Arc (s, e, r) ->
            drawArc s e r
        DashedLine (s, e) ->
            drawLine s e Dashed None


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

drawArc start end radius =
    let
        rx = radius
        ry = radius
        sx = start.x
        sy = start.y
        ex = end.x
        ey = end.y
        paths = 
            ["M", toString sx, toString sy
            ,"A", toString rx, toString ry, "0" ,"0", "0"
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
