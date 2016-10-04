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

--basic drawing elements that has path maps
type DrawElement

    {--
    draw a horizontal line in the mid of a block
     - 
    --}
     
    = LineMidHorizontal
    | LineMidHorizontalDashed
    | LineLowHorizontal
    | LineLowHorizontalDashed
    | LineMidVertical
    | LineMidVerticalDashed
    | LineSlantLeft
    | LineSlantRight
    | SharpCorner Location
    | RoundCorner Location
    | CrossIntersection

    {--
    
     .-.

     --}
    | ArcMidHalfTop 
    {--

     '-'

    --}
    | ArcMidHalfBottom 

    {--

    -- ._. 

    --}
    | ArcLowHalfBottom 

    {--
         _
        (
         -
    --}
    | ArcMidHalfLeft 
    {--
        -
         )
        -
    --}
    | ArcMidHalfRight 
    {--
       .-
         
    --}
    | ArcMidQuarterTopLeft 
    {--
        -.
          
    --}
    | ArcMidQuarterTopRight
    {--
         
        '-
    --}
    | ArcMidQuarterBottomLeft
    {--
          
        -'
    --}
    | ArcMidQuarterBottomRight

type alias Point = 
    { x : Float
    , y: Float
    }

type Path
    = Line (Point, Point)
    | Arc (Point, Point, Float)
    | DashedLine (Point, Point)


elementPaths: Int -> Int -> List (DrawElement, List Path)
elementPaths x y =
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
     (LineMidHorizontal
     ,[Line (Point sx my, Point ex my)]
     )
     ,
     (LineMidHorizontalDashed
     ,[DashedLine (Point sx my, Point ex my)]
     )
    ,
    (LineLowHorizontal
     ,[Line (Point sx ey, Point ex ey)]
     )
    ,
    (LineLowHorizontalDashed
      ,[DashedLine (Point sx ey, Point ex ey)]
     )
    ,
    (LineMidVertical
     ,[Line (Point mx sy, Point mx ey)]
     )
    ,
    (LineMidVerticalDashed 
     ,[DashedLine (Point mx sy, Point mx ey)]
     )
    ,
    (LineSlantLeft
    ,[Line (Point sx sy, Point ex ey)]
    )
    ,
    (LineSlantRight
    ,[Line (Point sx ey, Point ex sy)]
    )
    ,
    (SharpCorner TopLeft
    ,[Line (Point mx my, Point ex my)
     ,Line (Point mx my, Point mx ey)
     ]
    )
    ,
    (SharpCorner TopRight
    ,[Line (Point sx my, Point mx my)
     ,Line (Point mx my, Point mx ey)
     ]
    )
    ,
    (SharpCorner BottomLeft
    ,[Line (Point mx sy, Point mx my)
     ,Line (Point mx my, Point ex my)
     ]
    )
    ,
    (SharpCorner BottomRight
    ,[Line (Point sx my, Point mx my)
     ,Line (Point mx my, Point mx sy)
     ]
    )
    ,
    (CrossIntersection
    ,[Line (Point sx my, Point ex my)
     ,Line (Point mx sy, Point mx ey)
     ]
    )
    {--
       .-
       |
    --}
    ,(RoundCorner TopLeft
     ,[ Arc  (Point ex my, Point mx q3y, arcRadius)
       ,Line (Point mx q3y, Point mx ey)
      ]
     )
    {--
       .-
       |
    --}
    ,(RoundCorner TopRight
     ,[ Arc  (Point mx q3y, Point sx my, arcRadius)
       ,Line (Point mx q3y, Point mx ey)
      ]
     )
    {--
      |  
      '- 
    --}
    ,(RoundCorner BottomLeft
     ,[ Arc  (Point mx qy, Point ex my, arcRadius)
       ,Line (Point mx sy, Point mx qy)
      ]
     )
    {--
        |
       -' 
    --}
    ,(RoundCorner BottomRight
     ,[ Arc  (Point sx my, Point mx qy, arcRadius)
       ,Line (Point mx sy, Point mx qy)
      ]
     )
    ]

-- get the correspoding drawing elements for each component
componentElements: List (Component, DrawElement)
componentElements =
    [
        (Piece Mid Vertical Solid, LineMidVertical)
       ,(Piece Mid Vertical Dashed, LineMidVerticalDashed)
       ,(Piece Mid Horizontal Solid, LineMidHorizontal)
       ,(Piece Mid Horizontal Dashed, LineMidHorizontalDashed)
       ,(Piece Low Horizontal Solid, LineLowHorizontal)
       ,(Piece Low Horizontal Dashed, LineLowHorizontalDashed)
       ,(Piece Mid SlantLeft Solid, LineSlantLeft)
       ,(Piece Mid SlantRight Solid, LineSlantRight)
        --cross intersection
       ,(Junction Mid [Top, Left, Bottom, Right] Sharp, CrossIntersection)
       --sharp corners
       ,(Junction Mid [Top, Left] Sharp, SharpCorner BottomRight)
       ,(Junction Mid [Top, Right] Sharp, SharpCorner BottomLeft)
       ,(Junction Mid [Bottom, Left] Sharp, SharpCorner TopRight)
       ,(Junction Mid [Bottom, Right] Sharp, SharpCorner TopLeft)
       --smooth corners
       ,(Junction Mid [Top, Left] Smooth, RoundCorner BottomRight)
       ,(Junction Mid [Top, Right] Smooth, RoundCorner BottomLeft)
       ,(Junction Mid [Bottom, Left] Smooth, RoundCorner TopRight)
       ,(Junction Mid [Bottom, Right] Smooth, RoundCorner TopLeft)
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

                        


vertical = ['|']
verticalDashed = [':']
horizontal = ['-']
horizontalDashed = ['=']
lowHorizontal = ['_']
intersection = ['+']
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



getElement: Int -> Int -> Model -> List DrawElement
getElement x y model =
    let
        component = matchComponent x y model
    in
        case component of
            Just component ->
                 List.filterMap (
                    \ (comp, elem) ->
                    if component == comp then
                        Just elem
                    else 
                        Nothing
                 ) componentElements
            Nothing ->
                []
                                

getSvg model =
    let 
        gwidth = toString <| (measureX model.columns)
        gheight = toString <| (measureY model.rows)
    in
    svg [height gheight, width gwidth]
        (gridFill
        ++drawPaths model
        )


drawPaths: Model -> List (Svg a)
drawPaths model =
    Array.indexedMap
    (\y line ->
       Array.indexedMap
        (\ x char->
            elementSvg x y model
        ) line
        |> Array.toList
    ) model.lines
    |> Array.toList
    |> List.concat
    |> List.concat

elementSvg: Int -> Int -> Model -> List (Svg a)
elementSvg x y model =
    let 
        elements: List DrawElement
        elements = getElement x y model 

        paths: List Path
        paths = List.map (
            \ elem ->
                drawElementPaths x y elem
        ) elements
            |> List.concat

        svgPaths: List (Svg a)
        svgPaths = List.map (
            \ p ->
                svgPath p
        ) paths
     in
        svgPaths

drawElementPaths: Int -> Int -> DrawElement -> List Path
drawElementPaths x y elem =
    List.filterMap(
        \ (delem, path) ->
            if elem == delem then
                Just path
            else
                Nothing
    ) (elementPaths x y)
        |> List.concat

svgPath: Path -> Svg a
svgPath elem =
    case elem of
        Line (s, e) ->
            drawLine s e Solid
        Arc (s, e, r) ->
            drawArc s e r
        DashedLine (s, e) ->
            drawLine s e Dashed


drawLine: Point -> Point ->  Stroke -> Svg a
drawLine start end lineStroke =
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
