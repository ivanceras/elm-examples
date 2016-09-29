module Grid exposing (..)
import String
import Svg exposing (Svg,svg,path,line,marker,defs)
import Svg.Attributes exposing (
    x,y,x1,y1,x2,y2,height,
    width,d,markerHeight,
    markerWidth,orient,markerEnd,
    markerUnits,refX,refY,viewBox,id,
    stroke,strokeWidth
    )

import Color

type alias Model =
    {rows: Int
    ,columns: Int
    ,lines: List (List Char)
    }

type Element
    = Intersection -- also corner
    | HorizontalLine
    | LowHorizontalLine
    | VerticalLine
    | RoundedCorner
    | ArrowRight
    | ArrowDown
    | ArrowSouthWest
    | ArrowSouthEast
    | ArrowUp
    | ArrowNorthWest
    | ArrowNorthEast
    | ArrowLeft
    | SlantRight
    | SlantLeft
    | Text Char

verticalLines = ['|',':']
horizontalLines = ['-']
lowHorizontalLine = ['_']
intersections = ['+']
roundedCorners = ['.','\'']
arrowRight = ['>']
arrowDown = ['V','v']
arrowLeft = ['<']
arrowUp = ['^','î']
slantRight = ['/']
slantLeft = ['\\']

isVerticalLine char =
    List.member char verticalLines

isHorizontalLine char =
    List.member char horizontalLines

isLowHorizontalLine char =
    List.member char lowHorizontalLine

isIntersection char =
    List.member char intersections

isLine char =
    isVerticalLine char || isHorizontalLine char || isLowHorizontalLine char

isRoundedCorner char =
    List.member char roundedCorners

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


--check if top of this character is vertical line
isNeighborTopVerticalLine x y model =
    let top = get x (y - 1) model
    in
    case top of
        Just top ->
            isVerticalLine top
        Nothing ->
            False

isNeighborTopRightSlantedRight x y model =
    let top = get (x + 1) (y - 1) model
    in
    case top of
        Just top ->
            isSlantRight top
        Nothing ->
            False

isNeighborTopLeftSlantedLeft x y model =
    let top = get (x - 1) (y - 1) model
    in
    case top of
        Just top ->
            isSlantLeft top
        Nothing ->
            False

isNeighborDownVerticalLine x y model =
    let down = get x (y+1) model
    in
        case down of
            Just down ->
                isVerticalLine down
            Nothing ->
                False
isNeighborLeftHorizontalLine x y model =
    let left = get (x-1) y model
    in
        case left of
            Just left ->
                isHorizontalLine left
            Nothing ->
                False
isNeighborRightHorizontalLine x y model =
    let right = get (x+1) y model
    in
        case right of
            Just right ->
                isHorizontalLine right
            Nothing ->
                False

isNeighborBottomLeftSlantedRight x y model =
    let bottomLeft = get (x-1) (y+1) model
    in
        case bottomLeft of
            Just bottomLeft ->
                isSlantRight bottomLeft
            Nothing ->
                False

isNeighborBottomRightSlantedLeft x y model =
    let bottomRight = get (x+1) (y+1) model
    in
        case bottomRight of
            Just bottomRight ->
                isSlantLeft bottomRight
            Nothing ->
                False


getElement: Int -> Int -> Model -> Maybe Element
getElement x y model =
    let
        char = get x y model
    in
        case char of
            Just char ->
                if isVerticalLine char then
                    Just VerticalLine
                else if isHorizontalLine char then
                    Just HorizontalLine
                else if isLowHorizontalLine char then
                    Just LowHorizontalLine
                else if isIntersection char then
                    Just Intersection
                else if isRoundedCorner char then
                    Just RoundedCorner
                else if isArrowRight char then
                    Just ArrowRight
                else if isArrowDown char then
                    if isNeighborTopVerticalLine x y model then
                        Just ArrowDown
                    else if isNeighborTopRightSlantedRight x y model then
                        Just ArrowSouthWest
                    else if isNeighborTopLeftSlantedLeft x y model then
                        Just ArrowSouthEast
                    else
                        Just <| Text char

                else if isArrowLeft char then
                    Just ArrowLeft
                else if isArrowUp char then
                    if isNeighborDownVerticalLine x y model then
                        Just ArrowUp
                    else if isNeighborBottomLeftSlantedRight x y model then
                        Just ArrowNorthWest
                    else if isNeighborBottomRightSlantedLeft x y model then
                        Just ArrowNorthEast
                    else
                        Just <| Text char
                else if isSlantRight char then
                    Just SlantRight
                else if isSlantLeft char then
                    Just SlantLeft
                else if char /= ' ' then
                    Just <| Text char 
                else
                    Nothing
            Nothing ->
                Nothing


drawArc: Int -> Int -> Float -> Float -> Float -> Svg a
drawArc x' y' radius startAngle endAngle =
    let
        startRadian = degrees startAngle
        endRadian = degrees endAngle
        (startX, startY) = fromPolar (radius, startRadian)
        (endX, endY) = fromPolar (radius, endRadian)
        largeArcFlag = 
            if endAngle - startAngle <= 180 then
                0
            else
                1

        calcStartX = measureX x' + startX + textWidth / 2
        calcStartY = measureY y' + startY + textHeight / 2
        calcEndX = measureX x' + endX + textWidth / 2
        calcEndY = measureY y' + endY + textHeight / 2

        paths = ["M", toString calcStartX, toString calcStartY
            ,"A", toString radius, toString radius, "0"
            ,toString largeArcFlag, "0"
            ,toString calcEndX, toString calcEndY
            ]
            |> String.join " "
    in
       path [d paths, stroke "black", strokeWidth "2"] []

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

getSvg model =
    let 
        gwidth = toString <| (measureX model.columns) + 10
        gheight = toString <| (measureY model.rows) + 10
    in
    svg [height gheight, width gwidth]
        (defs []
            [arrowMarker]
        ::drawPaths model
        )
        

drawPaths: Model -> List (Svg a)
drawPaths model =
    let 
        rowRange = [0..model.rows]
        columnRange = [0..model.columns]
    in
        List.map
        (\r ->
           List.map
            (\ c ->
               drawElement c r model
            ) columnRange
        ) rowRange
        |> List.concat
        |> List.concat

drawElement: Int -> Int -> Model -> List (Svg a)
drawElement x y model =
    let 
        element =
            getElement x y model 
    in
        case element of
            Just element ->
                case element of
                    HorizontalLine ->
                       [drawHorizontalLine x y model]

                    LowHorizontalLine ->
                        [drawLowHorizontalLine x y model]

                    VerticalLine ->
                       [drawVerticalLine x y model]

                    Intersection ->
                       (drawIntersection x y model)

                    RoundedCorner ->
                        (drawRoundedCorner x y model)

                    ArrowRight ->
                        [drawArrowRight x y model]

                    ArrowDown ->
                        [drawArrowDown x y model]

                    ArrowSouthWest ->
                        [drawArrowSouthWest x y model]

                    ArrowSouthEast ->
                        [drawArrowSouthEast x y model]

                    ArrowUp ->
                        [drawArrowUp x y model]

                    ArrowNorthWest ->
                        [drawArrowNorthWest x y model]

                    ArrowNorthEast ->
                        [drawArrowNorthEast x y model]

                    ArrowLeft ->
                        [drawArrowLeft x y model]

                    SlantRight ->
                        [drawSlantRightLine x y model]

                    SlantLeft ->
                        [drawSlantLeftLine x y model]

                    Text char ->
                        [drawText x y char]

            Nothing ->
                []

drawHorizontalLine: Int -> Int -> Model -> Svg a
drawHorizontalLine x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y + textHeight / 2
        endY = startY
    in
    drawLine startX startY endX endY (Color.rgb 200 20 20)


drawLowHorizontalLine: Int -> Int -> Model -> Svg a
drawLowHorizontalLine x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY (Color.rgb 200 20 20)


drawVerticalLine: Int -> Int -> Model -> Svg a
drawVerticalLine x y model =
    let
        startX = measureX x + textWidth / 2
        endX = startX
        startY = measureY y
        endY = startY + textHeight
    in
    drawLine startX startY endX endY (Color.rgb 20 200 20)


drawSlantRightLine: Int -> Int -> Model -> Svg a
drawSlantRightLine x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y + textHeight
        endY = measureY y
    in
    drawLine startX startY endX endY (Color.rgb 20 200 20)


drawSlantLeftLine: Int -> Int -> Model -> Svg a
drawSlantLeftLine x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y
        endY = measureY y + textHeight
    in
    drawLine startX startY endX endY (Color.rgb 20 200 20)


--TODO: cornerTopLeft, corderTopRight, cornerBottomLeft, cornderBottomRight
drawRoundedCorner: Int -> Int -> Model -> List (Svg a)
drawRoundedCorner x y model =
    let radius = textWidth / 2
    in
        [drawArc x y radius 0 270]

--TODO: need to modularized
-- center
-- midHorizontal
-- midVertical
-- topLeft
-- topRight
-- bottomLeft
-- bottomRight
drawIntersection: Int -> Int -> Model -> List (Svg a)
drawIntersection x y model =
    let
        --vertical line
        v1startX = measureX x + textWidth / 2
        v1endX = v1startX
        v1startY = measureY y
        v1endY = v1startY + textHeight / 2

        -- v line part 2
        v2startX = measureX x + textWidth / 2
        v2endX = v2startX
        v2startY = measureY y + textHeight / 2
        v2endY = v2startY + textHeight / 2

        --horizontal line
        h1startX = measureX x
        h1endX = h1startX + textWidth / 2
        h1startY = measureY y + textHeight / 2
        h1endY = h1startY

        --h line part 2
        h2startX = measureX x + textWidth / 2
        h2endX = h2startX + textWidth
        h2startY = measureY y + textHeight / 2
        h2endY = h2startY
        
    in
        [if isNeighborTopVerticalLine x y model then
             Just <| drawLine v1startX v1startY v1endX v1endY (Color.rgb 20 20 200)
         else
            Nothing

        ,if isNeighborDownVerticalLine x y model then
            Just <| drawLine v2startX v2startY v2endX v2endY (Color.rgb 20 200 200)
         else
            Nothing

        ,if isNeighborLeftHorizontalLine x y model then
            Just <| drawLine h1startX h1startY h1endX h1endY (Color.rgb 200 200 200)
         else 
            Nothing

        ,if isNeighborRightHorizontalLine x y model then
            Just <| drawLine h2startX h2startY h2endX h2endY (Color.rgb 20 200 200)
         else
            Nothing
        ]
            |> List.filterMap
                (\ a -> a)

drawArrowRight x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y + textHeight / 2
        endY = startY
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []


drawArrowLeft x y model =
    let
        startX = measureX x + textWidth
        endX = startX - textWidth
        startY = measureY y + textHeight / 2
        endY = startY
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []



drawArrowDown x y model =
    let
        startX = measureX x + textWidth / 2
        endX = startX
        startY = measureY y
        endY = startY + textHeight 
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []

drawArrowSouthWest x y model =
    let
        startX = measureX x + textWidth
        endX = startX - textWidth
        startY = measureY y
        endY = startY + textHeight 
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []

drawArrowSouthEast x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y
        endY = startY + textHeight 
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []

drawArrowUp x y model =
    let
        startX = measureX x + textWidth /2
        endX = startX
        startY = measureY y + textHeight
        endY = startY - textHeight
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []

drawArrowNorthWest x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y + textHeight
        endY = measureY y
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []

drawArrowNorthEast x y model =
    let
        startX = measureX x + textWidth
        endX = measureX x
        startY = measureY y + textHeight
        endY = measureY y
        color = (Color.rgb 230 40 178)
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ,markerEnd "url(#triangle)"
            ]
            []

drawPath centerX centerY radius degreeAngle =
    42

drawLine startX startY endX endY color =
    let 
        {red,green,blue,alpha} = Color.toRgb color
        colorText = "rgb("++(toString red)++","++(toString green)++","++(toString blue)++")"
    in
        line
            [x1 <| toString startX
            ,x2 <| toString endX
            ,y1 <| toString startY
            ,y2 <| toString endY
            ,Svg.Attributes.style ("stroke: "++colorText++";stroke-width:"++toString lineWidth)
            ]
            []


drawText: Int -> Int -> Char -> Svg a
drawText x' y' char =
    let x'' = measureX x' + textWidth / 2
        y'' = measureY y' + textHeight / 2
    in
    Svg.text'
        [x <| toString x''
        ,y <| toString y''
        ,Svg.Attributes.style ("font-size:"++toString fontSize++"px;font-family:monospace")
        ]
        [Svg.text <| String.fromChar char]

textWidth = 8.0
fontSize = 14.0
lineWidth = 1.0
textHeight = 20.0

measureX: Int -> Float
measureX x =
    toFloat x * textWidth

measureY: Int -> Float
measureY y =
    toFloat y * textHeight

get: Int -> Int -> Model -> Maybe Char
get x y model =
    let
        row = y

        line: Maybe (List Char)
        line = 
            List.indexedMap 
                (\ r l  ->
                   if r == row then
                     Just l
                   else
                    Nothing
                ) model.lines
                |> List.filterMap
                    (\l -> l)
                    |> List.head

        char: Maybe Char
        char =
            case line of
                Just l ->
                    List.indexedMap
                        (\i char ->
                           if i == x then
                             Just char
                           else
                             Nothing 
                        ) l
                        |> List.filterMap
                            (\c -> c)
                                |> List.head
                Nothing ->
                    Nothing
    in 
        char
    

init: String -> Model
init str =
    let
        lines = String.lines str 
        max = 
            List.map
            (\l -> 
                String.length l
            )lines
                |> List.maximum
        lineChar = 
            List.map (
                \l ->
                    String.toList l
            ) lines
    in
    {rows = List.length lineChar
    ,columns = Maybe.withDefault 0 max
    ,lines = lineChar
    }
