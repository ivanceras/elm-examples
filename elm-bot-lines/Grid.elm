module Grid exposing (..)
import String
import Svg exposing (Svg,svg,path,line,marker,defs)
import Svg.Attributes exposing (
    x,y,x1,y1,x2,y2,height,
    width,d,markerHeight,
    markerWidth,orient,markerEnd,
    markerUnits,refX,refY,viewBox,id,
    stroke,strokeWidth,fill
    )
import Char
import Color

type alias Model =
    {rows: Int
    ,columns: Int
    ,lines: List (List Char)
    }

fontSize = 14.0
lineWidth = 1.0
textWidth = 8.0
textHeight = 16.0
arcRadius = textWidth / 2

type CornerPosition 
    = TopRightCorner 
    | TopLeftCorner 
    | BottomRightCorner 
    | BottomLeftCorner
    | TopLeftSlantedDown
    | TopRightSlantedDown

type Element
    = Intersection IntersectionType -- also corner
    | HorizontalLine
    | LowHorizontalLine
    | VerticalLine
    | RoundedCorner CornerPosition
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
    | OpenCurve
    | CloseCurve
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
openCurve = ['(']
closeCurve = [')']
horizontalCurveMarker = ['~']--TODO: any horizontal line has this will make the horizontal line curvy
verticalCurveMarker = ['$','S'] --TODO: any vertical line that has this will make the vertical line curvy

isOpenCurve char = 
    List.member char openCurve

isCloseCurve char =
    List.member char closeCurve

isVerticalLine char =
    List.member char verticalLines

isAlphaNumeric char =
    Char.isDigit char || Char.isUpper char || Char.isLower char

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
leftOf x y model = 
    get (x-1) y model

rightOf x y model =
    get (x+1) y model

isNeighbor neighbor check =
    case neighbor of
        Just neighbor ->
            check neighbor
        Nothing ->
            False

isNeighborDown x y check model =
    let down = get x (y+1) model
    in
        case down of
            Just down ->
                check down
            Nothing ->
                False

isNeighborTop x y check model =
    let top = get x (y-1) model
    in
        case top of
            Just top ->
                check top
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
                if isVerticalLine char 
                    && not (isNeighbor (leftOf x y model) isAlphaNumeric) 
                    && not (isNeighbor (rightOf x y model) isAlphaNumeric) then
                    Just VerticalLine
                else if isHorizontalLine char
                    && not (isNeighbor (leftOf x y model) isAlphaNumeric) 
                    && not (isNeighbor (rightOf x y model) isAlphaNumeric) then
                    Just HorizontalLine
                else if isLowHorizontalLine char
                    && not (isNeighbor (leftOf x y model) isAlphaNumeric) 
                    && not (isNeighbor (rightOf x y model) isAlphaNumeric) then
                    Just LowHorizontalLine
                else if isIntersection char then
                    let
                        isVerticalJunctionLeft = 
                            isNeighborTopVerticalLine x y model 
                            && isNeighborDownVerticalLine x y model
                            && isNeighborLeftHorizontalLine x y model 

                        isVerticalJunctionRight = 
                            isNeighborTopVerticalLine x y model 
                            && isNeighborDownVerticalLine x y model
                            && isNeighborRightHorizontalLine x y model 

                        isHorizontalJunctionTop =
                            isNeighborLeftHorizontalLine x y model 
                            && isNeighborRightHorizontalLine x y model
                            && isNeighborTopVerticalLine x y model 

                        isHorizontalJunctionBot =
                            isNeighborLeftHorizontalLine x y model 
                            && isNeighborRightHorizontalLine x y model
                             && isNeighborDownVerticalLine x y model 

                        isTopLeftIntersection =
                            isNeighborDownVerticalLine x y model && isNeighborRightHorizontalLine x y model

                        isTopRightIntersection =
                            isNeighborDownVerticalLine x y model && isNeighborLeftHorizontalLine x y model

                        isBottomRightIntersection =
                            isNeighborTopVerticalLine x y model && isNeighborLeftHorizontalLine x y model

                        isBottomLeftIntersection =
                            isNeighborTopVerticalLine x y model && isNeighborRightHorizontalLine x y model
                        
                        isCrossIntersection = 
                            isNeighborTopVerticalLine x y model 
                            && isNeighborDownVerticalLine x y model
                            && isNeighborLeftHorizontalLine x y model 
                            && isNeighborRightHorizontalLine x y model 
                    in 
                    if isCrossIntersection then
                        Just (Intersection Cross)
                    else if isVerticalJunctionLeft then
                        Just (Intersection VertJunctionLeft)
                    else if isVerticalJunctionRight then
                        Just (Intersection VertJunctionRight)
                    else if isHorizontalJunctionTop then
                        Just (Intersection HorJunctionTop)
                    else if isHorizontalJunctionBot then
                        Just (Intersection HorJunctionBot)
                    else if isTopRightIntersection then
                        Just (Intersection TopRight)
                    else if isTopLeftIntersection then
                        Just (Intersection TopLeft)
                    else if isBottomRightIntersection then
                        Just (Intersection BottomRight)
                    else if isBottomLeftIntersection then
                        Just (Intersection BottomLeft)
                    else
                        Nothing
                else if isRoundedCorner char then
                    if isNeighborDownVerticalLine x y model 
                        && isNeighborRightHorizontalLine x y model then
                        Just (RoundedCorner TopLeftCorner)
                    else if isNeighborDownVerticalLine x y model
                        && isNeighborLeftHorizontalLine x y model then
                        Just (RoundedCorner TopRightCorner)
                    else if isNeighborTopVerticalLine x y model
                        && isNeighborRightHorizontalLine x y model then
                        Just (RoundedCorner BottomLeftCorner)
                    else if isNeighborTopVerticalLine x y model
                        && isNeighborLeftHorizontalLine x y model then
                        Just (RoundedCorner BottomRightCorner)
                        -- no verticals, rounded corner next to it
                    else if isNeighborRightHorizontalLine x y model
                        && isNeighborDown x y isRoundedCorner model then
                        Just (RoundedCorner TopLeftCorner)
                    else if isNeighborLeftHorizontalLine x y model 
                        && isNeighborDown x y isRoundedCorner model then
                        Just (RoundedCorner TopRightCorner)
                    else if isNeighborLeftHorizontalLine x y model 
                        && isNeighborTop x y isRoundedCorner model then
                        Just (RoundedCorner BottomRightCorner)
                    else if isNeighborRightHorizontalLine x y model 
                        && isNeighborTop x y isRoundedCorner model then
                        Just (RoundedCorner BottomLeftCorner)
                    else if isNeighborRightHorizontalLine x y model
                        && isNeighborBottomLeftSlantedRight x y model then
                        Just (RoundedCorner TopLeftSlantedDown)
                    else if isNeighborLeftHorizontalLine x y model
                        && isNeighborBottomRightSlantedLeft x y model then
                        Just (RoundedCorner TopRightSlantedDown)
                    else
                        Just (Text char)
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
                else if isOpenCurve char 
                    && isNeighborTopRightSlantedRight x y model 
                    && isNeighborBottomRightSlantedLeft x y model then
                    Just OpenCurve
                else if isCloseCurve char
                    && isNeighborTopLeftSlantedLeft x y model
                    && isNeighborBottomLeftSlantedRight x y model then
                    Just CloseCurve
                else if char /= ' ' then
                    Just <| Text char 
                else
                    Nothing
            Nothing ->
                Nothing


drawArc: Float -> Float -> Float -> Float -> Float -> Svg a
drawArc startX startY endX endY radius =
    let
        rx = radius
        ry = radius
        paths = 
            ["M", toString startX, toString startY
            ,"A", toString rx, toString ry, "0" ,"0", "0"
            ,toString endX, toString endY
            ] |> String.join " "
    in
       path [d paths, stroke "black", strokeWidth <| toString lineWidth, fill "transparent"] []

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

                    Intersection itype->
                       (drawIntersection x y itype model)

                    RoundedCorner pos ->
                        (drawRoundedCorner x y pos model)

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

                    OpenCurve ->
                        drawOpenCurve x y model

                    CloseCurve ->
                        drawCloseCurve x y model

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

drawOpenCurve: Int -> Int -> Model -> List (Svg a)
drawOpenCurve x y model =
    let
        startX = measureX x + textWidth
        startY = measureY y
        endX = measureX x + textWidth
        endY = measureY y + textHeight
        radius = textHeight
    in
    [drawArc startX startY endX endY radius
    ]


drawCloseCurve: Int -> Int -> Model -> List (Svg a)
drawCloseCurve x y model =
    let
        startX = measureX x
        startY = measureY y + textHeight
        endX = measureX x
        endY = measureY y
        radius = textHeight
    in
    [drawArc startX startY endX endY radius
    ]

drawRoundedCorner: Int -> Int -> CornerPosition -> Model -> List (Svg a)
drawRoundedCorner x y pos  model =
    case pos of
        TopLeftCorner ->
            drawRoundedTopLeftCorner x y
        TopRightCorner ->
            drawRoundedTopRightCorner x y
        BottomLeftCorner ->
            drawRoundedBottomLeftCorner x y
        BottomRightCorner ->
            drawRoundedBottomRightCorner x y
        TopLeftSlantedDown ->
            drawRoundedTopLeftSlantedDownCorner x y
        TopRightSlantedDown ->
            drawRoundedTopRightSlantedDownCorner x y

drawRoundedTopLeftCorner x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        endX = measureX x + textWidth / 2  --circular arc 
        endY = measureY y + textHeight / 2 + textWidth / 2 --then the rest is line
    in
    [drawArc startX startY endX endY arcRadius
    ,drawLine endX endY endX (measureY y +  textHeight) (Color.rgb 0 0 0)
    ]

drawRoundedTopRightSlantedDownCorner x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        lstartX = measureX x + textWidth
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth * 3 /4
        lendY = measureY y + textHeight * 3 /4 
    in
    [drawArc lendX lendY startX startY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY (Color.rgb 0 0 0)
    ]

drawRoundedTopLeftSlantedDownCorner x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        lstartX = measureX x
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth * 1 /4
        lendY = measureY y + textHeight * 3 /4 
    in
    [drawArc startX startY lendX lendY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY (Color.rgb 0 0 0)
    ]

drawRoundedBottomLeftCorner x y =
    let
        startX = measureX x + textWidth / 2
        startY = measureY y + textHeight/2 - textWidth / 2
        endX = measureX x + textWidth
        endY = measureY y + textHeight / 2
    in
    [drawArc startX startY endX endY arcRadius
    ,drawLine startX startY startX (measureY y) (Color.rgb 0 0 0)
    ]

drawRoundedTopRightCorner x y =
    let
        startX = measureX x + textWidth / 2
        startY = measureY y + textWidth / 2 + textHeight / 2
        endX = measureX x
        endY = measureY y + textHeight / 2
    in
    [drawArc startX startY endX endY arcRadius 
    ,drawLine startX startY startX (measureY y + textHeight) (Color.rgb 0 0 0)
    ]

drawRoundedBottomRightCorner x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        endX = measureX x + textWidth / 2
        endY = measureY y + textHeight / 2 - textWidth / 2
    in
    [drawArc startX startY endX endY arcRadius 
    ,drawLine endX endY endX (measureY y) (Color.rgb 0 0 0)
    ]

--TODO: need the junction: JunctionVertLeft, JunctionVertRight, JunctionHorTop, JunctionHorBot
type IntersectionType = Cross | HorJunctionTop | HorJunctionBot | VertJunctionLeft | VertJunctionRight| TopLeft | TopRight | BottomLeft | BottomRight

drawIntersection: Int -> Int -> IntersectionType ->  Model -> List (Svg a)
drawIntersection x y itype model =
    let
        lw = lineWidth / 2
        --vertical line
        v1startX = measureX x + textWidth / 2
        v1endX = v1startX 
        v1startY = measureY y - lw
        v1endY = measureY y + textHeight / 2 + lw
 
        -- v line part 2
        v2startX = measureX x + textWidth / 2 
        v2endX = v2startX 
        v2startY = measureY y + textHeight / 2 - lw
        v2endY = measureY y + textHeight + lw

        --horizontal line
        h1startX = measureX x - lw
        h1endX = measureX x + textWidth / 2 + lw
        h1startY = measureY y + textHeight / 2 
        h1endY = h1startY

        --h line part 2
        h2startX = measureX x + textWidth / 2 - lw
        h2endX = h2startX + textWidth + lw
        h2startY = measureY y + textHeight / 2
        h2endY = h2startY 

        v1Line = drawLine v1startX v1startY v1endX v1endY (Color.rgb 20 20 200)
        v2Line = drawLine v2startX v2startY v2endX v2endY (Color.rgb 20 200 200)
        h1Line = drawLine h1startX h1startY h1endX h1endY (Color.rgb 200 200 200)
        h2Line = drawLine h2startX h2startY h2endX h2endY (Color.rgb 20 200 200)
        
    in
    case itype of
        VertJunctionLeft ->
            [v1Line, v2Line, h1Line]
        VertJunctionRight ->
            [v1Line, v2Line, h2Line]
        HorJunctionTop ->
            [h1Line, h2Line, v1Line]
        HorJunctionBot ->
            [h1Line, h2Line, v2Line]
        TopLeft ->
            [h2Line, v2Line]
        TopRight ->
            [h1Line, v2Line]
        BottomLeft ->
            [v1Line, h2Line]
        BottomRight ->
            [v1Line, h1Line]
        Cross ->
            [v1Line, v2Line, h1Line, h2Line]
    

drawArrowRight x y model =
    let
        startX = measureX x
        endX = startX + textWidth / 2
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
        endX = measureX x + textWidth / 2
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
        startY = measureY y
        endX = startX - textWidth / 2
        endY = startY + textHeight / 2
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
        startY = measureY y
        endX = startX + textWidth / 2
        endY = startY + textHeight  / 2
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
        startX = measureX x + textWidth / 2
        endX = startX
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

drawArrowNorthWest x y model =
    let
        startX = measureX x
        startY = measureY y + textHeight
        endX = startX + textWidth / 2
        endY = measureY y + textHeight / 2
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
        startY = measureY y + textHeight
        endX = measureX x + textWidth / 2
        endY = measureY y + textHeight / 2
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
    let x'' = measureX x'
        y'' = measureY y' + textHeight / 2
    in
    Svg.text'
        [x <| toString x''
        ,y <| toString y''
        ,Svg.Attributes.style ("font-size:"++toString fontSize++"px;font-family:monospace")
        ]
        [Svg.text <| String.fromChar char]


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
