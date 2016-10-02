module Grid exposing (..)
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

type Position 
    = TopRightCorner 
    | TopLeftCorner 
    | BottomRightCorner 
    | BottomLeftCorner
    | BottomLeftLowHorizontal
    | BottomRightLowHorizontal
    | BottomLeftSlantedTopLeft
    | BottomLeftSlantedTopRight
    | BottomLeftSlantedBottomRight
    | BottomLeftSlantedTopRightLowHorizontal
    | BottomRightSlantedTopRight
    | BottomRightSlantedTopLeftLowHorizontal
    | BottomRightSlantedTopLeft
    | BottomRightSlantedBottomLeft
    | TopLeftSlantedBottomLeft
    | TopLeftSlantedBottomRight
    | TopRightSlantedBottomRight
    | TopRightSlantedBottomLeft
    | TopRightSlantedTopLeft
    | SlantedRightJunctionRight
    | SlantedLeftJunctionLeft
    | SlantedRightJunctionLeft
    | SlantedLeftJunctionRight
    | VerticalTopDownJunctionBottomLeft
    | VerticalTopDownJunctionBottomRight
    | TopLeftSlantedTopRight
    | VerticalTopDownJunctionTopRight
    | VerticalTopDownJunctionTopLeft
    | TopLeftBigCurve
    | TopRightBigCurve
    | BottomLeftBigCurve
    | BottomRightBigCurve

type Element
    = Intersection Type -- also corner
    | Horizontal
    | LowHorizontal
    | LowHorizontalExtendLeft
    | LowHorizontalExtendVerticalLeft
    | LowHorizontalExtendRight
    | LowHorizontalExtendVerticalRight
    | LowHorizontalExtendVerticalBottomLeft
    | LowHorizontalExtendVerticalBottomRight
    | Vertical
    | RoundCorner Position
    | ArrowEast
    | ArrowSouth
    | ArrowSouthWest
    | ArrowSouthEast
    | ArrowNorth
    | ArrowNorthWest
    | ArrowNorthEast
    | ArrowWest
    | SlantRight
    | SlantLeft
    | OpenCurve
    | CloseCurve
    | BigOpenCurve
    | BigCloseCurve
    | Text Char

{-- intersection types
--}
type Type 
    = Cross 
    | HorJunctionTop
    | HorJunctionBot
    | VertJunctionLeft 
    | VertJunctionRight
    | TopLeft 
    | TopRight 
    | BottomLeft 
    | BottomRight

vertical = ['|']
verticalDashed = [':']
horizontal = ['-']
horizontalDouble = ['=']
lowHorizontal = ['_']
intersections = ['+']
roundCorners = ['.','\'']
arrowRight = ['>']
arrowDown = ['V','v']
arrowLeft = ['<']
arrowUp = ['^','î']
slantRight = ['/']
slantLeft = ['\\']
openCurve = ['(']
closeCurve = [')']

isOpenCurve char = 
    List.member char openCurve

--close parenthesis
isCloseCurve char =
    List.member char closeCurve

isVertical char =
    List.member char vertical

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

isRoundCorner char =
    List.member char roundCorners

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


getElement: Int -> Int -> Model -> Maybe Element
getElement x y model =
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
        case char of
            Just char ->
                if isVertical char
                    && not (isNeighbor left isAlphaNumeric) 
                    && not (isNeighbor right isAlphaNumeric) then
                    Just Vertical
                else if isHorizontal char 
                    && not (isNeighbor left isAlphaNumeric) 
                    && not (isNeighbor right isAlphaNumeric) then
                    Just Horizontal
                else if isLowHorizontal char
                    && isNeighbor left isSlantRight then
                    Just LowHorizontalExtendLeft
                else if isLowHorizontal char
                    && isNeighbor left isVertical then
                    Just LowHorizontalExtendVerticalLeft
                else if isLowHorizontal char
                    && isNeighbor right isSlantLeft then
                    Just LowHorizontalExtendRight
                else if isLowHorizontal char
                    && isNeighbor right isVertical then
                    Just LowHorizontalExtendVerticalRight
                else if isLowHorizontal char
                    && isNeighbor bottomLeft isVertical then
                    Just LowHorizontalExtendVerticalBottomLeft
                else if isLowHorizontal char
                    && isNeighbor bottomRight isVertical then
                    Just LowHorizontalExtendVerticalBottomRight
                else if isLowHorizontal char
                    && not (isNeighbor left isAlphaNumeric) 
                    && not (isNeighbor right isAlphaNumeric) then
                    Just LowHorizontal
                else if isIntersection char then
                    let
                        isVerticalJunctionLeft = 
                            isNeighbor top isVertical
                            && isNeighbor(bottomOf  x y model) isVertical
                            && isNeighbor(leftOf  x y model) isHorizontal 

                        isVerticalJunctionRight = 
                            isNeighbor top isVertical 
                            && isNeighbor bottom isVertical
                            && isNeighbor right isHorizontal 

                        isHorizontalJunctionTop =
                            isNeighbor left isHorizontal 
                            && isNeighbor right isHorizontal
                            && isNeighbor top isVertical 

                        isHorizontalJunctionBot =
                            isNeighbor left isHorizontal 
                            && isNeighbor right isHorizontal
                             && isNeighbor bottom isVertical 

                        isTopLeftIntersection =
                            isNeighbor bottom isVertical && isNeighbor right isHorizontal

                        isTopRightIntersection =
                            isNeighbor bottom isVertical && isNeighbor left isHorizontal

                        isBottomRightIntersection =
                            isNeighbor top isVertical && isNeighbor left isHorizontal

                        isBottomLeftIntersection =
                            isNeighbor top isVertical && isNeighbor right isHorizontal
                        
                        isCrossIntersection = 
                            isNeighbor top isVertical 
                            && isNeighbor bottom isVertical
                            && isNeighbor left isHorizontal 
                            && isNeighbor right isHorizontal 

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
                else if isRoundCorner char then
                    if isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor right isHorizontal then
                        Just (RoundCorner SlantedRightJunctionRight)
                    else if isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft then
                        Just (RoundCorner VerticalTopDownJunctionTopLeft)
                    else if isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor left isHorizontal then
                        Just (RoundCorner SlantedLeftJunctionLeft)
                    else if isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor left isHorizontal then
                        Just (RoundCorner SlantedRightJunctionLeft)
                    else if isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor right isHorizontal then
                        Just (RoundCorner SlantedLeftJunctionRight)
                    else if isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomLeft isSlantRight then
                        Just (RoundCorner VerticalTopDownJunctionBottomLeft)
                    else if isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomRight isSlantLeft then
                        Just (RoundCorner VerticalTopDownJunctionBottomRight)
                    else if isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight then
                        Just (RoundCorner VerticalTopDownJunctionTopRight)
                    else if isNeighbor bottom isVertical 
                        && isNeighbor right isHorizontal then
                        Just (RoundCorner TopLeftCorner)
                    else if isNeighbor bottom isVertical
                        && isNeighbor left isHorizontal then
                        Just (RoundCorner TopRightCorner)
                    else if isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight then
                        Just (RoundCorner TopLeftSlantedTopRight)
                    else if isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isOpenCurve then
                        Just (RoundCorner TopLeftBigCurve)
                    else if isNeighbor right isRoundCorner
                        && isNeighbor bottomLeft isOpenCurve then
                        Just (RoundCorner TopLeftBigCurve)
                    else if isNeighbor left isHorizontal
                        && isNeighbor bottomRight isCloseCurve then
                        Just (RoundCorner TopRightBigCurve)
                    else if isNeighbor left isRoundCorner
                        && isNeighbor bottomRight isCloseCurve then
                        Just (RoundCorner TopRightBigCurve)
                    else if isNeighbor right isHorizontal
                        && isNeighbor topLeft isOpenCurve then
                        Just (RoundCorner BottomLeftBigCurve)
                    else if isNeighbor left isHorizontal
                        && isNeighbor topRight isCloseCurve then
                        Just (RoundCorner BottomRightBigCurve)
                    else if isNeighbor right isRoundCorner
                        && isNeighbor topLeft isOpenCurve then
                        Just (RoundCorner BottomLeftBigCurve)
                    else if isNeighbor left isRoundCorner
                        && isNeighbor topRight isCloseCurve then
                        Just (RoundCorner BottomRightBigCurve)
                    else if isNeighbor top isVertical
                        && isNeighbor right isHorizontal then
                        Just (RoundCorner BottomLeftCorner)
                    else if isNeighbor top isVertical 
                        && isNeighbor right isLowHorizontal then
                        Just (RoundCorner BottomLeftLowHorizontal) 
                    else if isNeighbor top isVertical 
                        && isNeighbor left isLowHorizontal then
                        Just (RoundCorner BottomRightLowHorizontal) 
                    else if isNeighbor right isHorizontal 
                            && isNeighbor topLeft isSlantLeft then
                        Just (RoundCorner BottomLeftSlantedTopLeft)
                    else if isNeighbor right isHorizontal 
                            && isNeighbor topRight isSlantRight then
                        Just (RoundCorner BottomLeftSlantedTopRight)
                    else if isNeighbor top isVertical 
                            && isNeighbor bottomRight isSlantLeft then
                        Just (RoundCorner BottomLeftSlantedBottomRight)
                    else if isNeighbor left isHorizontal 
                            && isNeighbor topRight isSlantRight then
                        Just (RoundCorner BottomRightSlantedTopRight)
                    else if isNeighbor right isLowHorizontal 
                            && isNeighbor topRight isSlantRight then
                        Just (RoundCorner BottomLeftSlantedTopRightLowHorizontal)
                    else if isNeighbor left isLowHorizontal 
                            && isNeighbor topLeft isSlantLeft then
                        Just (RoundCorner BottomRightSlantedTopLeftLowHorizontal)
                    else if isNeighbor left isHorizontal
                            && isNeighbor topLeft isSlantLeft then
                        Just (RoundCorner BottomRightSlantedTopLeft)
                    else if isNeighbor top isVertical
                            && isNeighbor bottomLeft isSlantRight then
                        Just (RoundCorner BottomRightSlantedBottomLeft)
                    else if isNeighbor top isVertical
                        && isNeighbor left isHorizontal then
                        Just (RoundCorner BottomRightCorner)
                    else if isNeighbor right isHorizontal
                        && isNeighbor bottom isRoundCorner then
                        Just (RoundCorner TopLeftCorner)
                    else if isNeighbor left isHorizontal 
                        && isNeighbor bottom isRoundCorner then
                        Just (RoundCorner TopRightCorner)
                    else if isNeighbor left isHorizontal 
                        && isNeighbor top isRoundCorner then
                        Just (RoundCorner BottomRightCorner)
                    else if isNeighbor right isHorizontal
                        && isNeighbor top isRoundCorner then
                        Just (RoundCorner BottomLeftCorner)
                    else if isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isSlantRight then
                        Just (RoundCorner TopLeftSlantedBottomLeft)
                    else if isNeighbor right isHorizontal
                        && isNeighbor bottomRight isSlantLeft then
                        Just (RoundCorner TopLeftSlantedBottomRight)
                    else if isNeighbor left isHorizontal
                        && isNeighbor bottomRight isSlantLeft then
                        Just (RoundCorner TopRightSlantedBottomRight)
                    else if isNeighbor left isHorizontal
                        && isNeighbor bottomLeft isSlantRight then
                        Just (RoundCorner TopRightSlantedBottomLeft)
                    else if isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft then
                        Just (RoundCorner TopRightSlantedTopLeft)
                    else
                        Just (Text char)
                else if isArrowRight char then
                    Just ArrowEast
                else if isArrowDown char then
                    if isNeighbor top isVertical then
                        Just ArrowSouth
                    else if isNeighbor topRight isSlantRight then
                        Just ArrowSouthWest
                    else if isNeighbor topLeft isSlantLeft then
                        Just ArrowSouthEast
                    else
                        Just <| Text char

                else if isArrowLeft char then
                    Just ArrowWest
                else if isArrowUp char then
                    if isNeighbor bottom isVertical then
                        Just ArrowNorth
                    else if isNeighbor bottomLeft isSlantRight then
                        Just ArrowNorthWest
                    else if isNeighbor bottomRight isSlantLeft then
                        Just ArrowNorthEast
                    else
                        Just <| Text char
                else if isSlantRight char then
                    Just SlantRight
                else if isSlantLeft char then
                    Just SlantLeft
                else if isOpenCurve char then
                        if isNeighbor topRight isSlantRight 
                            && isNeighbor bottomRight isSlantLeft then
                            Just OpenCurve
                        else if isNeighbor topRight isRoundCorner 
                            && isNeighbor bottomRight isRoundCorner then
                            Just BigOpenCurve
                        else
                            Just <| Text char
                else if isCloseCurve char
                    && isNeighbor topLeft isRoundCorner 
                    && isNeighbor bottomLeft isRoundCorner then
                    Just BigCloseCurve
                else if isCloseCurve char
                    && isNeighbor topLeft isSlantLeft
                    && isNeighbor bottomLeft isSlantRight then
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
        
--TODO: optimize here to indexedMap only the lines and chars
--TODO: modularized parts in order to easily fit and match
drawPaths: Model -> List (Svg a)
drawPaths model =
    Array.indexedMap
    (\r line ->
       Array.indexedMap
        (\ c char->
           drawElement c r model
        ) line
        |> Array.toList
    ) model.lines
    |> Array.toList
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
                    Horizontal ->
                       [drawHorizontalLine x y model]

                    LowHorizontal ->
                        [drawLowHorizontalLine x y model]

                    LowHorizontalExtendLeft ->
                        [drawLowHorizontalExtendLeft x y model]

                    LowHorizontalExtendVerticalLeft ->
                        [drawLowHorizontalExtendVerticalLeft x y model]

                    LowHorizontalExtendRight ->
                        [drawLowHorizontalExtendRight x y model]

                    LowHorizontalExtendVerticalRight ->
                        [drawLowHorizontalExtendVerticalRight x y model]

                    LowHorizontalExtendVerticalBottomLeft ->
                        [drawLowHorizontalExtendVerticalBottomLeft x y model]

                    LowHorizontalExtendVerticalBottomRight ->
                        [drawLowHorizontalExtendVerticalBottomRight x y model]

                    Vertical ->
                       [drawVerticalLine x y model]

                    Intersection itype->
                       (drawIntersection x y itype model)

                    RoundCorner pos ->
                        (drawRoundCorner x y pos model)

                    ArrowEast ->
                        [drawArrowRight x y model]

                    ArrowSouth ->
                        [drawArrowDown x y model]

                    ArrowSouthWest ->
                        [drawArrowSouthWest x y model]

                    ArrowSouthEast ->
                        [drawArrowSouthEast x y model]

                    ArrowNorth ->
                        [drawArrowUp x y model]

                    ArrowNorthWest ->
                        [drawArrowNorthWest x y model]

                    ArrowNorthEast ->
                        [drawArrowNorthEast x y model]

                    ArrowWest ->
                        [drawArrowLeft x y model]

                    SlantRight ->
                        [drawSlantRightLine x y model]

                    SlantLeft ->
                        [drawSlantLeftLine x y model]

                    OpenCurve ->
                        drawOpenCurve x y model

                    CloseCurve ->
                        drawCloseCurve x y model

                    BigOpenCurve ->
                        drawBigOpenCurve x y model

                    BigCloseCurve ->
                        drawBigCloseCurve x y model

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
    drawLine startX startY endX endY color


drawLowHorizontalLine: Int -> Int -> Model -> Svg a
drawLowHorizontalLine x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY color

drawLowHorizontalExtendLeft: Int -> Int -> Model -> Svg a
drawLowHorizontalExtendLeft x y model =
    let
        startX = measureX x - textWidth
        endX = measureX x + textWidth
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY color

drawLowHorizontalExtendVerticalLeft: Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalLeft x y model =
    let
        startX = measureX x - textWidth / 2
        endX = measureX x + textWidth
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY color

drawLowHorizontalExtendVerticalBottomLeft: Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalBottomLeft x y model =
    let
        startX = measureX x - textWidth / 2
        endX = measureX x + textWidth
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY color

drawLowHorizontalExtendVerticalBottomRight: Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalBottomRight x y model =
    let
        startX = measureX x
        endX = measureX x + textWidth + textWidth / 2
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY color

drawLowHorizontalExtendRight: Int -> Int -> Model -> Svg a
drawLowHorizontalExtendRight x y model =
    let
        startX = measureX x
        endX = measureX x + textWidth * 2
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY color

drawLowHorizontalExtendVerticalRight: Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalRight x y model =
    let
        startX = measureX x
        endX = measureX x + textWidth + textWidth / 2
        startY = measureY y + textHeight
        endY = startY
    in
    drawLine startX startY endX endY color

drawVerticalLine: Int -> Int -> Model -> Svg a
drawVerticalLine x y model =
    let
        startX = measureX x + textWidth / 2
        endX = startX
        startY = measureY y
        endY = startY + textHeight
    in
    drawLine startX startY endX endY color


drawSlantRightLine: Int -> Int -> Model -> Svg a
drawSlantRightLine x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y + textHeight
        endY = measureY y
    in
    drawLine startX startY endX endY color


drawSlantLeftLine: Int -> Int -> Model -> Svg a
drawSlantLeftLine x y model =
    let
        startX = measureX x
        endX = startX + textWidth
        startY = measureY y
        endY = measureY y + textHeight
    in
    drawLine startX startY endX endY color

drawOpenCurve: Int -> Int -> Model -> List (Svg a)
drawOpenCurve x y model =
    let
        startX = measureX x + textWidth
        startY = measureY y
        endX = measureX x + textWidth
        endY = measureY y + textHeight
    in
    [drawArc startX startY endX endY (arcRadius * 4)
    ]

drawBigOpenCurve: Int -> Int -> Model -> List (Svg a)
drawBigOpenCurve x y model =
    let
        startX = measureX x + textWidth / 2
        startY = measureY y
        endX = measureX x + textWidth / 2
        endY = measureY y + textHeight
    in
    [drawArc startX startY endX endY (arcRadius * 4)
    ]

drawBigCloseCurve: Int -> Int -> Model -> List (Svg a)
drawBigCloseCurve x y model =
    let
        startX = measureX x + textWidth / 2
        startY = measureY y + textHeight
        endX = measureX x + textWidth / 2
        endY = measureY y
    in
    [drawArc startX startY endX endY (arcRadius * 4)
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

drawRoundCorner: Int -> Int -> Position -> Model -> List (Svg a)
drawRoundCorner x y pos  model =
    case pos of
        TopLeftCorner ->
            drawRoundTopLeftCorner x y
        TopRightCorner ->
            drawRoundTopRightCorner x y
        BottomLeftCorner ->
            drawRoundBottomLeftCorner x y
        BottomRightCorner ->
            drawRoundBottomRightCorner x y
        TopLeftSlantedBottomLeft ->
            drawRoundTopLeftSlantedBottomLeftCorner x y
        TopLeftSlantedBottomRight ->
            drawRoundTopLeftSlantedBottomRightCorner x y
        TopRightSlantedBottomRight ->
            drawRoundTopRightSlantedBottomRight x y
        TopRightSlantedBottomLeft ->
            drawRoundTopRightSlantedBottomLeft x y
        TopRightSlantedTopLeft ->
            drawRoundTopRightSlantedTopLeft x y 
        VerticalTopDownJunctionTopLeft ->
            drawVerticalTopDownJunctionTopLeft x y
        SlantedRightJunctionRight ->
            drawRoundSlantedRightJunctionRight x y
        SlantedLeftJunctionLeft ->
            drawRoundSlantedLeftJunctionLeft x y
        SlantedRightJunctionLeft ->
            drawRoundSlantedRightJunctionLeft x y
        SlantedLeftJunctionRight ->
            drawRoundSlantedLeftJunctionRight x y
        BottomLeftLowHorizontal ->
            drawRoundBottomLeftLowHorizontalCorner x y
        BottomRightLowHorizontal ->
            drawRoundBottomRightLowHorizontalCorner x y
        BottomLeftSlantedTopLeft ->
            drawRoundBottomLeftSlantedTopLeftCorner x y
        BottomLeftSlantedTopRight ->
            drawRoundBottomLeftSlantedTopRightCorner x y
        BottomLeftSlantedBottomRight ->
            drawRoundBottomLeftSlantedBottomRightCorner x y
        BottomLeftSlantedTopRightLowHorizontal ->
            drawRoundBottomLeftSlantedTopRightLowHorizontal x y
        BottomRightSlantedTopRight ->
            drawRoundBottomRightSlantedTopRightCorner x y
        BottomRightSlantedTopLeftLowHorizontal ->
            drawRoundBottomRightSlantedTopLeftLowHorizontal x y
        BottomRightSlantedTopLeft ->
            drawRoundBottomRightSlantedTopLeftCorner x y
        BottomRightSlantedBottomLeft ->
            drawRoundBottomRightSlantedBottomLeft x y
        VerticalTopDownJunctionBottomLeft ->
            drawVerticalTopDownJunctionBottomLeft x y
        VerticalTopDownJunctionBottomRight ->
            drawVerticalTopDownJunctionBottomRight x y
        TopLeftSlantedTopRight ->
            drawRoundTopLeftSlantedTopRightCorner x y
        VerticalTopDownJunctionTopRight ->
            drawVerticalTopDownJunctionTopRight x y
        TopLeftBigCurve ->
            drawTopLeftBigCurve x y
        TopRightBigCurve ->
            drawTopRightBigCurve x y
        BottomLeftBigCurve ->
            drawBottomLeftBigCurve x y
        BottomRightBigCurve ->
            drawBottomRightBigCurve x y

drawRoundTopLeftCorner x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        endX = measureX x + textWidth / 2  --circular arc 
        endY = measureY y + textHeight / 2 + textWidth / 2 --then the rest is line
    in
    [drawArc startX startY endX endY arcRadius
    ,drawLine endX endY endX (measureY y +  textHeight) color
    ]

drawTopLeftBigCurve x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        endX = measureX x - textWidth / 2
        endY = measureY y + textHeight
    in
        [drawArc startX startY endX endY (arcRadius * 4)]

drawBottomLeftBigCurve x y =
    let
        startX = measureX x - textWidth / 2
        startY = measureY y
        endX = measureX x + textWidth
        endY = measureY y + textHeight / 2
    in
        [drawArc startX startY endX endY (arcRadius * 4)]

drawBottomRightBigCurve x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        endX = measureX x + textWidth  + textWidth / 2
        endY = measureY y
    in
        [drawArc startX startY endX endY (arcRadius * 4)]


drawTopRightBigCurve x y =
    let
        startX = measureX x + textWidth + textWidth / 2
        startY = measureY y + textHeight
        endX = measureX x
        endY = measureY y + textHeight / 2
    in
        [drawArc startX startY endX endY (arcRadius * 4)]

drawRoundTopLeftSlantedTopRightCorner x y =
    let
        lstartX = measureX x + textWidth
        lstartY = measureY y
        lendX = measureX x + textWidth * 3 / 4
        lendY = measureY y + textHeight * 1 / 4
        l2startX = measureX x + textWidth / 2
        l2startY = measureY y + textHeight
        l2endX = measureX x + textWidth / 2
        l2endY = measureY y + textHeight * 3 / 4
    in
    [drawArc lendX lendY l2endX l2endY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ]


drawVerticalTopDownJunctionTopRight x y =
    let
        lstartX = measureX x + textWidth
        lstartY = measureY y
        lendX = measureX x + textWidth * 3 / 4
        lendY = measureY y + textHeight * 1 / 4
        l2startX = measureX x + textWidth / 2
        l2startY = measureY y + textHeight
        l2endX = measureX x + textWidth / 2
        l2endY = measureY y + textHeight * 3 / 4
        l3startX = measureX x + textWidth / 2
        l3startY = measureY y
        l3endX = measureX x + textWidth / 2
        l3endY = measureY y + textHeight
    in
    [drawArc lendX lendY l2endX l2endY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ,drawLine l3startX l3startY l3endX l3endY color
    ]

drawRoundTopRightSlantedBottomRight x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        lstartX = measureX x + textWidth
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth * 3 /4
        lendY = measureY y + textHeight * 3 /4 
    in
    [drawArc lendX lendY startX startY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundTopRightSlantedBottomLeft x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        lstartX = measureX x
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth * 1 / 4
        lendY = measureY y + textHeight * 3 / 4
    in
    [drawArc lendX lendY startX startY (arcRadius * 3 / 4)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundTopRightSlantedTopLeft x y =
    let
        lstartX = measureX x + textWidth / 2
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight * 3 / 4
        l2startX = measureX x
        l2startY = measureY y
        l2endX = measureX x + textWidth * 1 /4
        l2endY = measureY y + textHeight * 1 /4
    in
    [drawArc lendX lendY l2endX l2endY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ]

drawVerticalTopDownJunctionTopLeft x y =
    let
        lstartX = measureX x + textWidth / 2
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight * 3 / 4
        l2startX = measureX x
        l2startY = measureY y
        l2endX = measureX x + textWidth * 1 /4
        l2endY = measureY y + textHeight * 1 /4
        l3startX = measureX x + textWidth / 2
        l3startY = measureY y
        l3endX = measureX x + textWidth / 2
        l3endY = measureY y + textHeight
    in
    [drawArc lendX lendY l2endX l2endY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ,drawLine l3startX l3startY l3endX l3endY color
    ]

drawRoundTopLeftSlantedBottomLeftCorner x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        lstartX = measureX x
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth * 1 /4
        lendY = measureY y + textHeight * 3 /4 
    in
    [drawArc startX startY lendX lendY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundTopLeftSlantedBottomRightCorner x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        lstartX = measureX x + textWidth
        lstartY = measureY y + textHeight
        lendX = measureX x + textWidth * 3 /4
        lendY = measureY y + textHeight * 3 /4 
    in
    [drawArc startX startY lendX lendY (arcRadius * 3 / 4)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundBottomLeftSlantedTopLeftCorner x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        lstartX = measureX x
        lstartY = measureY y
        lendX = measureX x + textWidth * 1 /4
        lendY = measureY y + textHeight * 1 /4 
    in
    [drawArc lendX lendY startX startY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundBottomLeftSlantedTopRightCorner x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        lstartX = measureX x + textWidth
        lstartY = measureY y
        lendX = measureX x + textWidth * 3 /4
        lendY = measureY y + textHeight * 1 /4 
    in
    [drawArc lendX lendY startX startY (arcRadius * 3 / 4)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundBottomLeftSlantedBottomRightCorner x y =
    let
        lstartX = measureX x + textWidth / 2
        lstartY = measureY y
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight * 1 /4 
        l2startX = measureX x + textWidth
        l2startY = measureY y + textHeight
        l2endX = measureX x + textWidth * 3 / 4
        l2endY = measureY y + textHeight * 3 / 4
    in
    [drawArc lendX lendY l2endX l2endY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ]

drawRoundBottomLeftSlantedTopRightLowHorizontal x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight
        lstartX = measureX x + textWidth
        lstartY = measureY y
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight / 2
    in
    [drawArc lendX lendY startX startY (arcRadius * 1.5)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundBottomRightSlantedTopLeftLowHorizontal x y =
    let
        startX = measureX x
        startY = measureY y + textHeight
        lstartX = measureX x
        lstartY = measureY y
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight / 2
    in
    [drawArc startX startY lendX lendY (arcRadius * 1.5)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundSlantedRightJunctionRight x y =
    let
        startX = measureX x + textWidth
        startY = measureY y + textHeight / 2
        endX = measureX x + textWidth * 1 /4
        endY = measureY y + textHeight * 3 /4 
        lstartX = measureX x + textWidth
        lstartY = measureY y
        lendX = measureX x
        lendY = measureY y + textHeight
    in
    [drawArc startX startY endX endY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]


drawRoundSlantedRightJunctionLeft x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        endX = measureX x + textWidth * 3 / 4
        endY = measureY y + textHeight * 1 / 4
        lstartX = measureX x + textWidth
        lstartY = measureY y
        lendX = measureX x
        lendY = measureY y + textHeight
    in
    [drawArc startX startY endX endY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundSlantedLeftJunctionLeft x y =
    let
        startX = measureX x + textWidth * 3 / 4
        startY = measureY y + textHeight * 3 / 4
        endX = measureX x
        endY = measureY y + textHeight / 2
        lstartX = measureX x
        lstartY = measureY y
        lendX = measureX x + textWidth
        lendY = measureY y + textHeight
    in
    [drawArc startX startY endX endY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundSlantedLeftJunctionRight x y =
    let
        startX = measureX x + textWidth * 1 /4
        startY = measureY y + textHeight * 1 / 4
        endX = measureX x + textWidth 
        endY = measureY y + textHeight / 2
        lstartX = measureX x
        lstartY = measureY y
        lendX = measureX x + textWidth
        lendY = measureY y + textHeight
    in
    [drawArc startX startY endX endY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundBottomRightSlantedTopRightCorner x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        lstartX = measureX x + textWidth
        lstartY = measureY y
        lendX = measureX x + textWidth * 3 /4
        lendY = measureY y + textHeight * 1 /4 
    in
    [drawArc startX startY lendX lendY (arcRadius * 2)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundBottomRightSlantedTopLeftCorner x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        lstartX = measureX x
        lstartY = measureY y
        lendX = measureX x + textWidth * 1 / 4
        lendY = measureY y + textHeight * 1 / 4
    in
    [drawArc startX startY lendX lendY (arcRadius * 3 / 4)
    ,drawLine lstartX lstartY lendX lendY color
    ]

drawRoundBottomRightSlantedBottomLeft x y =
    let
        lstartX = measureX x + textWidth / 2
        lstartY = measureY y
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight * 1 / 4
        l2startX = measureX x
        l2startY = measureY y + textHeight
        l2endX = measureX x + textWidth * 1 / 4
        l2endY = measureY y + textHeight * 3 / 4
    in
    [drawArc l2endX l2endY lendX lendY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ]

drawVerticalTopDownJunctionBottomLeft x y =

    let
        lstartX = measureX x + textWidth / 2
        lstartY = measureY y
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight * 1 / 4
        l2startX = measureX x
        l2startY = measureY y + textHeight
        l2endX = measureX x + textWidth * 1 / 4
        l2endY = measureY y + textHeight * 3 / 4
        l3startX = measureX x + textWidth / 2
        l3startY = measureY y
        l3endX = measureX x + textWidth / 2
        l3endY = measureY y + textHeight
    in
    [drawArc l2endX l2endY lendX lendY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ,drawLine l3startX l3startY l3endX l3endY color
    ]

drawVerticalTopDownJunctionBottomRight x y =
    let
        lstartX = measureX x + textWidth / 2
        lstartY = measureY y
        lendX = measureX x + textWidth / 2
        lendY = measureY y + textHeight * 1 /4 
        l2startX = measureX x + textWidth
        l2startY = measureY y + textHeight
        l2endX = measureX x + textWidth * 3 / 4
        l2endY = measureY y + textHeight * 3 / 4
        l3startX = measureX x + textWidth / 2
        l3startY = measureY y
        l3endX = measureX x + textWidth / 2
        l3endY = measureY y + textHeight
    in
    [drawArc lendX lendY l2endX l2endY (arcRadius * 4)
    ,drawLine lstartX lstartY lendX lendY color
    ,drawLine l2startX l2startY l2endX l2endY color
    ,drawLine l3startX l3startY l3endX l3endY color
    ]

drawRoundBottomLeftCorner x y =
    let
        startX = measureX x + textWidth / 2
        startY = measureY y + textHeight/2 - textWidth / 2
        endX = measureX x + textWidth
        endY = measureY y + textHeight / 2
    in
    [drawArc startX startY endX endY arcRadius
    ,drawLine startX startY startX (measureY y) color
    ]

drawRoundBottomLeftLowHorizontalCorner x y =
    let
        startX = measureX x + textWidth / 2
        startY = measureY y + textHeight - textWidth / 2
        endX = measureX x + textWidth
        endY = measureY y + textHeight
    in
    [drawArc startX startY endX endY arcRadius
    ,drawLine startX startY startX (measureY y) color
    ]


drawRoundBottomRightLowHorizontalCorner x y =
    let
        startX = measureX x
        startY = measureY y + textHeight
        endX = measureX x + textWidth / 2
        endY = measureY y + textHeight - textWidth / 2
    in
    [drawArc startX startY endX endY arcRadius
    ,drawLine endX endY endX (measureY y) color
    ]

drawRoundTopRightCorner x y =
    let
        startX = measureX x + textWidth / 2
        startY = measureY y + textWidth / 2 + textHeight / 2
        endX = measureX x
        endY = measureY y + textHeight / 2
    in
    [drawArc startX startY endX endY arcRadius 
    ,drawLine startX startY startX (measureY y + textHeight) color
    ]

drawRoundBottomRightCorner x y =
    let
        startX = measureX x
        startY = measureY y + textHeight / 2
        endX = measureX x + textWidth / 2
        endY = measureY y + textHeight / 2 - textWidth / 2
    in
    [drawArc startX startY endX endY arcRadius 
    ,drawLine endX endY endX (measureY y) color
    ]


drawIntersection: Int -> Int -> Type ->  Model -> List (Svg a)
drawIntersection x y itype model =
    let
        --vertical line
        v1startX = measureX x + textWidth / 2
        v1endX = v1startX 
        v1startY = measureY y
        v1endY = measureY y + textHeight / 2 
 
        -- v line part 2
        v2startX = measureX x + textWidth / 2 
        v2endX = v2startX 
        v2startY = measureY y + textHeight / 2 
        v2endY = measureY y + textHeight

        --horizontal line
        h1startX = measureX x 
        h1endX = measureX x + textWidth / 2 
        h1startY = measureY y + textHeight / 2 
        h1endY = h1startY

        --h line part 2
        h2startX = measureX x + textWidth / 2 
        h2endX = h2startX + textWidth
        h2startY = measureY y + textHeight / 2
        h2endY = h2startY 

        v1Line = drawLine v1startX v1startY v1endX v1endY color
        v2Line = drawLine v2startX v2startY v2endX v2endY color
        h1Line = drawLine h1startX h1startY h1endX h1endY color
        h2Line = drawLine h2startX h2startY h2endX h2endY color
        
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
            ,stroke colorText
            ,strokeWidth <| toString lineWidth
            ,strokeLinecap "round"
            ,strokeLinejoin "mitter"
            ]
            []


drawText: Int -> Int -> Char -> Svg a
drawText x' y' char =
    let x'' = measureX x' - textWidth / 4
        y'' = measureY y' + textHeight * 3 / 4
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
