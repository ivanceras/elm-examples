
           ,(isVertical char
            ,Element Vertical
            )

           ,(isHorizontal char 
            ,Element Horizontal
            )

           ,(isLowHorizontal char
            ,Action Low Horizontal Extend Left Full
            )

            ,( isLowHorizontal char
            && isNeighbor left isVertical
            ,Action Low Horizontal Extend Left Half
            )
            
            ,(isLowHorizontal char
            && isNeighbor right isSlantLeft
            ,Action Low Horizontal Extend Right Full
            )
            
            ,(isLowHorizontal char
            && isNeighbor right isVertical 
            ,Action Low Horizontal Extend Right Half
            )
            
            ,(isLowHorizontal char
            && isNeighbor bottomLeft isVertical 
            ,Action Low Horizontal Extend Left Half
            )
            
            ,(isLowHorizontal char
            && isNeighbor bottomRight isVertical 
            ,Action Low Horizontal Extend Right Half
            )
            
            ,(isLowHorizontal char
            ,Element Low Horizontal
            )
            

            ,(isNeighbor top isVertical 
                && isNeighbor bottom isVertical
                && isNeighbor left isHorizontal 
                && isNeighbor right isHorizontal 
            ,Juncion (Junction4 Top Bottom Left Right) Sharp 
            )

            ,(
                isNeighbor top isVertical
            ,Junction (Junction3 Top Bottom Left) Sharp
            )

            ,(
                isNeighbor top isVertical 
                && isNeighbor bottom isVertical
                && isNeighbor right isHorizontal 
            ,Junction (Junction3 Top Bottom Right) Sharp
            )

            ,( isNeighbor left isHorizontal 
                && isNeighbor right isHorizontal
                && isNeighbor top isVertical 
            ,Junction (Junction3 Top Left Right) Sharp
            )

            ,( isNeighbor left isHorizontal 
                && isNeighbor right isHorizontal
                 && isNeighbor bottom isVertical 
            ,Junction (Junction3 Bottom Left Right) Sharp


            ,(
            isNeighbor top isVertical && isNeighbor left isHorizontal
            ,Junction (Junction2 Bottom Right) Sharp
            )

            ,(
             isNeighbor top isVertical && isNeighbor right isHorizontal
             ,Junction (Junction2 Bottom Left) Sharp
             )
             
           ,(isRoundCorner char 
            isNeighbor topRight isSlantRight
            && isNeighbor bottomLeft isSlantRight
            && isNeighbor right isHorizontal
            ,Junction (Junction3 TopRight BottomRight Right) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical
            && isNeighbor bottom isVertical
            && isNeighbor topLeft isSlantLeft 
            ,Junction (Junction3 Top Down TopLeft) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor topLeft isSlantLeft
            && isNeighbor bottomRight isSlantLeft
            && isNeighbor left isHorizontal 
            ,Junction (Junction3 TopLeft BottomRight Left) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor topRight isSlantRight
            && isNeighbor bottomLeft isSlantRight
            && isNeighbor left isHorizontal 
            ,Junction (Junction3 TopRight BottomLeft Left) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor topLeft isSlantLeft
            && isNeighbor bottomRight isSlantLeft
            && isNeighbor right isHorizontal 
            ,Junction (Junction3 TopLeft BottomRight Right) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical
            && isNeighbor bottom isVertical
            && isNeighbor bottomLeft isSlantRight 
            ,Junction (Junction3 Top Bottom BottomLeft) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical
            && isNeighbor bottom isVertical
            && isNeighbor bottomRight isSlantLeft 
            ,Junction (Junction3 Top Bottom BottomRight) Round 
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical
            && isNeighbor bottom isVertical
            && isNeighbor topRight isSlantRight 
            ,Junction (Junction3 Top Bottom TopRight) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor bottom isVertical 
            && isNeighbor right isHorizontal 
            ,Junction (Junction2 Bottom Right) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor bottom isVertical
            && isNeighbor left isHorizontal 
            ,Junction (Junction2 Bottom Left) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor bottom isVertical
            && isNeighbor topRight isSlantRight 
            ,Junction (Junction2 Bottom TopRight) Round 
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isRoundCorner
            && isNeighbor bottomLeft isOpenCurve 
            ,Curve TopLeft Big
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isRoundCorner
            && isNeighbor bottomRight isCloseCurve 
            ,Curve TopRight Big
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isRoundCorner
            && isNeighbor topLeft isOpenCurve 
            ,Curve BottomLeft Big
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isRoundCorner
            && isNeighbor topRight isCloseCurve 
            ,Curve BottomRight Big
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical
            && isNeighbor right isHorizontal 
            ,Corner BottomLeft Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical 
            && isNeighbor right isLowHorizontal 
            ,Corner BottomLeft Low Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical 
            && isNeighbor left isLowHorizontal 
            ,Corner BottomRight Low Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isHorizontal 
                && isNeighbor topLeft isSlantLeft 
            ,Junction Mid (Junction2 Right TopLeft) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isHorizontal 
                && isNeighbor topRight isSlantRight 
            ,Junction Mid (Junction2 Right TopRight) Round 
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical 
                && isNeighbor bottomRight isSlantLeft 
            ,Junction Mid (Junction2 Top BottomRight) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isHorizontal 
                && isNeighbor topRight isSlantRight 
            ,Junction Mid (Junction2 Left TopRight)
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isLowHorizontal 
                && isNeighbor topRight isSlantRight 
            ,Junction Low (Junction2 Right TopRight) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isLowHorizontal 
                && isNeighbor topLeft isSlantLeft 
            ,Junction Low (Junction2 Left TopLeft) Round
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isHorizontal
                && isNeighbor topLeft isSlantLeft 
            ,RoundCorner BottomRightSlantedTopLeft
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical
                && isNeighbor bottomLeft isSlantRight 
            ,RoundCorner BottomRightSlantedBottomLeft
            )
            
           ,(isRoundCorner char 
            &&isNeighbor top isVertical
            && isNeighbor left isHorizontal 
            ,RoundCorner BottomRightCorner
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isHorizontal
            && isNeighbor bottom isRoundCorner 
            ,RoundCorner TopLeftCorner
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isHorizontal 
            && isNeighbor bottom isRoundCorner 
            ,RoundCorner TopRightCorner
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isHorizontal 
            && isNeighbor top isRoundCorner 
            ,RoundCorner BottomRightCorner
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isHorizontal
            && isNeighbor top isRoundCorner 
            ,RoundCorner BottomLeftCorner
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isHorizontal
            && isNeighbor bottomLeft isSlantRight 
            ,RoundCorner TopLeftSlantedBottomLeft
            )
            
           ,(isRoundCorner char 
            &&isNeighbor right isHorizontal
            && isNeighbor bottomRight isSlantLeft 
            ,RoundCorner TopLeftSlantedBottomRight
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isHorizontal
            && isNeighbor bottomRight isSlantLeft 
            ,RoundCorner TopRightSlantedBottomRight
            )
            
           ,(isRoundCorner char 
            &&isNeighbor left isHorizontal
            && isNeighbor bottomLeft isSlantRight 
            ,RoundCorner TopRightSlantedBottomLeft
            )
            
           ,(isRoundCorner char 
            &&isNeighbor bottom isVertical
            && isNeighbor topLeft isSlantLeft 
            ,RoundCorner TopRightSlantedTopLeft
            )
           
