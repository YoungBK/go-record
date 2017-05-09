import Html exposing (Html, Attribute, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing (x1, x2, cx, y1, y2, cy, r, stroke, strokeWidth, fill)
import Dict exposing (Dict)
import Maybe exposing (Maybe)

-- import GoRecordStylesheet

main =
  Html.beginnerProgram
    { model = game
    , view = view
    , update = update
    }


-- MODEL

type Player = Black | White
type alias Position = (Int,Int)
type alias Game = { moves : List Position, positions : Dict Position Player, nextPlayer : Player }

game : Game
game = { moves = [], positions = Dict.empty, nextPlayer = Black }


-- UPDATE

type Msg = Menu | Move Position

update : Msg -> Game -> Game
update msg game =
  case msg of
    Move pos -> { game | moves = pos::game.moves
                       , nextPlayer = next game.nextPlayer
                       , positions = Dict.insert pos game.nextPlayer game.positions }
    Menu     -> game

next : Player -> Player
next p =
    case p of
        Black -> White
        White -> Black

-- VIEW

view : Game -> Html Msg
view game =
    div [] (list_positions game::[build_board 19 19 game])

list_positions : Game -> Html Msg
list_positions game =
    div [] [ text (toString game) ]

build_board : Int -> Int -> Game -> Html Msg
build_board cols rows game =
    div [ board ] (build_board_cells cols rows game)
    
--     div [ board ] (build_labels cols rows ++ build_board_cells cols rows game)
--     div [ board ] (buildSvg cols rows::build_board_cells cols rows game)

-- build_labels : Int -> Int -> Html Msg
-- build_labels cols rows =

build_board_cells : Int -> Int -> Game -> List(Html Msg)
build_board_cells cols rows game =
    case rows of
        0 -> []
        _ -> (build_row_cells cols rows game) ++ build_board_cells cols (rows - 1) game

build_row_cells : Int -> Int -> Game -> List(Html Msg)
build_row_cells cols row game =
    case cols of
        0 -> []
        _ -> build_cell (20 - cols) (20 - row) game::build_row_cells (cols - 1) row game

build_cell : Int -> Int -> Game -> Html Msg
build_cell col row game =
    div [ (onClick (Move (col, row))), cell ] [ drawCell col row game ]

-- STYLES

board : Attribute msg
board =
  style
  [ ("display", "-webkit-flex")
  , ("display", "flex")
  , ("width", "855px")
  , ("height", "855px")
  , ("background-color", "goldenrod")
  , ("flex-wrap", "wrap")
  , ("align-content", "flex-start")
  ]

cell : Attribute msg
cell =
    style [ ("width", "45px")
          , ("height", "45px")
          , ("margin", "0px")
          ]

center : Position
center = (22,22)

top : Position
top = (22,0)

left : Position
left = (0,22)

right : Position
right = (45,22)

bottom : Position
bottom = (22, 45)

-- lines_nw_corner : Attribute msg
-- lines_nw_corner =
--     style [ ("background", url(genlines (center,bottom) (center,right) )) ]

type alias Line = (Position,Position)

drawCell : Int -> Int -> Game -> Html msg
drawCell col row game =
    svg [ cell ] ((cellLines (calcLines col row)) ++ (starPoint col row) ++ (stone col row game))
    
--     text (toString (col,row))

calcLines : Int -> Int -> List Line
calcLines col row =
    case (col,row) of
        (1,1)   -> [(center, bottom), (center,right)]
        (1,19)  -> [(center, top), (center,right)]
        (19,1)  -> [(center, bottom), (center,left)]
        (19,19) -> [(center, top), (center,left)]
        (1,_)   -> [(center, right), (top,bottom)]
        (19,_)  -> [(center, left), (top,bottom)]
        (_,1)   -> [(center, bottom), (left,right)]
        (_,19)  -> [(center, top), (left,right)]
        _       -> [(top, bottom), (left,right)]

cellLines : List Line -> List (Svg msg)
cellLines lines =
    List.map positionToLine lines

positionToLine : Line -> Svg msg
positionToLine (point1,point2) =
    line (createLineStyle point1 point2) [ ]

createLineStyle : Position -> Position -> List (Svg.Attribute msg)
createLineStyle (px1,py1) (px2,py2) =
    [ x1 (toString px1), y1 (toString py1), x2 (toString px2), y2 (toString py2), stroke "rgb(0,0,0)", strokeWidth "1.25" ]


starPoint : Int -> Int -> List (Svg msg)
starPoint col row =
    if (col == 4 || col == 10 || col == 16) && (row == 4 || row == 10 || row == 16) then
        [ circle [ cx "22", cy "22", r "3", stroke "black", strokeWidth "1.5", fill "black" ] [] ]
    else
        [ ]

        
stone : Int -> Int -> Game -> List (Svg msg)
stone col row game =
    case Dict.get (col,row) game.positions of
        Just Black -> [ circle [ cx "22", cy "22", r "20", stroke "black", strokeWidth "1.5", fill "black" ] [] ]
        Just White -> [ circle [ cx "22", cy "22", r "20", stroke "white", strokeWidth "1.5", fill "white" ] [] ]
        Nothing     -> []

-- div []
--     [ button [ onClick (Board(1,1)) ] [ text "+" ]
--     , button [ onClick (Board(1,2)) ] [ text "+" ]
--     , button [ onClick (Board(2,1)) ] [ text "+" ]
--     , button [ onClick (Board(2,2)) ] [ text "+" ]
--    ]

{-
  Version to build the grid on the parent div
  but it pushes the intersection divs down, so
  this'll work best as a background svg using
  a data uri on a stylesheet. For now, just use
  the cell by cell version above
-}

buildSvg : Int -> Int -> Svg Msg
buildSvg cols rows =
    svg [ boardSize ] (buildLines cols rows ++ buildStarPoints)

buildLines : Int -> Int -> List(Svg Msg)
buildLines cols rows =
    buildVerticalLines cols ++ buildHorizontalLines rows

        
buildVerticalLines : Int -> List(Svg Msg)
buildVerticalLines cols =
    let
        x = (cols - 1) * 45 + 22
    in
        if cols == 0 then
            []
        else
            positionToLine ((x,22),(x,832))::buildVerticalLines (cols - 1)

buildHorizontalLines : Int -> List(Svg Msg)
buildHorizontalLines rows =
    let
        y = (rows - 1) * 45 + 22
    in
        if rows == 0 then
            []
        else
            positionToLine ((22,y),(832,y))::buildHorizontalLines (rows - 1)

buildStarPoints : List(Svg Msg)
buildStarPoints =
    List.concatMap (\x -> List.map (buildStarPoint x) [4,10,16]) [4,10,16]

buildStarPoint : Int -> Int -> Svg Msg
buildStarPoint col row =
    let
        xpos = (col - 1) * 45 + 22
        ypos = (row - 1) * 45 + 22
    in
        circle [ cx (toString xpos), cy (toString ypos), r "3", stroke "black", strokeWidth "1.5", fill "black" ] []
                            
boardSize : Attribute msg
boardSize =
    style
    [ ("width", "855px")
    , ("height", "855px")
    ]

