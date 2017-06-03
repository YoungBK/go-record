import Html exposing (Html, Attribute, div, text)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing (x1, x2, cx, y1, y2, cy, r, stroke, strokeWidth, fill, opacity)
import Dict exposing (Dict)
import Maybe exposing (Maybe)

-- import GoRecordStylesheet

main =
  Html.beginnerProgram
    { model = gameModel
    , view = view
    , update = update
    }


-- MODEL

type Player = Black | White
type alias Position = (Int,Int)
type alias GamePosition = Dict Position Player
type alias Game = { moves : List Position
                  , positions : GamePosition
                  , nextPlayer : Player
                  , hover : Maybe Position
                  , liberties : List Position
                  , neighborGroup : List Position
                  , opponentGroups : List (List Position)
                  , mostRecentCaptures : List (List Position)
                  }

gameModel : Game
gameModel = { moves = []
            , positions = Dict.empty
            , nextPlayer = Black
            , hover = Nothing
            , liberties = []
            , neighborGroup = []
            , opponentGroups = []
            , mostRecentCaptures = []
            }


-- UPDATE

type Msg = Menu | Move Position | Hover (Maybe Position)

update : Msg -> Game -> Game
update msg game =
  case msg of
    Hover mpos -> { game | hover = mpos}
    Menu       -> game
    Move pos   ->
      let
        group = findContiguous pos game.nextPlayer game.positions
        adjacentOpponentGroups = adjacentOpponentGroupsOf pos game.nextPlayer game.positions
        newPositions = Dict.insert pos game.nextPlayer game.positions
        -- captures = capturesOf pos adjacentOpponentGroups newPositions
      in
        { game | moves = pos::game.moves
        , nextPlayer = next game.nextPlayer
        , positions = newPositions -- removeCaptures captures newPositions
        , hover = Nothing
        , liberties = findLiberties group game.nextPlayer newPositions
        , neighborGroup = group
        , opponentGroups = adjacentOpponentGroups
        , mostRecentCaptures = [] -- captures
        }

next : Player -> Player
next p =
    case p of
        Black -> White
        White -> Black

-- GAME MECHANICS

{-
canMove : Player -> Position -> Bool
canMove player pos =
    if containsStone pos then
        False
    else if isSuicide pos then
        False
    else if isKo player pos then
        False
    else
-}      
    
positionEmpty : Position -> GamePosition -> Bool
positionEmpty pos position =
    Dict.get pos position == Nothing
    
isSuicide : Position -> Game -> Bool
isSuicide move game = False
                 
isKo : Position -> Game -> Bool
isKo move game = False

{-
isSuicide : Position -> Game -> Bool
isSuicide pos game =
    libertyCount (pos::findContiguous pos game) game == 0 && List.length (stonesCapturedByMove pos game) == 0

isKo : Position -> Game -> Bool
isKo move game =
    let
        captured = stonesCapturedByMove move game
        firstCapture = List.head captured
    in
        if List.length captured /= 1 then
            False
        else
            case firstCapture of
                Nothing -> False
                Just stonepos -> libertyCount (stonepos::(findContiguous stonepos game)) game == 0 -- &&
            -- List.contains kolist captured (or maybe pos)
-}

adjacentOpponentGroupsOf : Position -> Player -> GamePosition -> List (List Position)
adjacentOpponentGroupsOf pos player gamePosition =
  let
    opponent = next player
    adjacentOpponents = List.filter (inGame opponent gamePosition) (calcNeighbors pos)
  in
    adjacentOpponentGroupsOfIter adjacentOpponents opponent gamePosition []

adjacentOpponentGroupsOfIter : List Position -> Player -> GamePosition -> List (List Position) -> List (List Position)
adjacentOpponentGroupsOfIter adjacentOpponents opponent gamePosition result =
  case adjacentOpponents of
    []        -> result
    pos::tail ->
      let
        newResult = if List.any ((==) True) (List.map (List.member pos) result) then
                      result
                    else
                      (findContiguous pos opponent gamePosition)::result
      in
        adjacentOpponentGroupsOfIter tail opponent gamePosition newResult

findLiberties : List Position -> Player -> GamePosition -> List Position
findLiberties group player positions =
  findLibertiesIter group player positions []

findLibertiesIter : List Position -> Player -> GamePosition -> List Position -> List Position
findLibertiesIter group player positions liberties =
  case group of
    []        -> liberties
    pos::tail ->
      let
        calcedLiberties = merge (libertiesOf pos player positions) liberties
      in
        findLibertiesIter tail player positions calcedLiberties

libertiesOf : Position -> Player -> Dict Position Player -> List Position
libertiesOf pos player positions =
  if Dict.get pos positions == Just player then
    List.filter (\p -> Dict.get p positions == Nothing) (calcNeighbors pos)
  else
    [] -- some kind of error
    
    -- List.filter ((==) Nothing) (List.map (\p -> Dict.get p game.positions) (calcNeighbors pos))

merge : List Position -> List Position -> List Position
merge toInsert list =
  case toInsert of
    [] -> list
    pos::tail -> merge tail (mergeIfNotExists pos list)

mergeIfNotExists : Position -> List Position -> List Position
mergeIfNotExists pos list =                 
  if List.member pos list then
    list
  else
    pos::list
  
stonesCapturedByMove : Position -> Game -> List Position
stonesCapturedByMove move game = []

findContiguous : Position -> Player -> GamePosition -> List Position
findContiguous pos player gamePosition =
    findNeighbors [pos] player gamePosition []

findNeighbors : List Position -> Player -> GamePosition -> List Position -> List Position
findNeighbors toSearch player gamePosition found =
  case toSearch of
    []        -> found
    pos::tail ->
      let
        neighbors = List.filter (inGame player gamePosition) (calcNeighbors pos)
        newNeighbors = List.filter (\n -> not (List.member n found)) neighbors
        newFound = if List.member pos found then
                     newNeighbors ++ found
                   else
                     pos::(newNeighbors ++ found)
      in
        findNeighbors (tail ++ newNeighbors) player gamePosition newFound

calcNeighbors : Position -> List Position
calcNeighbors (x,y) =
  List.filter withinBounds [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]

withinBounds : Position -> Bool
withinBounds (x,y) =
  x >=1 && x <= 19 && y >= 1 && y <= 19

inGame : Player -> GamePosition -> Position -> Bool
inGame player positions pos =
    Dict.get pos positions == Just player
    
-- VIEW

view : Game -> Html Msg
view game =
    div [] (list_positions game::[build_board 19 19 game])

list_positions : Game -> Html Msg
list_positions game =
    div [] [ text (toString game) ]

build_board : Int -> Int -> Game -> Html Msg
build_board cols rows game =
    div [ board, onMouseLeave (Hover Nothing) ] (build_board_cells cols rows game)
    
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
        _ -> build_cell ((20 - cols), (20 - row)) game::build_row_cells (cols - 1) row game

build_cell : Position -> Game -> Html Msg
build_cell pos game =
    div (cell::actions pos game.positions) [ drawCell pos game ]


actions : Position -> GamePosition -> List (Attribute Msg)
actions pos positions =
    if positionEmpty pos positions then
        onClick (Move pos) :: [ onMouseEnter (Hover (Just pos)) ]
    else
        [ onMouseEnter (Hover Nothing) ]

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

type alias Line = (Position,Position)

drawCell : Position -> Game -> Html Msg
drawCell pos game =
    svg [ cell ] ((cellLines (calcLines pos)) ++ starPoint pos ++ stone pos game ++ hover pos game)
--     text (toString (col,row))

calcLines : Position -> List Line
calcLines pos =
    case pos of
        (1,1)   -> [(center, bottom), (center,right)]
        (1,19)  -> [(center, top), (center,right)]
        (19,1)  -> [(center, bottom), (center,left)]
        (19,19) -> [(center, top), (center,left)]
        (1,_)   -> [(center, right), (top,bottom)]
        (19,_)  -> [(center, left), (top,bottom)]
        (_,1)   -> [(center, bottom), (left,right)]
        (_,19)  -> [(center, top), (left,right)]
        _       -> [(top, bottom), (left,right)]

cellLines : List Line -> List (Svg Msg)
cellLines lines =
    List.map positionToLine lines

positionToLine : Line -> Svg Msg
positionToLine (point1,point2) =
    line (createLineStyle point1 point2) []

createLineStyle : Position -> Position -> List (Svg.Attribute Msg)
createLineStyle (px1,py1) (px2,py2) =
    [ x1 (toString px1), y1 (toString py1), x2 (toString px2), y2 (toString py2), stroke "rgb(0,0,0)", strokeWidth "1.25" ]

starPoint : Position -> List (Svg Msg)
starPoint (col,row) =
    if (col == 4 || col == 10 || col == 16) && (row == 4 || row == 10 || row == 16) then
        [ circle [ cx "22", cy "22", r "3", stroke "black", strokeWidth "1.5", fill "black" ] [] ]
    else
        []
        
stone : Position -> Game -> List (Svg Msg)
stone pos game =
  case Dict.get pos game.positions of
    Just player -> drawStone player defaultOpacity
    Nothing     -> []

hover : Position -> Game -> List (Svg Msg)
hover pos game =
  case game.hover of
    Just hoverpos -> if hoverpos == pos then
                       drawHover game
                     else
                       []
    Nothing       -> []

drawHover : Game -> List (Svg Msg)
drawHover game =
  drawStone game.nextPlayer hoverOpacity

drawStone : Player -> String -> List(Svg Msg)
drawStone player opacityValue =
  [ circle [ cx "22", cy "22", r "20", stroke (color player), strokeWidth "1.5", fill (color player), opacity opacityValue ] [] ]

color : Player -> String
color player =
  case player of
    Black -> "black"
    White -> "white"
        
hoverOpacity = "0.5"
defaultOpacity = "1.0"

-- STYLES

board : Attribute Msg
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

cell : Attribute Msg
cell =
    style [ ("width", "45px")
          , ("height", "45px")
          , ("margin", "0px")
          ]

-- lines_nw_corner : Attribute msg
-- lines_nw_corner =
--     style [ ("background", url(genlines (center,bottom) (center,right) )) ]

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

