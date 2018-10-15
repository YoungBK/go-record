import Html exposing (Html, Attribute, div, text)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing (x1, x2, cx, y1, y2, cy, r, stroke, strokeWidth, fill, opacity)
import Dict exposing (Dict)
import Maybe exposing (Maybe)
import Set exposing (Set)
-- import String exposing (cons)

-- import GoRecordStylesheet

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type Player = Black | White
type alias BoardPosition = List (Int, Char)
type alias Position = (Int,Int)
type alias GamePosition = Dict Position Player
type alias PositionHistory = Set BoardPosition
type alias Game = { moves : List Position
                  , positions : GamePosition
                  , positionCheck : PositionHistory
                  , nextPlayer : Player
                  , hover : Maybe Position
                  , liberties : List Position
                  , neighborGroup : List Position
                  , opponentGroups : List (List Position)
                  , mostRecentCaptures : List (List Position)
                  , error : Maybe String
                  , menu : MenuOption
                  }

gameModel : Game
gameModel = { moves = []                 -- ordered moves made
            , positions = Dict.empty     -- current positions on board
            , positionCheck = Set.empty  -- super-ko rule check
            , nextPlayer = Black         -- player to move
            , hover = Nothing            -- current mouse hover position
            , liberties = []             -- liberties of last move
            , neighborGroup = []         -- stones in the group of last move
            , opponentGroups = []        -- opponent groups adjacent to last move
            , mostRecentCaptures = []    -- groups captured by last move
            , error = Nothing            -- error in move attempt
            , menu = None
            }

-- INIT

init : (Game, Cmd Msg)
init = (gameModel, Cmd.none)

-- type alias Flags = { flag1: Int, flag2: String, ... }
-- initWithFlags : Flags -> (Game, Cmd Msg)
-- initWithFlags flags = (gameModel, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game = Sub.none

-- UPDATE

type MenuOption = Save | Load | Begin | Rewind | Back | Up | Down | Step | FastForward | End | None
  
type Msg = Menu MenuOption | Move Position | Hover (Maybe Position) | Save (Result Http.Error String) | Load (Result Http.Error String)

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    Hover mpos -> ({ game | hover = mpos}, Cmd.none)
    Menu option -> menuSelection option game
    Move pos   -> (makeMove pos game, Cmd.none)

menuSelection : MenuOption -> Game -> Game
menuSelection option game =
  case option of
    Save -> ({ game | menu = Save }, Cmd.none)
    Load -> ({ game | menu = Load }, Cmd.none)
    Begin -> ( { game | menu = Begin }, Cmd.none)
    Rewind -> (game, Cmd.none)
    Back -> (game, Cmd.none)
    Up -> (game, Cmd.none)
    Down -> (game, Cmd.none)
    Step -> (game, Cmd.none)
    FastForward -> (game, Cmd.none)
    End -> (game, Cmd.none)
    None -> { game | menu = None }

makeMove : Position -> Game -> Game
makeMove pos game =
  let
    currentPlayer = game.nextPlayer
    nextPlayer = next currentPlayer
    group = findContiguous pos currentPlayer game.positions
    adjacentOpponentGroups = adjacentOpponentGroupsOf pos currentPlayer game.positions
    positionsMoveAdded = Dict.insert pos game.nextPlayer game.positions
    captures = capturedByMove pos nextPlayer adjacentOpponentGroups positionsMoveAdded
    newPositions = removeCaptures captures positionsMoveAdded
    moveLiberties = findLiberties group currentPlayer newPositions
    newBoardPosition = buildBoardPosition newPositions nextPlayer
    newBoardPositionCheck = Set.insert newBoardPosition game.positionCheck
  in
    if not <| positionEmpty pos game.positions then
      { game | error = Just "Illegal move: Position occupied" }
    else if List.isEmpty moveLiberties then
      { game | error = Just "Illegal move: Cannot commit suicide" }
    else if Set.member newBoardPosition game.positionCheck then
      { game | error = Just "Illegal move: Duplicate position (ko rule)" }
    else
      { game | moves = pos::game.moves
      , positions = newPositions
      , positionCheck = newBoardPositionCheck
      , nextPlayer = nextPlayer
      , hover = Nothing
      , liberties = moveLiberties
      , neighborGroup = group
      , opponentGroups = adjacentOpponentGroups
      , mostRecentCaptures = captures
      , error = Nothing
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
positionEmpty pos positions =
  playerAt pos positions == Nothing
    
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
  if playerAt pos positions == Just player then
    List.filter (flip positionEmpty positions) (calcNeighbors pos)
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
  
capturedByMove : Position -> Player -> List (List Position) -> Dict Position Player -> List (List Position)
capturedByMove move player groups positions =
  List.filter (\g -> List.isEmpty <| findLiberties g player positions) groups

removeCaptures : List (List Position) -> Dict Position Player -> Dict Position Player
removeCaptures captures positions =
  removeCapturesIter (List.concat captures) positions
  
removeCapturesIter : List Position -> Dict Position Player -> Dict Position Player
removeCapturesIter captures positions =
  case captures of
    []        -> positions
    pos::rest -> removeCapturesIter rest (Dict.remove pos positions)

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

playerAt : Position -> GamePosition -> Maybe Player
playerAt = Dict.get

inGame : Player -> GamePosition -> Position -> Bool
inGame player positions pos =
    playerAt pos positions == Just player

map : (Position -> a) -> (a -> b -> b) -> b -> Int -> Int -> b
map mapf collectf empty cols rows =
  mapRowIter mapf collectf cols rows empty

mapRowIter : (Position -> a) -> (a -> b -> b) -> Int -> Int -> b -> b
mapRowIter mapf collectf cols rows acc =
  case rows of
    0 -> acc
    _ -> mapRowIter mapf collectf cols (rows-1) (mapCellIter mapf collectf cols rows acc)

mapCellIter : (Position -> a) -> (a -> b -> b) -> Int -> Int -> b -> b
mapCellIter mapf collectf cols rows acc =
  case cols of
    0 -> acc
    _ -> mapCellIter mapf collectf (cols-1) rows (collectf (mapf (cols,rows)) acc)
    
buildBoardPosition : GamePosition -> Player -> BoardPosition
buildBoardPosition gamePosition nextPlayer =
  (0, positionCode (Just nextPlayer))::map (positionCode << flip playerAt gamePosition) sparseMerge [] 19 19

positionCode : Maybe Player -> Char
positionCode player =
  case player of
    Nothing    -> '.'
    Just Black -> 'B'
    Just White -> 'W'

sparseMerge : Char -> BoardPosition -> BoardPosition
sparseMerge code boardPosition =
  case boardPosition of
    []                  -> [(1,code)]
    (count,bpcode)::bps -> if bpcode == code then
                             (count + 1, code)::bps
                           else
                             (1,code)::boardPosition

-- VIEW

view : Game -> Html Msg
view game =
    div [] [list_positions game, buildBoard game, navbar game]

list_positions : Game -> Html Msg
list_positions game =
    div [] [ text (toString game) ]

navbar : Game -> Html Msg
navbar game =
  div [] [nav_save game, nav_begin game, nav_rew game, nav_back game, nav_up game, nav_down game, nav_step game, nav_ff game, nav_end game]

nav_save : Game -> Html Msg
nav_save game =
  div [ onClick (Menu Save) ] [ text "save" ]

nav_begin : Game -> Html Msg
nav_begin game =
  div [ ] [ text "begin" ]

nav_rew : Game -> Html Msg
nav_rew game =
  div [ ] [ text "rew" ]
  
nav_back : Game -> Html Msg
nav_back game =
  div [ ] [ text "back" ]

nav_up : Game -> Html Msg
nav_up game =
  div [ ] [ text "up" ]

nav_down : Game -> Html Msg
nav_down game =
  div [ ] [ text "down" ]

nav_step : Game -> Html Msg
nav_step game =
  div [ ] [ text "step" ]

nav_ff : Game -> Html Msg
nav_ff game =
  div [ ] [ text "ff" ]

nav_end : Game -> Html Msg
nav_end game =
  div [ ] [ text "end" ]
    
buildBoard : Game -> Html Msg
buildBoard game =
  div [ board, onMouseLeave (Hover Nothing) ] (map (flip buildCell game) (::) [] 19 19)
    
--     div [ board ] (build_labels cols rows ++ build_board_cells cols rows game)
--     div [ board ] (buildSvg cols rows::build_board_cells cols rows game)

-- build_labels : Int -> Int -> Html Msg
-- build_labels cols rows =

buildCell : Position -> Game -> Html Msg
buildCell pos game =
  div (cell::actions pos game.positions) [ drawCell pos game ]


actions : Position -> GamePosition -> List (Attribute Msg)
actions pos positions =
  if positionEmpty pos positions then
    [ onClick (Move pos), onMouseEnter (Hover (Just pos)) ]
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
  case playerAt pos game.positions of
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

-}
