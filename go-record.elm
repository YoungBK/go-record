import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram
    { model = game
    , view = view
    , update = update
    }


-- MODEL

type alias Position = (Int,Int)
type alias Game = List Position

game : Game
game = []


-- UPDATE

type Msg = Menu | Board Position

update : Msg -> Game -> Game
update msg game =
  case msg of
    Board pos -> pos::game
    Menu      -> game


-- VIEW

view : Game -> Html Msg
view game =
  div [] (list_positions game::build_board 19 19 game)

list_positions : Game -> Html Msg
list_positions game =
    div [] [ text (toString game) ]

build_board : Int -> Int -> Game -> List(Html Msg)
build_board cols rows game =
    case rows of
        0 -> []
        _ -> div [] (build_row cols rows game)::build_board cols (rows - 1) game

build_row : Int -> Int -> Game -> List(Html Msg)
build_row cols row game =
    case cols of
        0 -> []
        _ -> button [ onClick (Board(20-cols,20-row)) ] [ text "+" ]::build_row (cols - 1) row game



-- div []
--     [ button [ onClick (Board(1,1)) ] [ text "+" ]
--     , button [ onClick (Board(1,2)) ] [ text "+" ]
--     , button [ onClick (Board(2,1)) ] [ text "+" ]
--     , button [ onClick (Board(2,2)) ] [ text "+" ]
--    ]
