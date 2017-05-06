import Html exposing (Html, Attribute, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

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

type Msg = Menu | Move Position

update : Msg -> Game -> Game
update msg game =
  case msg of
    Move pos -> pos::game
    Menu      -> game


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

build_board_cells : Int -> Int -> Game -> List(Html Msg)
build_board_cells cols rows game =
    case rows of
        0 -> []
        _ -> (build_row_cells cols rows game) ++ build_board_cells cols (rows - 1) game

build_row_cells : Int -> Int -> Game -> List(Html Msg)
build_row_cells cols row game =
    case cols of
        0 -> []
        _ -> build_cell cols row game::build_row_cells (cols - 1) row game

build_cell : Int -> Int -> Game -> Html Msg
build_cell col row game =
    let
        move = Move(20-col,20-row)
        odd_even_style col row =
            case (row * 19 + col - 1) % 2 of
                0 -> even
                _ -> odd
    in
        div [ (onClick move), point, odd_even_style col row ] [ text (toString (20-col,20-row)) ]


-- STYLES

board : Attribute msg
board =
  Html.Attributes.style
  [ ("display", "-webkit-flex")
  , ("display", "flex")
  , ("width", "855px")
  , ("height", "855px")
  , ("background-color", "lightgrey")
  , ("flex-wrap", "wrap")
  , ("align-content", "flex-start")
  ]

point : Attribute msg
point =
    Html.Attributes.style [ ("width", "45px")
                          , ("height", "45px")
                          , ("margin", "0px")
                          ]

odd : Attribute msg
odd =
    Html.Attributes.style [ ("background-color", "cornflowerblue") ]

even : Attribute msg
even =
    Html.Attributes.style [ ("background-color", "grey") ]

-- div []
--     [ button [ onClick (Board(1,1)) ] [ text "+" ]
--     , button [ onClick (Board(1,2)) ] [ text "+" ]
--     , button [ onClick (Board(2,1)) ] [ text "+" ]
--     , button [ onClick (Board(2,2)) ] [ text "+" ]
--    ]
