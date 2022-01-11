(* TYPES *)
type player =
  | Player_one
  | Player_two

(* Surely not the best choice *)
type point = 
  | Love
  | Fifteen
  | Thirty

type points_data =
  { player_one : point
  ; player_two : point
  }

type forty_data = 
  { player : player (* The player who have forty points *)
  ; other_point : point (* Points of the other player *)
  }

(* Surely incomplete *)
type score =
  | Points of points_data
  | Forty of forty_data
  | Deuce
  | Advantage of player
  | Game of player

(* TOOLING FUNCTIONS *)

let string_of_player player =
  match player with
  | Player_one -> "Player 1"
  | Player_two -> "Player 2"


let other_player player =
  match player with
  | Player_one -> Player_two
  | Player_two -> Player_one


let string_of_point : point -> string =
 fun point ->
  match point with
  | Love -> "love"
  | Fifteen -> "15"
  | Thirty -> "30"


let string_of_score : score -> string =
 fun score ->
  match score with
  | Points points_data -> 
    string_of_point points_data.player_one
    ; string_of_point points_data.player_two

  | Forty forty_data -> 
    string_of_player forty_data.player
    ; string_of_point forty_data.other_point

  | Deuce -> 
    "deuce"

  | Advantage player -> 
    "Advantage " ^ string_of_player player 

  | Game player -> 
    string_of_player player ^ " has won."


(* An exemple how to use option to avoid null values *)
let increment_point : point -> point option =
 fun point ->
  match point with
  | Love -> Some Fifteen
  | Fifteen-> Some Thirty
  | Thirty -> None


(* An exemple how to extract values from 'a option value*)
let read_from_option_point : point option -> point =
 fun optinal_point ->
  match optinal_point with
  | Some a -> a
  | None -> Love


(* TRANSITION FUNCTIONS *)
let score_when_deuce winner = Advantage winner


let score_when_advantage advantagedPlayer winner =
  if advantagedPlayer = winner then Game winner else Deuce


let score_when_forty current_forty winner = 
  if current_forty.player = winner
    then Game winner
    else
      match increment_point current_forty.other_point with
      | None -> Deuce
      | Some p -> Forty { player = current_forty.player; other_point = p }


let score_when_game winner = Game winner

let score_when_point : points_data -> player -> score =
 fun current_point winner -> 
  if winner = Player_one
    then
      match increment_point current_point.player_one with
      | None -> Forty { player = Player_one; other_point = current_point.player_two }
      | Some point -> Points {player_one = point; player_two = current_point.player_two } 
    else match increment_point current_point.player_two with
      | None -> Forty { player = Player_two; other_point = current_point.player_one }
      | Some point -> Points { player_one = current_point.player_one; player_two = point }

let score current winner =
  match current with
  | Points p -> score_when_point p winner
  | Forty f -> score_when_forty f winner
  | Deuce -> score_when_deuce winner
  | Advantage a -> score_when_advantage a winner
  | Game g -> score_when_game g

let new_game = Points {player_one = Love; player_two = Love}