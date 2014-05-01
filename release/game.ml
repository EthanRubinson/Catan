open Definition
open Constant
open Util
open Print


type game_info = Something | Or | Other
type game = state * game_info


let state_of_game g = fst g
let game_of_state s = (s,Something)


let init_game () = game_of_state (gen_initial_state())


let handle_move s m = failwith "Error: handle_move not implemented"

let presentation g = 
  let (board, player_list, turn, next) = state_of_game g in
  let active_player = player_list.active in

  let hide_cards_for_player =
    fun (player, (i, cards), t) -> 
  	  	if player != active_player then 
  	    	(player, (i, hide cards), t)
  		else 
  			(player, (i, cards), t)
  in

  let new_player_list = List.map hide_cards_for_player player_list in
  let new_state = (board, new_player_list, turn, next) in
  game_of_state new_state
