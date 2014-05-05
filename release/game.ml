open Definition
open Constant
open Util
open Print
open Handlemethods




type game_info = 
{
  
	playerNum :int;
	playerPointList : int list;
}

type game = state * game_info

let state_of_game g = fst g

let game_of_state s = 
  let (board,playerlist,turn,next) = s in

  let rec playerRecord lst accLst =
  match lst with
  |[] -> accLst
  |h::t -> playerRecord t (0::accLst)
            in

  let gameI = {playerNum = (List.length playerlist); playerPointList = (playerRecord playerlist [])} in
  (s,gameI)


let init_game () = game_of_state (gen_initial_state())


let handle_move s m =
  let ((((hex,port),strctures,dck, discd, robber),pLst, tn, nxt),gi) = s in
	let (intersList, rdList) = strctures in
	match m with
	|InitialMove(p1,p2) -> print_endline "InitialMove";
    (let p1 = (if (valid_town_spot intersList p1) then (print_endline "Valid_Intersection"; p1) 
      else (print_endline "Valid_Intersection"; list_indexof (fun x -> match x with |None -> true |_-> false) intersList )) in
		
					let newInterList = setIthEleSet intersList p1 Town tn.active  in 
					if ((adacentpoints p1 p2) && (valid_road_position rdList p1 p2 )) then
					(**ROAD IS VALID MOVE**)
					( print_endline "Valid_road";
            let newrd = (tn.active,(p1,p2))::rdList in
						(None,((((hex,port),(newInterList, newrd),dck, discd, robber),(update_resources pLst tn.active newInterList newrd p1 hex), 
          update_next_turn_color tn newrd, update_next tn newrd),gi))
					)
					else (**ROAD IS NOT VALID**)
					(
              print_endline "inValid_road";
              let newrd = rdList@(random_road p1 rdList tn.active) in
						 (None,((((hex,port),(newInterList,newrd),dck, discd, robber),(update_resources pLst tn.active newInterList newrd p1 hex), 
            update_next_turn_color tn newrd, update_next tn newrd),gi))
					))
			
			
			
	|RobberMove(rm) -> (None, s)
	|DiscardMove(cost1) -> (None, s)
	|TradeResponse(yesno) -> (None, s)
	|Action(a) -> print_endline "action move";
    match a with
    |RollDice -> print_endline "dice roll";
      let dr = random_roll () in 
      if (dr = cROBBER_ROLL) then ((None,((((hex,port),strctures,dck, discd, robber), pLst, tn, (tn.active, DiscardRequest)),gi))) 
      else 
      (None, ((((hex,port),strctures,dck, discd, robber), 
        update_resources_playerlist dr pLst intersList hex, tn, (tn.active, DiscardRequest)),gi))
    |MaritimeTrade (m) -> (None, s)
    |DomesticTrade (d)-> (None, s)
    |BuyBuild (b)-> (None, s)
    |PlayCard (pc)-> (None, s)
    |EndTurn -> (None, s)


let presentation g =  
  let (board, player_list, turn, next) = state_of_game g in
  let active_player = turn.active in
  let hide_cards_for_player : player -> player =
    function (player, (i, cards), t) -> 
  	  if player != active_player then 
  	    	(player, (i, hide cards), t)
  		else 
  			(player, (i, cards), t)
  in
  let new_player_list : player list = List.map hide_cards_for_player player_list in
  let new_state = (board, new_player_list, turn, next) in
    game_of_state new_state 
  
  
