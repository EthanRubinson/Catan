open Definition
open Constant
open Util
open Print




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
			(
				if (valid_town_spot intersList p1) then 
				( (**THE INTERSECTION IS OPEN FOR A TOWN**) print_endline "Valid_Intersection";
					let newInterList = setIthEleSet intersList p1 Town tn.active in
					if (adacentpoints p1 p2 && (roadDoesExist rdList p1 p2 = false)) then
					(**ROAD IS VALID MOVE**)
					( print_endline "Valid_road";
            let newrd = (tn.active,(p1,p2))::rdList in
						(None,((((hex,port),(newInterList, newrd),dck, discd, robber),(update_resources pLst tn.active newInterList newrd p1 hex), tn, update_next tn),gi))
					)
					else (**ROAD IS NOT VALID**)
					(
              print_endline "inValid_road";
              let newrd = rdList@(random_road p1 rdList tn.active) in
						 (None,((((hex,port),(newInterList,newrd),dck, discd, robber),(update_resources pLst tn.active newInterList newrd p1 hex), tn, update_next tn),gi))
					)		
				)
				else (**THE INTERSECTION IS ALREADY TAKEN**) 
				( print_endline "invalid_intersection";
					let p1 = list_indexof (fun x -> match x with |None -> true |_-> false) intersList in
          let newInterList = setIthEleSet intersList p1 Town tn.active in
          if (adacentpoints p1 p2 && (roadDoesExist rdList p1 p2 = false)) then
          (**ROAD IS VALID MOVE**)
          (
            print_endline "Valid_road";
            let newrd = (tn.active,(p1,p2))::rdList in
            (None,((((hex,port),(newInterList, newrd),dck, discd, robber),(update_resources pLst tn.active newInterList newrd p1 hex), tn, update_next tn),gi))
          )
          else (**ROAD IS NOT VALID**)
          (
            print_endline "inValid_road";
              let newrd = rdList@(random_road p1 rdList tn.active) in
             (None,((((hex,port),(newInterList,newrd),dck, discd, robber),(update_resources pLst tn.active newInterList newrd p1 hex), tn, update_next tn),gi))
          )   
				)

			
			)
	|RobberMove(rm) -> (None, s)
	|DiscardMove(cost1) -> (None, s)
	|TradeResponse(yesno) -> (None, s)
	|Action(a) -> (None, s) 


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
  
  
