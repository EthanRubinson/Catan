open Definition
open Constant
open Util
open Print
open Handlemethods
open Actionmethods




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
  let ((((hex,port),strctures,dck, discd, robber),pLst, tn, nxt),gi) = s in (printer(print_board ((hex,port),strctures,dck, discd, robber)));
	let (intersList, rdList) = strctures in
  let (nxtColor, req) = nxt in
	match m with
	|InitialMove(p1,p2) -> print_endline "InitialMove";
    (let p1 = (if (valid_town_spot intersList p1) then (print_endline "Valid_Intersection"; p1) 
      else (print_endline "inValid_Intersection"; random_open_town_spot intersList)) in
		
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
			
			
			
	|RobberMove(piece,colorOp) -> 
              (
                let list_rand = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18] in
                let rand_num = match (pick_random list_rand) with |Some x -> x |None -> failwith "should not happen" in
                let piece1 = if (piece > -1 && piece < 19) then piece else rand_num in
                match colorOp with 
                |None ->  (None,((((hex,port),strctures,dck, discd, piece1),pLst, update_turn tn nxt, (next_turn tn.active, ActionRequest)),gi)) 
                        
                |Some c -> if (check_color_ad c piece intersList) then (None,((((hex,port),strctures,dck, discd, piece1),remove_one pLst c, update_turn tn nxt, (next_turn tn.active, ActionRequest)),gi))  
                else (None,((((hex,port),strctures,dck, discd, piece1),pLst, update_turn tn nxt, (next_turn tn.active, ActionRequest)),gi)) 
              ) 
	|DiscardMove(cost1) -> (print_endline "discardMove"; 
    let nPlayList = discard_cost cost1 nxtColor pLst in 
    let check_discard = check_player_lst_discard nPlayList in 
    match check_discard with
    |None -> (None,((((hex,port),strctures,dck, discd, robber), nPlayList, tn, (tn.active, RobberRequest)),gi))
    |Some c -> (None,((((hex,port),strctures,dck, discd, robber), nPlayList, tn, (c, DiscardRequest)),gi)))
	|TradeResponse(yesno) -> (None, s)
	|Action(a) -> print_endline "action move";
    match a with
    |RollDice -> print_endline "dice roll";
      let dr = random_roll () in 
      if  (dr = cROBBER_ROLL) then ((print_endline "robber roll"); 
        let check_discard = check_player_lst_discard pLst in 
        match check_discard with
        |None -> (None,((((hex,port),strctures,dck, discd, robber), pLst, tn, (tn.active, RobberRequest)),gi))
        |Some c -> (None,((((hex,port),strctures,dck, discd, robber), pLst, update_dice tn dr, (c, DiscardRequest)),gi))) 
      else ((print_endline "non robber roll"); 
      (None, ((((hex,port),strctures,dck, discd, robber), 
        update_resources_playerlist dr pLst intersList hex, update_dice tn dr, (next_turn tn.active, ActionRequest)),gi)))
    |MaritimeTrade (m) -> (None, s)
    |DomesticTrade (d)-> (None, s)
    |BuyBuild (b)-> print_endline "building action";  build_method b s
    |PlayCard (pc)-> print_endline "playcard";
      (match pc with
      |PlayKnight(piece,colorOp)-> 
              (
                let list_rand = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18] in
                let rand_num = match (pick_random list_rand) with |Some x -> x |None -> failwith "should not happen" in
                let piece1 = if (piece > -1 && piece < 19) then piece else rand_num in
                match colorOp with 
                |None ->  (None,((((hex,port),strctures,dck, discd, piece1),update_trophy pLst tn.active, update_turn tn nxt, (next_turn tn.active, ActionRequest)),gi)) 
                        
                |Some c -> if (check_color_ad c piece intersList) then (None,((((hex,port),strctures,dck, discd, piece1),update_trophy (remove_one pLst c) tn.active, update_turn tn nxt, (next_turn tn.active, ActionRequest)),gi))  
                else (None,((((hex,port),strctures,dck, discd, piece1),update_trophy pLst tn.active, update_turn tn nxt, (next_turn tn.active, ActionRequest)),gi)) 
              ) 
      |PlayRoadBuilding (rd,rd1) -> 
            (
              let (c,(p1,p2)) = rd in
                if (valid_road_position rdList p1 p2 && check_road_connects c rdList p1 p2 ) then
                 (let newrd_lst = ((c,(p1,p2))::rdList) in
                match rd1 with
                |Some rd -> 
                  (
                    let (c,(p1,p2)) = rd in
                    if (valid_road_position newrd_lst p1 p2 && check_road_connects c newrd_lst p1 p2 ) then
                    (None,((((hex,port),(intersList, (c,(p1,p2))::newrd_lst),dck, discd, robber),pLst , tn, (next_turn tn.active, ActionRequest)),gi))
                    else
                    (None,((((hex,port),(intersList, newrd_lst),dck, discd, robber),pLst , tn, (next_turn tn.active, ActionRequest)),gi))
                  )
                |None -> (None,((((hex,port),(intersList, newrd_lst),dck, discd, robber),pLst , tn, (next_turn tn.active, ActionRequest)),gi))
                )
               else 
                (None,((((hex,port),(intersList,rdList),dck, discd, robber),pLst , tn, (next_turn tn.active, ActionRequest)),gi))
              )
      |PlayYearOfPlenty(res, res1) -> 
        (
          let new_pList = update_res_year_plenty res tn.active pLst in
          match res1 with
          |None ->  (None,((((hex,port),(intersList,rdList),dck, discd, robber), new_pList , tn, (next_turn tn.active, ActionRequest)),gi))
          |Some(res) -> (None,((((hex,port),(intersList,rdList),dck, discd, robber), update_res_year_plenty res tn.active new_pList , tn, (next_turn tn.active, ActionRequest)),gi))
        )
      |PlayMonopoly(res) -> 
            (None,((((hex,port),(intersList,rdList),dck, discd, robber), update_res_monoploy res tn.active pLst , tn, (next_turn tn.active, ActionRequest)),gi))
      )
    |EndTurn -> print_endline "end turn"; (None, ((((hex,port),strctures,dck, discd, robber),pLst, update_turn tn nxt, nxt),gi))


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
  
  
