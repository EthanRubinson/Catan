open Definition
open Constant
open Util
open Print


type playerInfo = 
{
	playerBase :player;
	move:int;
	totalpoints:int;
}

type game_info = 
{
	intersectionArr: intersection array;
	playerNum :int;
	playerArray : playerInfo array;
}

type game = state * game_info


let state_of_game g = fst g
let game_of_state s = (s,Something)


let init_game () = game_of_state (gen_initial_state())


let handle_move s m = failwith "testing"
	(**let ((mp,strctures,dck, discd, robber),pLst, tn, nxt) = s in
	let (intersList, rdList) = strctures in
	match m with
	|InitialMove(p1,p2) -> 
			(
				if (ithEleNone intersList p1) then 
				( (**THE INTERSECTION IS OPEN FOR A TOWN**)
					let newInterList = setIthEleSet intersList p1 Town tn.active in
					if (adacentpoints p1 p2 && (roadDoesExist rdList p1 p2 = false)) then
					(**ROAD IS VALID MOVE**)
					(
						(None,((mp,(newInterList,(tn.active,(p1,p2))::rdList),dck, discd, robber),pLst, tn, nxt))
					)
					else
					(
						failwith "not yet implemented"
					)		
				)
				else (**THE INTERSECTION IS ALREADY TAKEN**)
				(
					failwith "not htere yet"
				)

			
			)
	|RobberMove(rm) -> (None, s)
	|DiscardMove(cost1) -> (None, s)
	|TradeResponse(yesno) -> (None, s)
	|Action(a) -> (None, s) **)


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
  
  (**EXTRA METHODS --- KATE **)

  (**return true if ith element is None, false otherwise**)
let ithEleNone lst p1 = 
  let rec ithhelper lst i = 
  match lst with
  [] -> false
  |h::t -> if (i = p1) then
          (match h with
          |None -> true
          |_-> false) else ithhelper t (i+1) in
  ithhelper lst 0

  (**sets ith element in intersect list to settlement**)
  let setIthEleSet lst p1 set c=
    let rec ithhelper lst i acc=
    match lst with
    [] ->  acc
    |h::t -> if (i = p1) then ithhelper t (i+1) acc@[Some(c,set)] else ithhelper t (i+1) acc@t in
    ithhelper lst 0 []


(**checks if point is adacent to another point**)
  let adacentpoints p1 p2 =
    let adList = (adjacent_points p1) in
    List.mem p2 adList

(** checks if road does not exist true if does false if not**)  
  let roadDoesExist rdList p1 p2 = 
    let rec roadhelper rdList p1 p2 = 
    match rdList with
    [] -> false
    |(c,(p3,p4))::t -> 
    (
      if ((p3 = p1 && p4 = p2) || (p3 = p2 && p4 = p1)) then true else roadhelper t p1 p2
    )
  in roadhelper rdList p1 p2


  
