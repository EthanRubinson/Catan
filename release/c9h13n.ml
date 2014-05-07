open Definition
open Registry
open Constant
open Util
open Print


type weight = (float * float * float * float * float)

(** Give your bot a 2-20 character name. *)
let name = "c9h13n"

(* Debug mode enabled? *)
let debug_mode_is_active = true


(* Prints debug statements if debug_mode_is_active is set to true *)
let debug s = if debug_mode_is_active then (print_endline ("[ " ^ name ^ " ] --> " ^ s)) else ()

(* Index represents the probability of rolling that number given two 6 sides die*)
let roll_probs = [0.0; 0.0; 2.78; 5.56; 8.33; 11.11; 13.89; 16.67; 13.89; 11.11; 8.33; 5.56; 2.78]

(* = a list with duplicate elements removed*)
let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []
(**return true if ith element is None, false otherwise**)

let valid_town_spot lst p1 = 
  let adpoints = adjacent_points p1 in
  let rec ithhelper lst i p1= 
  match lst with
  [] -> false
  |h::t -> (if (i = p1) then
          (match h with
          |None -> true
          |_-> false) else (ithhelper t (i+1) p1) )in
  let rec checkadpoints pointlist = 
  match pointlist with
  |[] -> true
  |h::t -> if (ithhelper lst 0 h) then checkadpoints t else false in
 ((ithhelper lst 0 p1) && (checkadpoints adpoints ))


(* Outputs a string representation of a weighted cost "(b., w., o., g., l.)" *)
let string_of_weighted_cost tup = 
  	let (b,w,o,g,l) = tup in
  	let bs = string_of_float b in
  	let ws = string_of_float w in
  	let os = string_of_float o in
    let gs = string_of_float g in
  	let ls = string_of_float l in
  	"( " ^ bs ^ ", " ^ ws ^ ", " ^ os ^ ", " ^ gs ^ ", " ^ ls ^ " )"

let map_cost_weight f c1 c2 : weight =
  let (b1,w1,o1,l1,g1) = c1 in
  let (b2,w2,o2,l2,g2) = c2 in
  (f (float_of_int b1) b2, f (float_of_int w1) w2, f (float_of_int o1) o2, f (float_of_int l1) l2, f (float_of_int g1) g2)

 let get_inventory_for_player (pcolor:color) (player_list : player list) : cost = 
 	let player = List.nth player_list (list_indexof (fun elem -> let (c,_,_) = elem in if c == pcolor then true else false) player_list) in
 	let (_,(i,c),_) = player in
 	i

let get_hand_for_player (pcolor:color) (player_list : player list) : cards = 
  let player = List.nth player_list (list_indexof (fun elem -> let (c,_,_) = elem in if c == pcolor then true else false) player_list) in
  let (_,(i,c),_) = player in
  c

let get_num_cards_for_player (pcolor:color) (player_list : player list) : int = 
  let player = List.nth player_list (list_indexof (fun elem -> let (c,_,_) = elem in if c == pcolor then true else false) player_list) in
  let (_,(i,c),_) = player in
  match c with
  | Hidden i -> i
  | Reveal r -> List.length r

let get_trohpies_for_player (pcolor:color) (player_list : player list) : (bool*bool) = 
  let player = List.nth player_list (list_indexof (fun elem -> let (c,_,_) = elem in if c == pcolor then true else false) player_list) in
  let (_,_,(_,lr,la)) = player in
  (lr,la)


module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  
  let road_importance = ref (-1)
  let town_importance = ref (-1)
  let city_importance = ref (-1)
  let card_importance = ref (-1)
  let ideal_weight = ref (-1.,-1.,-1.,-1.,-1.) (*B W O G L*)

  let calc_ideal_resource_weight () : weight = 
  	let road_weighted_cost = map_cost (fun r -> !road_importance * r) cCOST_ROAD in
  	let town_weighted_cost = map_cost (fun r -> !town_importance * r) cCOST_TOWN in
  	let city_weighted_cost = map_cost (fun r -> !city_importance * r) cCOST_CITY in
  	let card_weighted_cost = map_cost (fun r -> !card_importance * r) cCOST_CARD in
  	let s1 = map_cost2 (fun x y -> x + y) road_weighted_cost town_weighted_cost in
  	let s2 = map_cost2 (fun x y -> x + y) s1 city_weighted_cost in
  	let s3 = map_cost2 (fun x y -> x + y) s2 card_weighted_cost in
  	let (b,w,o,g,l) = s3 in
  	let min_val = float_of_int (min (min (min (min b w) o) g) l) in
  	let normalized_weights = map_cost (fun r -> (float_of_int r) /. min_val) s3 in
  	normalized_weights

  let calc_utility cur_res : float= 
  	let (bu,wu,ou,gu,lu) = map_cost_weight (fun cr w -> if (cr < 0.0) then (-1000000000.0) else (sqrt cr) *. w) cur_res !ideal_weight in
  	(*(min (min (min (min bu wu) ou) gu) lu)*)
    bu +. wu +. ou +. gu +. lu

  let calc_vp_for_player (pcolor:color) (plist:player list) (ilist: intersection list) : int = 
    let trophies = get_trohpies_for_player pcolor plist in
    let trophy_vps = (if fst trophies then cVP_LONGEST_ROAD else 0) + (if snd trophies then cVP_LARGEST_ARMY else 0) in
    let town_vps = cVP_TOWN * (list_count (fun elem -> match elem with |None -> false | Some ((c,s)) -> if c == pcolor && s == Town then true else false ) ilist) in
    let city_vps = cVP_CITY * (list_count (fun elem -> match elem with |None -> false | Some ((c,s)) -> if c == pcolor && s == Town then true else false ) ilist) in
    trophy_vps + town_vps + city_vps

  (* Returns a list ordered from highest to lowest of player rank*)
  let rank_other_players self plist ilist : color list =
    let other_pl = list_memremove (fun x -> if x == self then true else false) [Blue;Red;Orange;White] in
    let tmp = List.map (fun elem -> (calc_vp_for_player elem plist ilist, sum_cost (get_inventory_for_player elem plist), get_num_cards_for_player elem plist, elem)) other_pl in
    let tmp2 = List.sort (fun e1 e2 -> compare e1 e2 * -1) tmp in
    List.map (fun elem -> let (_,_,_,c) = elem in c) tmp2


  let num_towns_on_hex_for_player (hex_point:piece) (ilist: intersection list) (pcolor:color) :int =
    let adj_settles = piece_corners hex_point in
    let count = ref 0 in
    (List.iter (fun pnt -> match (List.nth ilist pnt) with |Some ((c,s)) -> if c == pcolor && s == Town then (count := !count + 1) else () | None -> ()) adj_settles);
    !count

  let total_towns_for_player (ilist: intersection list) (pcolor:color) :int =
    let count = ref 0 in
    (List.iter (fun elem -> match elem with |Some ((c,s)) -> if c == pcolor && s == Town then (count := !count + 1) else () | None -> ()) ilist);
    !count

  let total_cities_for_player (ilist: intersection list) (pcolor:color) :int =
    let count = ref 0 in
    (List.iter (fun elem -> match elem with |Some ((c,s)) -> if c == pcolor && s == City then (count := !count + 1) else () | None -> ()) ilist);
    !count

  let num_cities_on_hex_for_player (hex_point:piece) (ilist: intersection list) (pcolor:color) : int =
    let adj_settles = piece_corners hex_point in
    let count = ref 0 in
    (List.iter (fun pnt -> match (List.nth ilist pnt) with |Some ((c,s)) -> if c == pcolor && s == City then (count := !count + 1) else () | None -> ()) adj_settles);
    !count

  let robber_weight_on_hex (hl:hex list) (hex_point:piece) (ilist:intersection list) (self:color) (opp:color) :float =
    if fst(List.nth hl hex_point) == Desert then 0.0 else 
    let num_self_towns = num_towns_on_hex_for_player hex_point ilist self in
    let num_opp_towns = num_towns_on_hex_for_player hex_point ilist opp in
    let num_self_city = num_cities_on_hex_for_player hex_point ilist self in
    let num_opp_city = num_cities_on_hex_for_player hex_point ilist opp in
    (print_endline (string_of_int hex_point));
    let hex_prob = List.nth roll_probs (snd(List.nth hl hex_point)) in
    hex_prob *. (float_of_int(cRESOURCES_GENERATED_TOWN * (num_opp_towns - num_self_towns) + cRESOURCES_GENERATED_CITY * (num_opp_city - num_self_city)))


  let calc_robber_placement_effect_list_for_opp (hl:hex list) (ilist: intersection list) (self:color) (opp:color) : (float*piece *color) list = 
    let opp_touched_hexes = ref [] in
    let rec find_hexes index il = match il with
      | [] -> ()
      | h::t -> begin
        match h with
          |Some ((c,_)) -> (if c == opp then (opp_touched_hexes :=  (adjacent_pieces index) @ !opp_touched_hexes)); find_hexes (index + 1) t
          |None -> find_hexes (index + 1) t

      end
    in
    (find_hexes 0 ilist);
    let unique_hexes = uniq !opp_touched_hexes in
    let weighted_hexes = List.map (fun hex -> ((robber_weight_on_hex hl hex ilist self opp), hex, opp) ) unique_hexes in
    List.sort (fun e1 e2 -> compare e1 e2 * -1) weighted_hexes

                                                    
  let calc_robber_placement_ranked_options (b:board) (self:color) (plist: player list) : (float * piece * color) list = 
    let (m,s,_,_,r) = b in
    let (hl,_) = m in
    let (ilist,_) = s in
    let p_ranks = rank_other_players self plist ilist in
    let possible_moves = ref [] in
    (List.iter (fun p -> (possible_moves := (calc_robber_placement_effect_list_for_opp hl ilist self p) @ !possible_moves);) p_ranks);
    List.filter (fun elem -> let (w,pec,_) = elem in (if w < 0.0 || pec == r then false else true)) !possible_moves



  let calc_initial_hexes_weight hl adj_hexes =
  let rec do_calc acc = function
    |[] -> acc
    | h::t -> begin
      let (terr,roll) = (List.nth hl h) in
       if terr == Desert then do_calc (acc+.0.0) t else do_calc (acc +. (List.nth roll_probs roll)) t
    end
  in
  do_calc 0.0 adj_hexes

  let calc_free_initial_point_weights (b:board) = 
    let (m,s,_,_,_) = b in
    let (hl,_) = m in
    let (ilist,_) = s in
    let index = ref (-1) in
    List.map (fun elem -> (index:= !index +1); match elem with | None -> (print_endline (string_of_float(calc_initial_hexes_weight hl (adjacent_pieces !index)))); (calc_initial_hexes_weight hl (adjacent_pieces !index),!index) | Some _ -> (print_endline "-1"); (-1.0,!index)) ilist


  let all_valid_adj_building_areas_for_point (b:board) (p:point) = 
    let (m,s,_,_,_) = b in
    let (ilist,_) = s in
    let adjpoints = (adjacent_points p) in
    let validpointmap = List.map (fun elem -> ((valid_town_spot ilist elem), elem)) adjpoints in
    List.map (fun elem -> let (_,s) = elem in s) (List.filter (fun elem -> let (v,_) = elem in v) validpointmap)

(** true if road is valid, false otherwise **)
let valid_road_position rdList p1 p2 = 
    let rec roadhelper rdList p1 p2 = 
    match rdList with
    [] -> true
    |(c,(p3,p4))::t -> 
    (
      if ((p3 = p1 && p4 = p2) || (p3 = p2 && p4 = p1)) then false else roadhelper t p1 p2
    )
  in roadhelper rdList p1 p2

(**pick random valid road**)
let valid_roads_for_point_and_color p rdList c=
  let adpoints = adjacent_points p in
  let rec valid_list pointlist acc = 
      match pointlist with
      |[] -> acc
      |h::t -> if (valid_road_position rdList p h = false) then valid_list t acc else valid_list t (h::acc)
  in
  valid_list adpoints []
    

  let weighted_valid_next_town_spot_list_for_point (b:board) (p:point) : (point * point) list =
    let (m,s,_,_,_) = b in
    let (hl,_) = m in
    let (ilist,rlist) = s in
    let valid_next_build_locs = all_valid_adj_building_areas_for_point b p in
    let weighted_next_town_spot_list = List.sort (fun e f -> if e > f then -1 else if e < f then 1 else 0) (List.map (fun elem -> calc_initial_hexes_weight hl (adjacent_pieces elem),elem) valid_next_build_locs) in
    let needed_roads = List.map (fun elem -> let (_,o) = elem in (p,o)) weighted_next_town_spot_list in
    (List.filter (fun elem -> valid_road_position rlist (fst elem) (snd elem)) needed_roads)


  let get_best_valid_buildable_point_for_player hl ilist rlist pcolor = 
    let road_endpoints = List.fold_left (fun acc elem -> let (_,(p1,p2)) = elem in p1 :: p2 :: acc) [] (List.filter (fun elem -> let (c,(p1,p2)) = elem in c == pcolor) rlist) in
    let valid_build_point_list = List.filter (fun pnt -> valid_town_spot ilist pnt) road_endpoints in
    let weighted_build_point_list = List.sort (fun e f -> if e > f then -1 else if e < f then 1 else 0) (List.map (fun elem -> calc_initial_hexes_weight hl (adjacent_pieces elem),elem) valid_build_point_list) in
    match weighted_build_point_list with
     | [] -> None
     | h::t -> let (_,o) = h in Some o



  let initialize () = (
  	(road_importance := 3);
  	(town_importance := 4);
  	(city_importance := 2);
  	(card_importance := 1);

  	(ideal_weight := calc_ideal_resource_weight() );
  
  	(debug ("[INFO] Bot initialized with initial weights: " ^ (string_of_weighted_cost !ideal_weight)));
  )

  (* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,n) : state) : move =
    let ((self:color), r) = n in
    (print_endline (string_of_color self));
    match r with
      | InitialRequest -> begin 

        let weighted_points = List.sort (fun e f -> if e > f then -1 else if e < f then 1 else 0) (calc_free_initial_point_weights b) in
        let rec do_move = function
          | [] -> -1
          | (w,p)::t -> (print_endline (string_of_float w) ); let (_,s,_,_,_) = b in let (ilist,_) = s in if (valid_town_spot ilist p) && (List.length (weighted_valid_next_town_spot_list_for_point b p) > 0) then p else do_move t
        in
        let selected_initial_point = do_move weighted_points in
        (debug ("[[][]]] selected_initial_point" ^ string_of_int selected_initial_point));
        if (selected_initial_point == -1) then begin
          (debug "[ERROR] Bot found no initial point. Returning (0,0)"); InitialMove(0,0); 
        end 
        else
          begin
          let where = weighted_valid_next_town_spot_list_for_point b selected_initial_point in
          match where with
            | [] -> (debug "[ERROR] Bot found no initial road. Returning (0,0)"); InitialMove(0,0);
            | h::t -> (debug ("[ERROR] Bot found  initial move. Returning (" ^ string_of_int (fst h) ^ ", " ^ string_of_int (snd h) ^ ")") ); InitialMove(h);
        end
      end

      | RobberRequest -> begin
                          match (calc_robber_placement_ranked_options b self p) with
                          | [] -> begin (debug "[INFO] Got RobberReq, but no ideal placement was found. Defaulting to (0,None)");
                                  RobberMove(0, None) 
                                end
                          | h::t -> begin 
                                     let (_,pc,pl) = h in 
                                      (debug ("[INFO] Got RobberReq, yielding (" ^ string_of_int pc ^ ", " ^ string_of_color pl ^ ")"));
                                      RobberMove(pc,Some pl) 
                                   end
                      end
      | DiscardRequest-> begin
        (* GOAL: Discard cards as to minimize the impact on utility *)
        let cur_inv = get_inventory_for_player self  p in

        if sum_cost cur_inv <= cMAX_HAND_SIZE then begin
          (debug "[INFO] Got DiscardRequest but hand was <= cMAX_HAND_SIZE. Not discarding anything");
          DiscardMove(0,0,0,0,0) 
        end else begin

        (* For each card that need to be discarded, find the new inventory that yeilds the highest utility; *)
        (* repeat until the number of cards that need to be discarded have been reached*)
        let num_to_discard = (sum_cost cur_inv) / 2 in
        let rec discard_res num_left inv =
          if num_left == 0 then 
            inv
          else 
            let (b,w,o,g,l) = inv in
            let (b', w', o', g', l') = map_cost (fun x -> x-1) inv in
            let i1 = (b',w,o,g,l) in
            let i2 = (b,w',o,g,l) in
            let i3 = (b,w,o',g,l) in
            let i4 = (b,w,o,g',l) in
            let i5 = (b,w,o,g,l') in
            let u1 = ((calc_utility i1), i1) in
            let u2 = ((calc_utility i2), i2) in
            let u3 = ((calc_utility i3), i3) in
            let u4 = ((calc_utility i4), i4) in
            let u5 = ((calc_utility i5), i5) in
            discard_res (num_left-1) (snd (List.hd (List.sort (fun e f -> (compare e f) * (-1) ) [u1;u2;u3;u4;u5])))
        in
        let ideal_inv = discard_res num_to_discard cur_inv in
        let discarded_res = map_cost2 (fun x y -> x - y) cur_inv ideal_inv in
        (debug ("[INFO] Got DiscardRequest"));
        (debug ("-----> Our inventory before : " ^ string_of_cost cur_inv));
        (debug ("-----> Our utility before   : " ^ string_of_float (calc_utility cur_inv)));
        (debug ("-----> Current weighting    : " ^ string_of_weighted_cost !ideal_weight));
        (debug ("-----> Our inventory after  : " ^ string_of_cost ideal_inv));
        (debug ("-----> Our utility after    : " ^ string_of_float (calc_utility ideal_inv)));
        

        (debug ("[INFO] Discarded " ^ string_of_cost discarded_res));
        DiscardMove(discarded_res)
        end
      end

      | TradeRequest -> begin
      	let trader = t.active in

      	 (* Sanity Check *)
 		     match (try Some (get_some t.pendingtrade) with _ -> None ) with
    		  | None -> 	(debug "[WARN] Got trade request but no pending trade was present. Declined request"); 
    					TradeResponse(false)
    		  | Some tradeReq -> begin
    			let (_,cProp,cReq) = tradeReq in
    			let self_inv_before = get_inventory_for_player self p in
    			let self_util_before = (calc_utility self_inv_before) in
    			let self_inv_after = map_cost2 (fun x y -> x - y) (map_cost2 (fun x y -> x + y) cProp self_inv_before) cReq in
    			let self_util_after = calc_utility self_inv_after in
 				
 				  (debug ("[INFO] Got TradeRequest from " ^ string_of_color trader ^ ": Proposing " ^ string_of_cost cProp ^ " | Requesting " ^ string_of_cost cReq));
 				  (debug ("-----> Our inventory before : " ^ string_of_cost self_inv_before));
    	  	(debug ("-----> Our inventory after  : " ^ string_of_cost self_inv_after));
    		  (debug ("-----> Current weighting    : " ^ string_of_weighted_cost !ideal_weight));
    		  (debug ("-----> Utility if declined  : " ^ string_of_float self_util_before));
    		  (debug ("-----> Utility if accepted  : " ^ string_of_float self_util_after));


    			if (self_util_after <= self_util_before || (float_of_int (sum_cost cProp)) < 0.8 *. (float_of_int (sum_cost cReq))) then begin
    				(debug "[INFO] Trade declined.");
    				TradeResponse(false)
          end
    			else begin
    				(debug "[INFO] Trade accepted.");
    				TradeResponse(true)
          end
    		end
    	end

      | ActionRequest -> begin
        let (m,s,dck,_,r) = b in
        let (hl,_) = m in
        let (ilist,rlist) = s in
        let pinv = get_inventory_for_player self p in
        let hand = match get_hand_for_player self p with | Hidden _ -> [] | Reveal i -> i in
        let has_knight = (List.length (List.filter (fun elem -> elem == Knight) hand)) > 0 in
        let index = ref (-1) in
        let should_move_robber = ref false in
        (List.iter (fun elem -> (index:= !index + 1); match elem with | Some ((c,s)) -> if c == self && (List.length (List.filter (fun nn -> if nn == r then true else false) (adjacent_pieces !index)) > 0) then (should_move_robber:=true) else () | None -> ()) ilist);
        if t.cardplayed == false && !should_move_robber == true && has_knight == true then begin
          let robMove = (match (calc_robber_placement_ranked_options b self p) with
                          | [] -> begin (debug "[INFO] Want to move robber, but no ideal placement was found. Defaulting to (0,None)");
                                  (0, None) 
                                end
                          | h::t -> begin 
                                     let (_,pc,pl) = h in 
                                      (debug ("[INFO] Want to move robber, yielding (" ^ string_of_int pc ^ ", " ^ string_of_color pl ^ ")"));
                                      (pc,Some pl) 
                                   end)
                                 in


          Action(PlayCard(PlayKnight(robMove)))
        end
        else begin
          if is_none t.dicerolled then begin
            Action(RollDice) 
          end
          else begin
            let can_afford_cost inv cst = let (r1,r2,r3,r4,r5) = (map_cost2 (fun x y -> x - y) inv cst) in (r1 >= 0 && r2 >= 0 && r3 >= 0 && r4 >= 0 && r5 >= 0) in

            if total_towns_for_player ilist self < cMAX_TOWNS_PER_PLAYER then begin
              if can_afford_cost pinv cCOST_TOWN then begin
                match get_best_valid_buildable_point_for_player hl ilist rlist self with
                  | None -> (* We need roads *) begin
                    
                    if can_afford_cost pinv cCOST_ROAD then begin
                      let road_endpoints = List.fold_left (fun acc elem -> let (_,(p1,p2)) = elem in p1 :: p2 :: acc) [] (List.filter (fun elem -> let (c,(p1,p2)) = elem in c == self) rlist) in
    
                      let where = List.sort (fun e f -> (compare e f) * -1) (List.flatten (List.map (weighted_valid_next_town_spot_list_for_point b) road_endpoints)) in
                      match where with
                        | [] -> Action(EndTurn);
                        | h::t -> Action(BuyBuild(BuildRoad(self,h)));

                    end
                    else begin
                      Action(EndTurn)
                    end

                  end






                  | Some x -> Action(BuyBuild(BuildTown(x)))
              end
              else begin
              
            Action(EndTurn)
              end
            end

            else begin
              if total_cities_for_player ilist self < cMAX_CITIES_PER_PLAYER then begin
                if can_afford_cost pinv cCOST_CITY then begin
                
            Action(EndTurn)
                end
                else begin
              
            Action(EndTurn)
                end
              end
              else begin
              
            Action(EndTurn)
              end
            end


          end
        end
      end
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
