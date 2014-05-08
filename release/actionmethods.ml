open Util
open Constant
open Definition
open Handlemethods



let take_half (b,w,o,l,g)  =
	let num = ((sum_cost (b,w,o,l,g) )/2) in 
	let rec take_half_helper (b1,w1,o1,l1,g1) counter= 
		if ((sum_cost (b1,w1,o1,l1,g1)) = (sum_cost (b,w,o,l,g) -num)) then (b1,w1,o1,l1,g1)
		else (
			let modnum = counter mod 5 in
			match modnum with
			|0 -> if (b1 < b) then take_half_helper (b1 +1 ,w1,o1,l1,g1) (counter+1) else take_half_helper (b1,w1,o1,l1,g1) (counter+1)
			|1 -> if (w1 < w) then take_half_helper (b1 ,w1 +1,o1,l1,g1) (counter+1) else take_half_helper (b1,w1,o1,l1,g1) (counter+1)
			|2 -> if (o1 < o) then take_half_helper (b1,w1,o1+1,l1,g1) (counter+1) else take_half_helper (b1,w1,o1,l1,g1) (counter+1)
			|3 -> if (l1 < l) then take_half_helper (b1,w1,o1,l1+1,g1) (counter+1) else take_half_helper (b1,w1,o1,l1,g1) (counter+1)
			|4 -> if (g1 < g) then take_half_helper (b1,w1,o1,l1,g1+1) (counter+1) else take_half_helper (b1,w1,o1,l1,g1) (counter+1)
			|_ -> failwith "mod division -should not happen"
		) in 
		take_half_helper (0,0,0,0,0) 5

(**

let rec robber_discard ply = 
	match ply with
	[] -> []
	|(color,(i,cards),trophies)::t -> 
		(
			if (sum_cost i > cMAX_HAND_SIZE) then (c,(take_half i,cards),trophies)
			else (c,(i,cards),trophies)::(robber_discard t)
		)**)
let rec check_player_lst_discard playerList col overallcol= 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) playerList in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf playerList player_index in
			if (sum_cost(b1,w1,o1,l1,g1)) > cMAX_HAND_SIZE then Some(co) 
			else 
			(if (next_turn col = overallcol) then None
			else check_player_lst_discard playerList (next_turn col) overallcol )

let check_vals (b1,w1,o1,l1,g1) (b,w,o,l,g) = 
	let vals = map_cost2 (fun x y -> if (y>x) then false else true) (b1,w1,o1,l1,g1) (b,w,o,l,g) in
	match vals with
	|(true,true,true,true,true) -> true
	|_-> false

let discard_cost (b,w,o,l,g) col playerList= 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) playerList in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf playerList player_index in
	let (nb,nw,no,nl,ng) = (if (((sum_cost(b,w,o,l,g)) = ((sum_cost(b1,w1,o1,l1,g1))/2)) && check_vals (b1,w1,o1,l1,g1) (b,w,o,l,g)) 
		then map_cost2 (-) (b1,w1,o1,l1,g1) (b,w,o,l,g) 
else take_half (b1,w1,o1,l1,g1)) in
	(co,((nb,nw,no,nl,ng),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) playerList)




let  find_resource hex i counter dr  (b1,w1,o1,l1,g1) set robber= 
	let ad_piece = adjacent_pieces i in
	let rec find_res_help adlist (b1,w1,o1,l1,g1) = 
	match adlist with
	[] -> (b1,w1,o1,l1,g1)
	|h::t -> 
	let (ter,roll) = getIndexOf hex h in
	if (roll = dr && ((h=robber)=false)) then 
							(
								match (resource_of_terrain ter) with
								|Some x1 -> 
									find_res_help t (map_cost2 (fun x y -> (x*(settlement_num_resources set)) + y) (single_resource_cost x1) (b1,w1,o1,l1,g1))
								|None ->  find_res_help t (b1,w1,o1,l1,g1)
							)
					else  find_res_help t (b1,w1,o1,l1,g1) in
		find_res_help ad_piece (b1,w1,o1,l1,g1) 

let rec update_resources_color col i inters  (b1,w1,o1,l1,g1) dr hex robber= 
	match inters with
	[] -> (b1,w1,o1,l1,g1) 
	|None::t ->  update_resources_color col (i+1) t  (b1,w1,o1,l1,g1) dr hex robber
	|Some(c,set)::t -> if (col = c) then update_resources_color col  (i+1) t (find_resource hex i 0 dr (b1,w1,o1,l1,g1) set robber) dr hex  robber else update_resources_color col (i+1) t  (b1,w1,o1,l1,g1) dr hex robber


let rec update_resources_playerlist diceroll playList intersectionList hex robber=
	match playList with
	|[] -> []
	| (c,(inv,cards), trophies)::t -> (c,(update_resources_color c 0 intersectionList inv diceroll hex robber,cards), trophies)::(update_resources_playerlist diceroll t intersectionList hex robber)

let update_turn tn next = 
	let (next_tn, request) = next in
	 {active= next_tn; 
    dicerolled= None; cardplayed= false; 
    cardsbought= Reveal[]; 
    tradesmade= 0; pendingtrade= None}

let check_road_connects c rdList p1 p2 = 
	let rec check_helper rdList =
	match rdList with
	|[] ->  false 
	|(color,(po1,po2))::t -> if (c = color && (po1 = p1 || po1 = p2 || po2 = p1 || po2 = p2)) then ( true) else check_helper t in 
	check_helper rdList

let rec check_town_to_city_update p1 interList c =
	let index_city = getIndexOf interList p1 in 
	match index_city with
	|None -> false
	|Some(col,set) -> if  (col = c && set = Town) then true else false

let rec check_res_to_buy b inv= 
	let cost_to_build = cost_of_build b in 
	match (check_vals inv cost_to_build) with
	|true ->  true
	|false ->  false

let rec get_inv playerList col= 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) playerList in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf playerList player_index in (b1,w1,o1,l1,g1)

let rec update_resources_building b pLst col = 
	let cost_to_build = cost_of_build b in 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	(co,(map_cost2 (-)(b1,w1,o1,l1,g1) cost_to_build,crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)

let update_resources_building_card b pLst col cd = 
	let cost_to_build = cost_of_build b in 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	match crds with
	|Reveal(crds1) ->
	(co,(map_cost2 (-)(b1,w1,o1,l1,g1) cost_to_build,crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)
	|_ -> failwith "cards should not be hidden"


let check_color_ad color piece  intersection= 
	let pointList = piece_corners piece in 
	let rec check_color_helper pointList = 
	match pointList with
	|[] -> false
	|h::t -> 
		(
			let return_inter = getIndexOf intersection h in
			match return_inter with
			|None -> check_color_helper t
			|Some(c,s) -> if (c = color) then true else check_color_helper t
		)
	in check_color_helper pointList

let remove_one pLst col = 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	if (sum_cost(b1,w1,o1,l1,g1)) = 0 then (co,((b1,w1,o1,l1,g1),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)
	else
	(if (b1 > 0) then (co,((b1-1,w1,o1,l1,g1),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)
	else if (w1 >0) then (co,((b1,w1-1,o1,l1,g1),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)
	else if (o1 >0) then (co,((b1,w1,o1 -1,l1,g1),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)
	else if (l1 > 0) then (co,((b1,w1,o1,l1 -1,g1),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)
	else (co,((b1,w1,o1,l1,g1-1),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)
	)

let update_knight_by_one (k,lr,larm) = 
	(k+1, lr,larm)

let update_trophy pLst col:player list = 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	 (co,((b1,w1,o1,l1,g1),crds),update_knight_by_one trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)

let update_res_year_plenty res col pLst = 
 	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	(co,(map_cost2 (+) (single_resource_cost res )(b1,w1,o1,l1,g1),crds), trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst)

let setToZero (b1,w1,o1,l1,g1) res = 
	match res with
	|Brick -> (0,w1,o1,l1,g1)
	| Wool -> (b1,0,o1,l1,g1)
	| Ore -> (b1,w1,0,l1,g1)
	| Grain -> (b1,w1,o1,0,g1)
	| Lumber -> (b1,w1,o1,l1,0)

let addToinv (b1,w1,o1,l1,g1) res  i = 
	match res with
	|Brick -> (b1+i,w1,o1,l1,g1)
	| Wool -> (b1,w1+i,o1,l1,g1)
	| Ore -> (b1,w1,o1+i,l1,g1)
	| Grain -> (b1,w1,o1,l1+i,g1)
	| Lumber -> (b1,w1,o1,l1,g1+i)

let update_res_monoploy res col pLst = 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	let removed_color_list = (list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst) in 
	let rec remove_res lst (i,newLst) = 
	match lst with
	|[] -> (i,newLst)
	|(co,((b1,w1,o1,l1,g1), crds), trop)::t -> remove_res t (i+ num_resource_in_inventory (b1,w1,o1,l1,g1) res, (co,(setToZero (b1,w1,o1,l1,g1) res, crds), trop)::newLst) in
	let (i,lst) = (remove_res removed_color_list (0,[])) in
	(co,(addToinv (b1,w1,o1,l1,g1) res i, crds), trop)::lst 

let check_point_in_port_list port p1 res acc = 
	let rec check_port_helper lst acc= 
	match lst with
	|[] -> acc
	|((po1,po2), ratio, pres)::t ->
		if (po1 = p1 || po2 = p1) then 
			(
				match pres with
				|Any -> if (is_none acc) then check_port_helper t (Some(ratio))
						else if (ratio < get_some acc) then check_port_helper t (Some(ratio)) else
						check_port_helper t acc
				|PortResource r-> if (r = res) then
									(
										if (is_none acc) then check_port_helper t (Some(ratio))
										else if (ratio < get_some acc) then check_port_helper t (Some(ratio)) else
										check_port_helper t acc
									)
								else check_port_helper t acc
			)
		else check_port_helper t acc in check_port_helper port acc

let play_owns_port c intersList portL res = 
	let rec port_check_helper lst acc i= 
	match lst with
	|[] -> acc
	|h::t ->
		match h with
		|None -> port_check_helper t acc (i+1)
		|Some(col, s)-> (if (col =c) 
					then port_check_helper t (check_point_in_port_list portL i res acc) (i+1) else port_check_helper t acc (i+1) )
	in port_check_helper intersList None 0

let can_afford rat res inv = 
	match (map_cost2 (fun x y -> if (y>x) then false else true) inv (map_cost (fun x -> x* rat) (single_resource_cost res))) with
	|(true,true,true,true,true) -> true
	|_-> false

let update_trade_move tn d= 
	{active= tn.active; 
    dicerolled= tn.dicerolled; cardplayed= tn.cardplayed; 
    cardsbought= tn.cardsbought; 
    tradesmade= tn.tradesmade+1; pendingtrade= Some(d)}


let update_trade pLst col rat res1 res2 tn = 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	if (can_afford rat res1 (b1,w1,o1,l1,g1)) then
		((co,((map_cost2 (+) (map_cost2 (-)(b1,w1,o1,l1,g1) (map_cost (fun x -> x* rat) (single_resource_cost res1))) (single_resource_cost res2)),crds),trop)::(list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst), tn)
	else
	 (pLst, tn)

let has_cost col pLst (b,w,o,l,g)= 
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	check_vals (b1,w1,o1,l1,g1) (b,w,o,l,g)

let turn_add_trade tn trade = update_trade_move tn trade

let clear_trade tn = 
	{active= tn.active; 
    dicerolled= tn.dicerolled; cardplayed= tn.cardplayed; 
    cardsbought= tn.cardsbought; 
    tradesmade= tn.tradesmade; pendingtrade= None}

let update_aproved pLst col (col1,cost1,cost2) =
	let player_index = list_indexof (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	let newList_main_player_rem = list_memremove (fun (c,h,t) -> if (c = col) then true else false) pLst in
	let trader_index = list_indexof (fun (c,h,t) -> if (c = col1) then true else false) newList_main_player_rem in 
	let (co_t,((b1_t,w1_t,o1_t,l1_t,g1_t), crds_t), trop_t) =  getIndexOf newList_main_player_rem trader_index in
	let final_remove_list =  list_memremove (fun (c,h,t) -> if (c = col1) then true else false) newList_main_player_rem in
	(co,(map_cost2 (+) (map_cost2 (-) (b1,w1,o1,l1,g1) cost1) cost2, crds), trop):: 
	(co_t,(map_cost2 (+) (map_cost2 (-) (b1_t,w1_t,o1_t,l1_t,g1_t) cost2) cost1, crds_t), trop_t)::final_remove_list

let update_largest_army (pList:player list):player list = 
	if (list_count (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> larm = true) pList = 0)
	then 
	(
		if (list_count (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> knight >= cMIN_LARGEST_ARMY) pList > 0) then
		(
			let new_troph_index = list_indexof (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> knight >= cMIN_LARGEST_ARMY) pList in
			let (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) = getIndexOf pList new_troph_index in
			let rem_list = list_memremove (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> knight >= cMIN_LARGEST_ARMY) pList in
			(co,((b1,w1,o1,l1,g1), crds), (knight,lroad, true))::rem_list
		)
		else pList
	)
	else
		(
		let index_troph = list_indexof (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> larm) pList in
		let (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) = getIndexOf pList index_troph in
		let list_rem_troph = list_memremove (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> larm) pList in
		let index_greater = list_count 
				(fun (co,((b1,w1,o1,l1,g1), crds), (knight1,lroad, larm)) -> if (knight1 >  knight) then true else false) list_rem_troph in
				if (index_greater > 0) then
				(
					let index_of_greater = list_indexof (fun (co,((b1,w1,o1,l1,g1), crds), (knight1,lroad, larm)) -> if (knight1>  knight) then true else false) list_rem_troph in
					let (co_n,((b1_n,w1_n,o1_n,l1_n,g1_n), crds_n), (knight_n,lroad_n, larm_n)) = getIndexOf pList index_of_greater in
					let fin_lst = list_memremove (fun (co,((b1,w1,o1,l1,g1), crds), (knight1,lroad, larm)) -> if (knight1>  knight) then true else false) list_rem_troph in
					(co_n,((b1_n,w1_n,o1_n,l1_n,g1_n), crds_n), (knight_n,lroad_n, true)):: (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, false))::fin_lst
				)
				else pList
			)

let update_longest_road rd interlst pList = 
	if (list_count (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> lroad = true) pList = 0)
	then 
	(
		if (list_count (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> (longest_road co rd interlst) >= cMIN_LONGEST_ROAD) pList > 0) then
		(
			let new_troph_index = list_indexof (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> (longest_road co rd interlst) >= cMIN_LONGEST_ROAD) pList in
			let (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) = getIndexOf pList new_troph_index in
			let rem_list = list_memremove (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> (longest_road co rd interlst) >= cMIN_LONGEST_ROAD) pList in
			(co,((b1,w1,o1,l1,g1), crds), (knight,true, larm))::rem_list
		)
		else pList

	)
	else
		(
		let index_troph = list_indexof (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> lroad) pList in
		let (col,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) = getIndexOf pList index_troph in
		let list_rem_troph = list_memremove (fun (co,((b1,w1,o1,l1,g1), crds), (knight,lroad, larm)) -> lroad) pList in
		let index_greater = list_count 
				(fun (co,((b1,w1,o1,l1,g1), crds), (knight1,lroad, larm)) -> if (longest_road co rd interlst >  longest_road col rd interlst) then true else false) list_rem_troph in
				if (index_greater > 0) then
				(
					let index_of_greater = list_indexof (fun (co,((b1,w1,o1,l1,g1), crds), (knight1,lroad, larm)) -> if (longest_road co rd interlst >  longest_road col rd interlst) then true else false) list_rem_troph in
					let (co_n,((b1_n,w1_n,o1_n,l1_n,g1_n), crds_n), (knight_n,lroad_n, larm_n)) = getIndexOf pList index_of_greater in
					let fin_lst = list_memremove (fun (co,((b1,w1,o1,l1,g1), crds), (knight1,lroad, larm)) -> if (longest_road co rd interlst >  longest_road col rd interlst) then true else false) list_rem_troph in
					(co_n,((b1_n,w1_n,o1_n,l1_n,g1_n), crds_n), (knight_n,true, larm_n)):: (col,((b1,w1,o1,l1,g1), crds), (knight,false, larm))::fin_lst
				)
				else pList
			)

let check_town_connects_road rdList p1 col= 
	let rec check_town_connect_helper lst = 
	match lst with
	|[] -> false
	|(c,(po1,po2))::t -> if ((c=col) && ((po1 = p1) || (po2 =p1))) then true else check_town_connect_helper t 
in check_town_connect_helper rdList

(**check to make sure not over max**)

 let below_max_roads col rdList=
 	let rec road_count rdList= 
 		match rdList with
 		|[] -> 0
 		|(c,(p1,p2))::t -> if (c=col) then 1 + (road_count t) else (road_count t) in
 	let fin_road_c = road_count rdList in
 	if (fin_road_c < cMAX_ROADS_PER_PLAYER) then true else false

 let below_max_town col interList = 
 	let rec town_count interList = 
 	match interList with
 	|[] -> 0
 	|Some(c,s)::t -> if (col = c && s=Town) then 1 + town_count t else town_count t
 	|h::t -> town_count t
 in 
 let fin_town_c = town_count interList in
 if (fin_town_c < cMAX_TOWNS_PER_PLAYER) then true else false

let below_max_city col interList = 
 	let rec town_count interList = 
 	match interList with
 	|[] -> 0
 	|Some(c,s)::t -> if (col = c && s=City) then 1 + town_count t else town_count t
 	|h::t -> town_count t
 in 
 let fin_town_c = town_count interList in
 if (fin_town_c < cMAX_CITIES_PER_PLAYER) then true else false

 let add_card_to_turn tn card_one = 
 	{active= tn.active; 
    dicerolled= tn.dicerolled; cardplayed= tn.cardplayed; 
    cardsbought= append_card tn.cardsbought card_one; 
    tradesmade= tn.tradesmade; pendingtrade= tn.pendingtrade
    	}


(**handles build returns **)
let build_method b  state1=
	let ((((hex,port),strctures,dck, discd, robber),pLst, tn, nxt),gi) = state1 in 
	let (interList, rdList) = strctures in
	match b with
	|BuildRoad(c,(p1,p2)) ->  if (valid_road_position rdList p1 p2 && check_road_connects c rdList p1 p2 && check_res_to_buy b (get_inv pLst tn.active) && valid_road_check_inter interList p1 tn.active && valid_road_check_inter interList p2 tn.active && below_max_roads tn.active rdList) 
				then 
					(None,((((hex,port),(interList, (c,(p1,p2))::rdList),dck, discd, robber),update_longest_road ((c,(p1,p2))::rdList) interList (update_resources_building b pLst tn.active), tn, ( tn.active, ActionRequest)),gi))  
				else  (None,((((hex,port),(interList,rdList),dck, discd, robber),pLst, tn, (tn.active, ActionRequest)),gi))
	|BuildTown(t) -> if (valid_town_spot  interList t && check_res_to_buy b (get_inv pLst tn.active) && check_town_connects_road rdList t tn.active && below_max_town tn.active interList) 
				then  (None,((((hex,port),((setIthEleSet  interList t Town tn.active),rdList),dck, discd, robber),update_resources_building b pLst tn.active, tn, ( tn.active, ActionRequest)),gi))  
				else (None,((((hex,port),(interList,rdList),dck, discd, robber),pLst, tn, ( tn.active, ActionRequest)),gi))
	|BuildCity(p) -> if (check_town_to_city_update p interList tn.active && check_res_to_buy b (get_inv pLst tn.active) && below_max_city tn.active interList) 
				then  (None,((((hex,port),(setIthEleSet interList p City tn.active,rdList),dck, discd, robber),update_resources_building b pLst tn.active, tn, ( tn.active, ActionRequest)),gi))  
				else (None,((((hex,port),(interList,rdList),dck, discd, robber),pLst, tn, ( tn.active, ActionRequest)),gi))
	|BuildCard -> if (check_res_to_buy b (get_inv pLst tn.active)) 
				then (
					match dck with
					|Reveal(cardList) ->
					 (let (card_one, deck_fin) = pick_one cardList in
					(None,((((hex,port),(interList,rdList),wrap_reveal deck_fin, discd, robber),update_resources_building_card b pLst tn.active card_one, add_card_to_turn tn card_one, ( tn.active, ActionRequest)),gi)))
					|_-> failwith "cards should not be hidden"
				)
				else (None,((((hex,port),(interList,rdList),dck, discd, robber),pLst, tn, ( tn.active, ActionRequest)),gi))


let rec count_vic_card cards count=
	match cards with
	[] -> count
	|VictoryPoint::t -> count_vic_card t (count+1)
	|h::t -> count_vic_card t (count)

let tol_vic cards (knight,lroad, larm) = 
	let vic_c = count_vic_card (reveal cards) 0 in
	match (lroad,larm) with
	|(true, true) -> cVP_LARGEST_ARMY + cVP_LONGEST_ROAD + vic_c
	|(true, false) -> cVP_LONGEST_ROAD + vic_c
	|(false, true) -> cVP_LARGEST_ARMY + vic_c
	|_-> vic_c

let rec count_total_set (intersList:intersection list) count col= 
	match intersList with
	|[] -> count
	|(Some(c,s))::t -> if (c = col) then count_total_set t (count + (match s with |Town -> cVP_TOWN |City -> cVP_CITY)) col else count_total_set t count col
	| h::t ->  count_total_set t count col

let update_winner (s:'a outcome):'a outcome= 
	let (w,((((hex,port),strctures,dck, discd, robber),pLst, tn, nxt),gi)) = s in
	let (intersList, rdList) = strctures in
  	let rec update_helper lst =
  	match lst with
  	|[] -> (None,((((hex,port),strctures,dck, discd, robber),pLst, tn, nxt),gi))
  	|h::t -> 
  		(let (col,hnd,troph) = h in
  		let (inv,crds) = hnd in
  		let (knight,lroad, larm) = troph in
  		if ((tol_vic crds (knight,lroad, larm)) + (count_total_set intersList 0 col) >= 10) then  (Some col,((((hex,port),strctures,dck, discd, robber),pLst, tn, nxt),gi))
  		else update_helper t )
  	in update_helper pLst

let card_played tn = 
	{active= tn.active; 
    dicerolled= tn.dicerolled; cardplayed= true; 
    cardsbought= tn.cardsbought; 
    tradesmade= tn.tradesmade; pendingtrade= tn.pendingtrade
    }


let update_card_played pc s = 
  	let	(w,((((hex,port),strctures,dck, discd, robber),pLst, tn, nxt),gi)) = s in
  		let card_just_played = 
  			match pc with
  			|PlayKnight(_) -> Knight
  			|PlayRoadBuilding(_) -> RoadBuilding
  			|PlayYearOfPlenty(_) -> YearOfPlenty
  			|PlayMonopoly(_) -> Monopoly in
  		(w,((((hex,port),strctures,dck, card_just_played::discd, robber),pLst, card_played tn, nxt),gi))


let update_cards_end_turn pLst tn=
	let player_index = list_indexof (fun (c,h,t) -> if (c = tn.active) then true else false) pLst in
	let (co,((b1,w1,o1,l1,g1), crds), trop) =  getIndexOf pLst player_index in
	 (co,((b1,w1,o1,l1,g1), Reveal((reveal crds)@(reveal tn.cardsbought))), trop)::(list_memremove (fun (c,h,t) -> if (c = tn.active) then true else false) pLst)






