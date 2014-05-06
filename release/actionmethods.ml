open Util
open Constant
open Definition
open Handlemethods


let take_half (b,w,o,l,g)  = print_endline "begin take half";
	let num = ((sum_cost (b,w,o,l,g) )/2) in 
	let rec take_half_helper (b1,w1,o1,l1,g1) counter= print_endline "happening";
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
let rec check_player_lst_discard playerList = 
	match playerList with
	|[] -> None
	|(c,(inv,crd),trop)::t -> if (sum_cost(inv)) > cMAX_HAND_SIZE then Some(c) else check_player_lst_discard t

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




let  find_resource hex i counter dr  (b1,w1,o1,l1,g1) set= 
	let ad_piece = adjacent_pieces i in
	let rec find_res_help adlist (b1,w1,o1,l1,g1) = 
	match adlist with
	[] -> (b1,w1,o1,l1,g1)
	|h::t -> 
	let (ter,roll) = getIndexOf hex h in
	if (roll = dr) then 
							(
								match (resource_of_terrain ter) with
								|Some x1 -> 
									find_res_help t (map_cost2 (fun x y -> (x*(settlement_num_resources set)) + y) (single_resource_cost x1) (b1,w1,o1,l1,g1))
								|None ->  find_res_help t (b1,w1,o1,l1,g1)
							)
					else  find_res_help t (b1,w1,o1,l1,g1) in
		find_res_help ad_piece (b1,w1,o1,l1,g1) 

let rec update_resources_color col i inters  (b1,w1,o1,l1,g1) dr hex= 
	match inters with
	[] -> (b1,w1,o1,l1,g1) 
	|None::t ->  update_resources_color col (i+1) t  (b1,w1,o1,l1,g1) dr hex
	|Some(c,set)::t -> if (col = c) then update_resources_color col  (i+1) t (find_resource hex i 0 dr (b1,w1,o1,l1,g1) set) dr hex  else update_resources_color col (i+1) t  (b1,w1,o1,l1,g1) dr hex


let rec update_resources_playerlist diceroll playList intersectionList hex=
	match playList with
	|[] -> []
	| (c,(inv,cards), trophies)::t -> (c,(update_resources_color c 0 intersectionList inv diceroll hex,cards), trophies)::(update_resources_playerlist diceroll t intersectionList hex)

let update_turn tn next = 
	let (next_tn, request) = next in
	 {active= next_tn; 
    dicerolled= None; cardplayed= false; 
    cardsbought= Reveal[]; 
    tradesmade= 0; pendingtrade= None}

let check_road_connects c rdList p1 p2 = 
	let rec check_helper rdList =
	match rdList with
	|[] -> false
	|(color,(po1,po2))::t -> if (c = color && (po1 = p1 || po1 = p2 || po2 = p1 || po2 = p2)) then true else check_helper t in 
	check_helper rdList

(**handles build returns **)
let build_method b  (interList, rdList) = 
	match b with
	|BuildRoad(c,(p1,p2)) -> print_endline "building road";  if (valid_road_position rdList p1 p2 && check_road_connects c rdList p1 p2) then ((print_endline "valid road"); (interList, (c,(p1,p2))::rdList)) else ((print_endline "invalid road"); (interList,rdList)) 
	|BuildTown(t) -> (interList,rdList)
	|BuildCity(p) -> (interList,rdList)
	|BuildCard ->  (interList,rdList)

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

