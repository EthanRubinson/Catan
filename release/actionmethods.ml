open Util
open Constant
open Definition


(**let take_half (b,w,o,l,g)  = 
	let num = (sum_cost (b,w,o,l,g) )/2 in 
	let rec take_half_helper (b1,w1,o1,l1,g1) counter=
		if (sum_cost base) = num then base 
		else if (
			let modnum = counter mod 5 in
			match modnum with
			|0 -> if (b1 < b) then take_half_helper (b1 +1 ,w1,o1,l1,g1) counter+1 else take_half_helper (b1,w1,o1,l1,g1) counter+1
			|1 -> if (w1 < w) then take_half_helper (b1 ,w1 +1,o1,l1,g1) counter+1 else take_half_helper (b1,w1,o1,l1,g1) counter+1
			|2 -> if (o1 < o) then take_half_helper (b1,w1,o1+1,l1,g1) counter+1 else take_half_helper (b1,w1,o1,l1,g1) counter+1
			|3 ->
			|4 ->
		)



let rec robber_discard ply = 
	match ply with
	[] -> []
	|(color,(i,cards),trophies)::t -> 
		(
			if (sum_cost i > cMAX_HAND_SIZE) then (c,(take_half i,cards),trophies)
			else (c,(i,cards),trophies)::(robber_discard t)
		)**)



let rec find_resource hex i counter dr  (b1,w1,o1,l1,g1) set= 
	match hex with
	|[] ->  (b1,w1,o1,l1,g1)
	|(ter, roll)::t -> (if (i = counter && roll = dr) then 
							(
								match (resource_of_terrain ter) with
								|Some x1 -> 
									(map_cost2 (fun x y -> (x*(settlement_num_resources set)) + y) (single_resource_cost x1) (b1,w1,o1,l1,g1))
								|None ->  (b1,w1,o1,l1,g1)
							) else find_resource t i (counter+1) dr (b1,w1,o1,l1,g1) set)

let rec update_resources_color col i inters  (b1,w1,o1,l1,g1) dr hex= 
	match inters with
	[] -> (b1,w1,o1,l1,g1) 
	|None::t ->  update_resources_color col (i+1) t  (b1,w1,o1,l1,g1) dr hex
	|Some(c,set)::t -> if (col = c) then find_resource hex i 0 dr (b1,w1,o1,l1,g1) set  else update_resources_color col (i+1) t  (b1,w1,o1,l1,g1) dr hex


let rec update_resources_playerlist diceroll playList intersectionList hex=
	match playList with
	|[] -> []
	| (c,(inv,cards), trophies)::t -> (c,(update_resources_color c 0 intersectionList inv diceroll hex,cards), trophies)::(update_resources_playerlist diceroll t intersectionList hex)
