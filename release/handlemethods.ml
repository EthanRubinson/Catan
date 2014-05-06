open Util
open Constant
open Definition

(**KATE METHOD**)

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

  (**sets ith element in intersect list to settlement**)
  let setIthEleSet lst p1 set c=
    let rec ithhelper lst i =
    match lst with
    [] ->  []
    |h::t -> if (i = p1) then (Some(c,set))::(ithhelper t (i+1)) else h::(ithhelper t (i+1))  in
    ithhelper lst 0 

(**get index of from list**)
let getIndexOf lst index = 
    let rec index_helper lst p = 
    match lst with
    |[] -> failwith "out of index"
    |h::t -> if p = index then h else index_helper t (p+1) in
    index_helper lst 0



(**checks if point is adacent to another point**)
  let adacentpoints p1 p2 =
    let adList = (adjacent_points p1) in
    List.mem p2 adList

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
let random_road p rdList c=
  let adpoints = adjacent_points p in
    let rec valid_list pointlist acc= 
      match pointlist with
      |[] -> acc
      |h::t -> if (valid_road_position rdList p h = false) then valid_list t acc else valid_list t (h::acc) in
    match (pick_random (valid_list adpoints [])) with
    |None -> []
    |Some(x) -> [(c,(p,x))]

(**update resources **)

let get_resource_for_town p1 hexList=
  let pieces_list = adjacent_pieces p1 in
  let rec piece_scan pieceList (b,w,o,l,g)=
  match pieceList with
  |[] -> (b,w,o,l,g)
  |h::t -> 
    (
      let (ter,r) = getIndexOf hexList h in
      let resource = resource_of_terrain ter in
      match resource with 
      |None -> piece_scan t (b,w,o,l,g)
      |Some(x) -> 
        (
          let (b1,w1,o1,l1,g1) = single_resource_cost x in
          piece_scan t (b+(b1*cRESOURCES_GENERATED_TOWN),w+(w1*cRESOURCES_GENERATED_TOWN),o+(o1*cRESOURCES_GENERATED_TOWN),l+(l1*cRESOURCES_GENERATED_TOWN),g+(g1*cRESOURCES_GENERATED_TOWN))
        )
    ) in piece_scan pieces_list (0,0,0,0,0)

(**updates resources of second turn player**)
let update_resources playerList color intersectionList roadList p1 hexList= 
  print_endline("here");
  let rec get_player playerList = 
  match playerList with
  |[] -> failwith "issues player does not exist"
  |(c,(i,h),t)::tl -> if (c = color) then (c,(i,h),t) else get_player tl in
  let (c,(inv,cards),t) = get_player playerList in
  let count_fun inter = 
    match inter with
    |None -> false
    |Some(c,s) -> if (c = color) then true else false in
  if ((list_count count_fun intersectionList) >= 2) then 
    (
      let (b1,w1,o1,l1,g1) = get_resource_for_town p1 hexList in
      (c,((map_cost2 (+) (b1,w1,o1,l1,g1) inv),cards),t)::(list_memremove (fun (c,h,t) -> if (c=color) then true else false) playerList)

    )
  else playerList

  (**updates turn color**)

  let update_next (tn:turn) rd:next = 
    if (List.length rd > 8) then (next_turn (tn.active), ActionRequest) 
   else if (List.length rd = 4) then ((tn.active), InitialRequest) 
 else if (List.length rd = 8) then ((tn.active), ActionRequest) 
    else if (List.length rd > 4 && List.length rd < 8 ) then (prev_turn (tn.active), InitialRequest) 
    else  (next_turn (tn.active), InitialRequest)


  let update_next_turn_color (tn:turn) rd:turn = 
    let newcolor = next_turn tn.active in
    let prev_newcolor = prev_turn tn.active in
  if (List.length rd > 4 && List.length rd < 8 )  then 
  {active= prev_newcolor; 
    dicerolled= tn.dicerolled; cardplayed= tn.cardplayed; 
    cardsbought= tn.cardsbought; 
    tradesmade= tn.tradesmade; pendingtrade= tn.pendingtrade} 
  else if (List.length rd = 4 || List.length rd = 8)  then
  {active= tn.active; 
    dicerolled= tn.dicerolled; cardplayed= tn.cardplayed; 
    cardsbought= tn.cardsbought; 
    tradesmade= tn.tradesmade; pendingtrade= tn.pendingtrade} 
  else
     {active= newcolor; 
    dicerolled= tn.dicerolled; cardplayed= tn.cardplayed; 
    cardsbought= tn.cardsbought; 
    tradesmade= tn.tradesmade; pendingtrade= tn.pendingtrade}

  let update_dice tn dr = 
     {active= tn.active; 
    dicerolled= Some(dr); cardplayed= tn.cardplayed; 
    cardsbought= tn.cardsbought; 
    tradesmade= tn.tradesmade; pendingtrade= tn.pendingtrade}

(**finds next random open spot**)
let random_open_town_spot interList =
  let rec random_helper lst i = 
  match lst with
  [] -> failwith "should not be possible"
  |h::t -> if valid_town_spot interList i then i else random_helper t (i+1) in
  random_helper interList 0
