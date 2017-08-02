open Command

let print_color c = 
  if      c = 1 then print_string "O" 
  else if c = 2 then print_string "X"
  else if c = 0 then print_string " " 
  else ()

let print_board board = 
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do 
    print_int j; print_string "|";
    for i=1 to 8 do 
      print_color (board.(i).(j)); print_string " " 
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"


let dyx = List.combine 
	[-1;-1; 0; 1; 1;  1;  0; -1]
	[ 0; 1; 1; 1; 0; -1; -1; -1]


let opcol x = 3 - x

let put bo (y,x) col isdo = 
	if bo.(y).(x) <> 0 then false else 
	let opc = (opcol col) in
	if List.fold_left (
		fun nr -> fun (dy,dx) -> 
		if bo.(y+dy).(x+dx) <> opc then nr else
			let rec find_myc (ny,nx) = 
				if bo.(ny).(nx) = col then true else
				if bo.(ny).(nx) = opc then 
					let res = find_myc (ny+dy,nx+dx) in
					(if res && isdo then bo.(ny).(nx) <- col else ()); res
				else false
			in
				if find_myc (y+dy,x+dx) then true else nr)
		false dyx
	then
		((if isdo then bo.(y).(x) <- col else ()); true)
	else false

let rec range a b = if a = b then [] else a :: (range (a+1) b)

let board_copy bo = 
	let res = Array.copy bo in
	List.iter (fun i -> res.(i) <- Array.copy bo.(i)) (range 0 10);
		res

let enum_boards bo col = 
	let nbo = ref (board_copy bo) in
	let res = ref [] in
	List.iter ( fun y -> 
		List.iter ( fun x -> 
			if put (!nbo) (y,x) col true then
				(res := (Mv(y,x),!nbo) :: !res;
				nbo := board_copy bo)
			else ()) (range 1 9)) (range 1 9);
	match (!res) with
	| [] -> [(Pass,bo)]
	| _ -> !res

let init_board () = 
  let board = Array.make_matrix 10 10 0 in 
    for i=0 to 9 do 
      board.(i).(0) <- 3 ;
      board.(i).(9) <- 3 ;
      board.(0).(i) <- 3 ;
      board.(9).(i) <- 3 ;
    done;
    board.(4).(4) <- 1;
    board.(5).(5) <- 1;
    board.(4).(5) <- 2;
    board.(5).(4) <- 2;
    board 


let arrsum bo = Array.fold_left (+) 0 (Array.map (Array.fold_left (+) 0) bo)

let remnum bo = 
	let tb = (Array.map (Array.map (fun x -> if x = 0 then 1 else 0)) bo) in
		arrsum tb

let isfinish bo = (remnum bo = 0)

let iswin bo col = 
	let tb = (Array.map (Array.map (fun x -> if x = col then 1 else if x = (opcol col) then -1 else 0)) bo) in
	let s = arrsum tb in
		if s > 0 then 1000000 else if s = 0 then 0 else -1000000




let inf = 10000000

let rec yomikiri bo col lastpass : (move * int) = 
	if isfinish bo then (Pass,iswin bo col) else
	let tbs = enum_boards bo col in
	match tbs with
	| [] -> raise (Failure "yomikiri fail")
	| _ -> 
		List.fold_left (fun (rh,rw) -> fun (nmv,nb) -> 
			if rw = 1 then (rh,rw) else 
				let (_,ngw) = yomikiri nb (opcol col) false in
				let nw = ngw * -1 in
					if nw > rw then (nmv,nw) else (rh,rw)) (Pass, -inf) tbs




let cbo = Array.of_list (List.map Array.of_list [
		[100;-10;30;10;10;30;-10;100];
		[-10;-50;-3;-3;-3;-3;-50;-10];
		[ 30; -3; 5; 5; 5; 5; -3; 30];
		[ 10; -3; 5; 1; 1; 5; -3; 10];
		[ 10; -3; 5; 1; 1; 5; -3; 10];
		[ 30; -3; 5; 5; 5; 5; -3; 30];
		[-10;-50;-3;-3;-3;-3;-50;-10];
		[100;-10;30;10;10;30;-10;100]])


(* バグの原因になるので、1にとってで評価する *)

let eval_board bo = 
	let res = ref 0 in
	for y=0 to 7 do 
		for x=0 to 7 do
			let p = cbo.(y).(x) in
			let c = bo.(y+1).(x+1) in
			res := (!res + (if c = 1 then p else if c = (opcol 1) then -p else 0))
		done
	done;
	(*
	print_board bo;
	Printf.printf "col %s evalto %d\n" (if col = 2 then "black" else "white") !res;
	*)
	!res

(*
親ノードは、既にcutmax以上の値を得ている。
cutmax以上の値ならば、どうせ親が諦めるのでcutしない
*)

exception Cut_ab

(* 1はmaxを目指す
   2はminを目指す *)

let rec dfs_read bo col d lastpass cutmax cutmin : (move * int) = 
	if isfinish bo then (Pass,(iswin bo 1)) else
	if d = 0 then 
		let rw = eval_board bo in
		(* Printf.printf "remdep %d : cutmax %d cutmin %d col %d eval %d\n" d cutmax cutmin col rw; *)
		(Pass,rw)
	else
		let tbs = enum_boards bo col in
		match tbs with
		| [] -> raise (Failure "dfs_read fail")
		| _ -> 
			let rw = ref (if col = 1 then (-inf) else inf) in
			let rh = ref Pass in
			try
				List.iter (fun (nh,nb) -> 
					if col = 1 then (
						let nw = snd (dfs_read nb (opcol col) (d-1) false cutmax (!rw)) in
							if nw <= (!rw) then () else (
								rw := nw; rh := nh;
								(* Printf.printf "remdep %d : update %d %d\n" d nw !rw; *)
								if nw >= cutmax then raise Cut_ab else ()))
					else (
						let nw = snd (dfs_read nb (opcol col) (d-1) false (!rw) cutmin) in
							if nw >= (!rw) then () else (
								rw := nw; rh := nh; 
								(* Printf.printf "remdep %d : update %d %d\n" d nw !rw; *)
								if nw <= cutmin then raise Cut_ab else ()))
				) tbs;
				raise Cut_ab
			with
				Cut_ab -> 
					(* Printf.printf "remdep %d : cutmax %d cutmin %d col %d eval %d\n" d cutmax cutmin col !rw; *)
					(!rh,!rw) 


let debug_board () = 
  let board = Array.make_matrix 10 10 0 in 
    for i=0 to 9 do 
      board.(i).(0) <- 3 ;
      board.(i).(9) <- 3 ;
      board.(0).(i) <- 3 ;
      board.(9).(i) <- 3 ;
    done;
    for y=1 to 8 do 
    	for x=1 to 8 do
    		board.(y).(x) <- (if y > 4 then 1 else 2);
			done
		done;
		board.(1).(1) <- 0;
    board 


let debug () = 
	let nb = debug_board () in
	print_board nb;
	Printf.printf "white %d\n" (snd (yomikiri nb 1 false));
	Printf.printf "black %d\n" (snd (yomikiri nb 2 false));
	raise Not_found

let ai_play bo col = 
	(* debug (); *)
	print_board bo;
	if remnum bo < 6 then (* スタックオーバーフローするのやばそう *)
		let (res,t) = yomikiri bo col false in 
		print_string (if t > 0 then "win\n" else if t < 0 then "lose\n" else "even");
		res 
	else
		let (res,p) = dfs_read bo col 8 false inf (-inf) in 
		Printf.printf "point :: %d\n" p;
		res


