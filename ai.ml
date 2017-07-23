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
				(res := ((y,x),!nbo) :: !res;
				nbo := board_copy bo)
			else ()) (range 1 9)) (range 1 9);
		!res

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
	let tb = (Array.map (Array.map (fun x -> if x = 1 then 1 else if x = 2 then -1 else 0)) bo) in
	let s = arrsum tb in
	let w1 = if s > 0 then -1 else if s = 0 then 0 else 1 in
		if col = 1 then w1 else w1 * -1


let rec yomikiri bo col lastpass : (move * int) = 
	if isfinish bo then (Pass,iswin bo col) else
	let tbs = enum_boards bo col in
	match tbs with
	| [] -> 
		(Pass,
			if lastpass then iswin bo col else
			let (_,res) = yomikiri bo (opcol col) true in res * -1)
	| _ -> 
		List.fold_left (fun (rh,rw) -> fun ((nha,nhb),nb) -> 
			if rw = 1 then (rh,rw) else 
				let (_,ngw) = yomikiri nb (opcol col) false in
				let nw = ngw * -1 in
					if nw > rw then ((Mv(nha,nhb)),nw) else (rh,rw)) (Pass, -100) tbs






let eval_board bo col = 
	let cbo = [
		[100;-50;30;10;10;30;-50;100];
		[-50;-50;-3;-3;-3;-3;-50;-50];
		[ 30; -3; 5; 5; 5; 5; -3; 30];
		[ 10; -3; 5; 1; 1; 5; -3; 10];
		[ 10; -3; 5; 1; 1; 5; -3; 10];
		[ 30; -3; 5; 5; 5; 5; -3; 30];
		[-50;-50;-3;-3;-3;-3;-50;-50];
		[100;-50;30;10;10;30;-50;100]]
	
	res = ref 0
	



let rec dfs_read bo col d lastpass : (move * int) = 
	if isfinish bo then (Pass,iswin bo col) else
	if d = 0 then 
		(Pass,eval_board bo col)
	else
		let tbs = enum_boards bo col in
		match tbs with
		| [] -> 
			(Pass,
				if lastpass then iswin bo col else
				let (_,res) = yomikiri bo (opcol col) true in res * -1)
		| _ -> 
			List.fold_left (fun (rh,rw) -> fun ((nha,nhb),nb) -> 
				if rw = 1 then (rh,rw) else 
					let (_,ngw) = yomikiri nb (opcol col) false in
					let nw = ngw * -1 in
						if nw > rw then ((Mv(nha,nhb)),nw) else (rh,rw)) (Pass, -100) tbs



let debug () = 
	let nb = init_board () in
	let x = put nb (3,5) 1 true in
	print_board nb;
	raise Not_found

let ai_play bo col = 
	(* debug (); *)
	if remnum bo < 12 then let (res,_) = yomikiri bo col false in res else
	let bs = enum_boards bo col in
		match bs with
		| [] -> Pass
		| ((y,x),tbo) :: _ -> 
			(* print_board tbo; *)
			Mv (y,x)


