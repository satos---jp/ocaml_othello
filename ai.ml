open Command

open Board

let print_color c = 
  if      c = 1 then print_string "O" 
  else if c = 2 then print_string "X"
  else if c = 0 then print_string " " 
  else ()

let print_board_bef board = 
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


let inf = 10000000

let rec yomikiri bo c lastpass : (board * int) = 
	if isfinish bo then (bo,iswin bo c inf) else
	let tbs = enum_boards bo c in
	match tbs with
	| [] -> if lastpass then (bo,iswin bo c inf) else let (_,ngw) = yomikiri bo (opcol c) true in (bo,-ngw)
	| _ -> 
		List.fold_left (fun (rh,rw) -> fun nb -> 
			if rw = 1 then (rh,rw) else 
				let (_,ngw) = yomikiri nb (opcol c) false in
				let nw = ngw * -1 in
					if nw > rw then (nb,nw) else (rh,rw)) (bo, -inf) tbs


(*
親ノードは、既にcutmax以上の値を得ている。
cutmax以上の値ならば、どうせ親が諦めるのでcutしない
*)

exception Cut_ab

(* whiteはmaxを目指す
   blackはminを目指す *)
   
let move2str m = 
	match m with
	| Pass -> "Pass"
	| Mv(y,x) -> Printf.sprintf "Mv %c%d" (Char.chr (y+64)) x
	| GiveUp -> "Giveup"

let rec print_boards tbs = 
	List.iter (fun b -> 
		print_board b) tbs

let rec dfs_read bo c d lastpass cutmax cutmin : (board * int) = 
	if isfinish bo then (bo,iswin bo white inf) else
	if d = 0 then 
		let rw = eval_board bo white in
		(* Printf.printf "remdep %d : cutmax %d cutmin %d col %d eval %d\n" d cutmax cutmin col rw; *)
		(bo,rw)
	else
		let tbs = enum_boards bo c in
		(* Printf.printf "%d\n" d;
		print_boards tbs; *)
		match tbs with
		| [] -> if lastpass then (bo,iswin bo c inf) else let (_,ngw) = dfs_read bo (opcol c) (d-1) true cutmax cutmin in (bo,ngw)
		| _ -> 
			let rw = ref (if c = white then (-inf) else inf) in
			let rb = ref bo in
			try
				List.iter (fun nb -> 
					(* if d = 8 then Printf.printf "%s\n" (move2str nh) else (); *)
					if c = white then (
						let nw = snd (dfs_read nb (opcol c) (d-1) false cutmax (!rw)) in
							if nw <= (!rw) then () else (
								rw := nw; rb := nb;
								(* Printf.printf "remdep %d : update %d %d\n" d nw !rw; *)
								if nw >= cutmax then raise Cut_ab else ()))
					else (
						let nw = snd (dfs_read nb (opcol c) (d-1) false (!rw) cutmin) in
							if nw >= (!rw) then () else (
								rw := nw; rb := nb;
								(* Printf.printf "remdep %d : update %d %d\n" d nw !rw; *)
								if nw <= cutmin then raise Cut_ab else ()))
				) tbs;
				raise Cut_ab
			with
				Cut_ab -> 
					(* Printf.printf "remdep %d : cutmax %d cutmin %d col %d eval %d\n" d cutmax cutmin col !rw; *)
					(!rb,!rw) 




let debug () = 
	let nb = debug_board in
	print_board nb;
	(* let tbs = enum_boards nb white in *)
	print_hands (enum_puttable nb white);
	(* print_board nb;
	let (res2,p2) = dfs_read nb 2 d false inf (-inf) in 
	Printf.printf "black :: %d %s\n" p2 (move2str res2); *)
	raise Not_found

let _ = if 0 = 1 then debug () else ()

let pos2move p = 
	match p with
	| None -> Pass
	| Some(y,x) -> Mv(y,x)

let ai_play bo col = 
	print_board_bef bo;
	let c = if col = 2 then white else black in
	let bb = to_board bo in
	if remnum bb > 53 then (* 最初の3手くらいは乱択 *)
		let tbs = enum_boards bb c in
		let ls = List.length tbs in
		let res = List.nth tbs (Random.int ls) in
		pos2move (board2pos res bb)
	else if remnum bb < 10 then 
		let (res,t) = yomikiri bb c false in 
		print_string (if t > 0 then "win\n" else if t < 0 then "lose\n" else "even");
		print_string "yomikiri ";
		calc_end ();
		pos2move (board2pos res bb)
	else
		let (res,p) = dfs_read bb c 6 false inf (-inf) in 
		Printf.printf "point :: %d\n" p;
		print_board res;
		print_string "dfs_read ";
		calc_end ();
		pos2move (board2pos res bb)


