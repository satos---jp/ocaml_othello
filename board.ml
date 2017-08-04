open Int64

type board = int64 * int64
type color = bool
type hand = int64
type hands = int64


let print_board bo = 
	let b = ref (fst bo) in
	let w = ref (snd bo) in
		for y=1 to 8 do
			print_string "\n";
			for x=1 to 8 do
				Printf.printf (if logand one (!w) = one then "O" else if logand one (!b) = one then "X" else ".");
				w := (shift_right_logical (!w) 1);
				b := (shift_right_logical (!b) 1)
			done
		done;
		print_string "\n"

let print_hands ha = print_board (ha,ha)

let shifts = [
	(fun x -> logand (shift_right_logical x 1) (logor (of_int 0x3f7f_7f7f_7f7f_7f7f) (shift_left one 62)));
	(fun x -> logand (shift_right_logical x 9) (of_int 0x007f_7f7f_7f7f_7f7f ));
	(fun x -> (shift_right_logical x 8));
	(fun x -> logand (shift_right_logical x 7) (of_int 0x00fe_fefe_fefe_fefe));
	(fun x -> logand (shift_left x 1) (lognot (of_int 0x0101_0101_0101_0101)));
	(fun x -> logand (shift_left x 9) (lognot (of_int 0x0101_0101_0101_01ff)));
	(fun x -> (shift_left x 8));
	(fun x -> (logand (shift_left x 7) (logor (of_int 0x3f7f_7f7f_7f7f_7f00) (shift_left one 62))))]
(*
let _ = 
	let b = minus_one in
	List.iter (fun f -> 
		print_hands (f b)
	) shifts;
	print_hands (shift_left b 7);
	raise (Failure "hoge")
*)
let opcol = not

let enum_puttable bo c =
	let col = if c then fst else snd in
	let opc = if c then snd else fst in
	let mb = (col bo) in
	let ob = (opc bo) in
	let wt = logand (lognot mb) (lognot ob) in
	let res = ref zero in
	List.iter (fun f -> 
		let nb = ref (logand (f mb) ob) in (* まず、隣が白。 *) 
		print_hands (!res);
		(for i=1 to 6 do
			let tb = f (!nb) in
			res := logor (!res) (logand wt tb); (* 空白なら置ける *)
			nb := (logand tb ob) (* 同じ色ならとりあえずれんちゃん *)
		done)
	) shifts;
	!res


let isfinish bo = (logor (fst bo) (snd bo) = minus_one)


let popcount b = 
	let c = add (logand b (of_int 0x5555_5555_5555_5555)) (logand (shift_right_logical b 1) (of_int 0x5555_5555_5555_5555)) in
	let d = add (logand c (of_int 0x3333_3333_3333_3333)) (logand (shift_right_logical c 2) (of_int 0x3333_3333_3333_3333)) in
	let e = add (logand d (of_int 0x0f0f_0f0f_0f0f_0f0f)) (logand (shift_right_logical d 4) (of_int 0x0f0f_0f0f_0f0f_0f0f)) in
	let f = add (logand e (of_int 0x00ff_00ff_00ff_00ff)) (logand (shift_right_logical e 8) (of_int 0x00ff_00ff_00ff_00ff)) in
	let g = add (logand f (of_int 0x0000_ffff_0000_ffff)) (logand (shift_right_logical f 16) (of_int 0x0000_ffff_0000_ffff)) in
	let h = add (logand g (of_int 0x0000_0000_ffff_ffff)) (logand (shift_right_logical g 32) (of_int 0x0000_0000_ffff_ffff)) in
		to_int h

let remnum bo = 64 - (popcount (logor (fst bo) (snd bo)))

let eval_cnt = ref 0

let iswin bo c inf =
	eval_cnt := (!eval_cnt) + 1;
	let col = if c then fst else snd in
	let opc = if c then snd else fst in
	let myn = popcount (col bo) in
	let ton = popcount (opc bo) in
		if myn > ton then (inf/10) else if myn = ton then 0 else -(inf/10)

exception Break

let put bo x c = 
	let col = if c then fst else snd in
	let opc = if c then snd else fst in
	let rev = ref zero in
	List.iter (fun f -> 
		let nm = ref zero in
		let no = ref (f x) in
		if logand (!no) (opc bo) = zero then () else
		(try
			(for i=1 to 6 do
				if logand (!no) (opc bo) <> zero then 
					(nm := logor (!nm) (!no); no := f (!no)) 
				else (
					(if logand (!no) (col bo) <> zero then (rev := logor (!nm) (!rev)) else ()); 
					raise Break
				)
			done)
		with
			Break -> ())
	) shifts;
	let bf = logxor (fst bo) (!rev) in
	let bs = logxor (snd bo) (!rev) in
	if (col (true,false)) then
		(logor bf x,bs)
	else
		(bf,logor x bs)


let enum_boards bo c = 
	let ps = ref (enum_puttable bo c) in
	let res = ref [] in
	while (!ps) <> zero do
		let p = logand (neg (!ps)) (!ps) in
			res := ((put bo p c) :: (!res));
			ps := logxor (!ps) p
	done;
	!res

let white = true
let black = false

let to_board fb = 
	let rb = ref zero in
	let rh = ref zero in
	let b = ref one in
	for y=1 to 8 do 
		for x=1 to 8 do
			(if fb.(y).(x) = 1 then
				rh := logor (!rh) (!b)
			else if fb.(y).(x) = 2 then
				rb := logor (!rb) (!b)
			else ());
			b := shift_left (!b) 1
		done
	done;
	(!rb,!rh)

let evb = 
	 [100;-10;30;10;10;30;-10;100;
		-10;-50;-3;-3;-3;-3;-50;-10;
		 30; -3; 5; 5; 5; 5; -3; 30;
		 10; -3; 5; 1; 1; 5; -3; 10;
		 10; -3; 5; 1; 1; 5; -3; 10;
		 30; -3; 5; 5; 5; 5; -3; 30;
		-10;-50;-3;-3;-3;-3;-50;-10;
		100;-10;30;10;10;30;-10;100]




let calc_end () = 
	Printf.printf "eval %d times\n" (!eval_cnt);
	eval_cnt := 0

let eval_board bo c = 
	eval_cnt := (!eval_cnt) + 1;
	let col = if c then fst else snd in
	let opc = if c then snd else fst in
	let rec f b eb acc = 
		match eb with
		| [] -> acc
		| x :: xs -> 
			f (shift_right_logical b 1) xs ((x * (to_int (logand b one)))+acc)
	in
		(f (col bo) evb 0) - (f (opc bo) evb 0)





let nonpos bo = 
	lognot (logor (fst bo) (snd bo))



let board2pos fb tb = 
	let d = ref (logxor (nonpos fb) (nonpos tb)) in
	let res = ref None in
	if (!d) = zero then None else
		((
			for y=1 to 8 do
				for x=1 to 8 do
					(if (!d) = one then res := Some(y,x) else ());
					d := (shift_right_logical (!d) 1)
				done
			done
		);
		!res)


let debug_board = (lognot (of_int 0x77f71577153701ff),of_int 0x1031577153701ff)

(*
OOOOOOOO
OXXXXXXX
OOOXOOXX
OXOXOXXX
OOOXOOOX
OXOXOXXX
OO.X....
O..X...X
*)




