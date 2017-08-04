type board
type color
type hand
type hands

val enum_puttable : board -> color -> hands

val put : board -> hand -> color -> board

val to_board : (int array) array -> board

val eval_board : board -> color -> int

val enum_boards : board -> color -> board list  

val isfinish : board -> bool
val iswin : board -> color -> int -> int 

val opcol : color -> color

val black : color
val white : color

val remnum : board -> int

val board2pos : board -> board -> (int * int) option

val print_board : board -> unit
val print_hands : hands -> unit

val debug_board : board

val calc_end : unit -> unit

