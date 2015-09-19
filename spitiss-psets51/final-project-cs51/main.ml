open Core.Std
open Printf

type color = Black | White
type position = int * int (* (row, column) from 0 through 7 =( *)
type move = position * position
type direction = Rank | File | Diagonal | None

(* Interface for pieces *)
class type piece_i =
object
  (* returns name of piece : "pawn", "knight", ... *)
  method get_name : string 

  (* returns char repping piece: 'P', 'N', 'B', 'R', 'Q', 'K' *)
  method get_char : char 

  (* returns color *)
  method get_color : color

  (* returns piece position *)
  method get_pos : position 

  (* returns value of piece (adjusted for board position, etc.) *)
  method get_value : int

  (* returns true if proposed move is legal, else false *)
  method can_move : position -> bool

  (* sets new position *)
  (* note that this does NOT check if legal, but I think it's best like this 
   * because most moves we will be doing are part of our move history or
   * saved future moves (as part of algorithm), and we've already checked
   * their legality with can_move *)
  method move : position -> unit

  (* returns list of legal moves this piece can make *)
  method legal_moves : (position * position) list

  method private pos_adj : int
end

(* Board module provides global functions *)
module type BOARD =
sig
  (* whos move it is *)
  val turn : color ref

  (* keeps track of move history, 
   * the piece_i option is Some piece if that piece was captured that move *)
  (* Note that as a list, it'll behave as a stack.. i.e., itll be backward *)
  val move_list : (move * piece_i option) list ref 
 (* list of all pieces on the board *)
  val piece_list : piece_i list ref

  val init_board : piece_i list -> unit (*sets up initial board*)
  val print_board : unit -> unit (*prints out board*)

  val evaluate : unit -> int (* evaluates current board *)
  val fold : ('a -> piece_i -> 'a) -> 'a -> 'a (* folds over all pieces *)
  
  (* retrieves piece at a given position *)
  val find_pos : position -> piece_i option

  (* list of positions, diag or straight, btw two end postions *)
  (* throws exception if inputs not on same diagonal, row, or column *)
  val path_to : position -> position -> direction * position list 
  

  val do_move_pos : position -> position -> string option
  (* function to move; returns None if successful; Some "failure" if error *)
  val do_move : string -> string option

  (* undoes last move *)
  val undo_move : unit -> unit 

  (* one-ply next move for now *)
  val next_move : unit -> move
  val next_move2: int -> move * int
 
end 

module Board : BOARD =
struct
  let turn = ref White
  let move_list : (move * piece_i option) list ref = ref []
  let piece_list : piece_i list ref = ref []
  
  (* So as to not accidentally initialize twice *)
  let initialized = ref false

  (* Folds over all pieces on the board *)
  let fold f init = List.fold_left ~f ~init !piece_list
  
  (* Returns None if board square empty, or Some piece if there's a piece there
   * Could be made more efficient by storing positions in an array,
   * But then move functions would be less efficient, and
   * I'm not entirely sure if efficient trade-off is worth it *)
  let find_pos pos = 
    let f l pc = if pc#get_pos = pos then pc :: l else l in
    match fold f [] with
    |[x] -> Some x
    |_ -> None

  (* Initializes board. Would be better with a position parser, 
   * so that one could save / load games *)
  let init_board plist = if not (!initialized) then 
    (piece_list := plist;
    initialized := true)
    else
    ()

  (* Evaluation function *)
  let evaluate () = 
    fold (fun y x -> let s = if x#get_color = White then x#get_value
				    else (-1) * x#get_value in
		    y + s) 0

  (* Board printout -- this function is largely copied from OChess *)
  let print_board () = 
    (let separator = "\n   +----+----+----+----+----+----+----+----+\n" in
    print_string separator;
    for j = 7 downto 0 do
        printf " %d |" (j + 1);
        for i = 0 to 7 do
            match find_pos (i, j) with
            | Some x -> printf " %c%c |" 
              (if x#get_color = White then ' ' else '*') 
              (x#get_char) 
            | None -> print_string "    |"
        done;
        print_string separator;
    done;
    print_string "\n      a    b    c    d    e    f    g    h\n";
    printf "\n\nEvaluate: %d\n\n" (evaluate ()))

  let path_to (p11,p12) (p21,p22) = 
    let accum = ref [] in
    let d : direction ref  = ref None in
    let x_dist = p11 - p21 in
    let y_dist = p12 - p22 in
    (match (x_dist, y_dist) with
    |(0,_) -> (* same file *) d := File;
      (match Int.compare p12 p22 with
       |(-1) -> for i = 1 to (p22 - p12) -1 do 
		accum := (p11, p12 + i) :: !accum
		done
       |_ -> for i = 1 to (p12 - p22) - 1 do 
	       accum := (p11, p22 + i) :: !accum
	     done
      )
    |(_,0) -> (* same rank *) d := Rank;
      (match Int.compare p11 p21 with
	 |(-1) -> for i = 1 to (p21 - p11) - 1 do 
		  accum := (p11 + i, p12) :: !accum
		done
	|_ -> for i = 1 to (p11 - p21) - 1 do 
	 	accum := (p21+i, p12) :: !accum
	      done
      )
    |_ -> (*diagonal? *)
      if (abs x_dist = abs y_dist) then (*yes, diagonal*)
	d := Diagonal;
	(match (Int.compare x_dist 0, Int.compare y_dist 0) with
	 |(-1,-1) -> (*up & right*)
	   for i = 1 to (p22 - p12) - 1 do 
		accum := (p11 +i, p12 + i) :: !accum
	   done
	 |(-1, 1) -> (*down & right *)
	   for i = 1 to abs (p22 - p12) - 1 do 
		accum := (p11 +i, p12 - i) :: !accum
	   done
	 |( 1,-1) -> (*up &left*)
	   for i = 1 to abs (p22 - p12) - 1 do 
		accum := (p11 -i, p12 + i) :: !accum
	   done
	 |_ -> (*down & left*)
	   for i = 1 to abs (p22 - p12) - 1  do 
		accum := (p11 -i, p12 - i) :: !accum
	   done
	)
    );
    (!d, !accum)


  let do_move_pos p1 p2 =
    match find_pos p1 with
    |None -> Some "Illegal move: there's no piece there!"
    |Some x -> 
      if not (x#get_color = !turn) then 
	Some "Wrong player!"
      else 
        let _ = (match !turn with
	|White -> turn := Black
	|Black -> turn := White) in

	(match x#can_move p2 with
	 |false -> Some "Illegal move: can't move there!"
	 |true -> (match find_pos p2 with
		   |None -> 
		     move_list := ((p1,p2),None) :: !move_list;
		     x#move p2; None
		   |Some z -> 
		     piece_list := List.filter 
				     !piece_list 
				     ~f:(fun y -> not (phys_equal z y));
		     move_list := ((p1,p2),Some z) :: !move_list;
		     x#move p2; None)
      )
		  

  let int_of_letter c = int_of_char c - int_of_char 'a'
  let int_of_num c = int_of_char c - int_of_char '0' - 1

  let do_move s =
    try 
      let p1 = int_of_letter s.[0], int_of_num s.[1] in
      let p2 = int_of_letter s.[2], int_of_num s.[3] in
      do_move_pos p1 p2
    with _ -> Some "Illegal move: bad input!"


  let undo_move () =
    match !move_list with
    |[] -> ()
    |((p1,p2), None) :: tl -> 
      let _ = (match !turn with
	       |White -> turn := Black
	       |Black -> turn := White) in
      move_list := tl;
      (match find_pos p2 with
       |Some x -> x#move p1
       |_ -> ())
    |((p1,p2), Some y) :: tl -> 
      let _ = (match !turn with
	       |White -> turn := Black
	       |Black -> turn := White) in
      move_list := tl;
      (match find_pos p2 with
       |Some x -> x#move p1; 
		  piece_list := y:: !piece_list
       |_ -> ())

  (*one ply function -- finds next move that maximizes evaluate for that player *)
  let rec maxmove (ml : move list) (maxsofar: move * int): move =
    let neg = match !turn with
      |White -> 1
      |Black -> -1
    in
    match ml with
    |[] -> fst maxsofar
    |hd :: tl -> ignore 
		   (do_move_pos (fst hd) (snd hd));
		 let tmp = (neg * (evaluate ())) in
		 if tmp > (snd maxsofar) then
		   (undo_move ();
		    maxmove tl (hd, tmp))
		 else 
		   (undo_move ();
		    maxmove tl maxsofar)

  let next_move () : move =
    let allmoves = fold (fun y x -> if x#get_color = !turn then 
				      x#legal_moves @ y
				    else y) [] in
    maxmove allmoves (((0,0),(1,1)),-1000000)

  let print_move ((p1,p2) : move) = 
    printf "%d,%d -> %d,%d\n" (fst p1) (snd p1) (fst p2) (snd p2)

  (*two ply function -- finds next move that maximizes evaluate for that player *)
  let rec maxmove2 
	    (ml : move list) (maxsofar: move * int) (plys: int): move * int =
    let neg = match !turn with
      |White -> 1
      |Black -> -1
    in
    match ml with
    |[] -> maxsofar
    |hd :: tl -> ignore (do_move_pos (fst hd) (snd hd));
		 match plys with
		 |1 -> 
		   (let tmp = (neg * (evaluate ())) in
		    if tmp > (snd maxsofar) then
		      (undo_move ();
		       maxmove2 tl (hd, tmp) 1)
		    else 
		      (undo_move ();
		       maxmove2 tl maxsofar) 1)
		 |_ ->
		   let allmoves = fold (fun y x -> if x#get_color = !turn then
						     x#legal_moves @ y
						   else y) [] 
		   in
		   let response = 
		     (*finds best move for opponent*)
		     maxmove2 allmoves (((0,0),(1,1)),-1000000) (plys - 1) 
		   in
		   (*does that move*)
		   ignore (do_move_pos (fst (fst response)) (snd (fst response)));
		   (let tmp = (neg * (evaluate ())) in
		    let _ = undo_move () in
		    if tmp > (snd maxsofar) then
		      (undo_move ();
		       maxmove2 tl (hd, tmp) plys)
		    else 
		      (undo_move ();
		       maxmove2 tl maxsofar plys))
		 
		   
  let next_move2 (plys : int) : move * int =
    let allmoves = fold (fun y x -> if x#get_color = !turn then 
				      x#legal_moves @ y
				    else y) [] in
    maxmove2 allmoves (((0,0),(1,1)),-1000000) plys	   


end

(* Basic superclass for all pieces, sets basic defaults *)
class virtual piece (init_p : position) (c : color) =
object (self)
  val mutable pos = init_p
  method virtual get_name : string
  method virtual get_char : char
  method get_color = c
  method get_pos = pos
  method virtual get_value : int
  method virtual can_move : position -> bool
  method move p = pos <- p
  method legal_moves : (position * position) list =
    let poslist : (position * position) list ref = ref [] in
    for i = 0 to 7 do
      for j = 0 to 7 do
        if self#can_move (i,j) then 
        poslist := (self#get_pos, (i,j)) :: !poslist
      done;
    done;
    !poslist

  (* positional adjustment for evaluation function *)
  method private pos_adj =
    let adj_matrix = [|
      [|  0;  0;  0;  0;  0;  0;  0;  0|];
      [|  0;  0;  0;  0;  0;  0;  0;  0|];
      [|  0;  0;  3;  5;  5;  3;  0;  0|];
      [|  0;  0;  5;  9;  9;  5;  0;  0|];
      [|  0;  0;  5;  9;  9;  5;  0;  0|];
      [|  0;  0;  3;  5;  5;  3;  0;  0|];
      [|  0;  0;  0;  0;  0;  0;  0;  0|];
      [|  0;  0;  0;  0;  0;  0;  0;  0|]
      |]
    in
    let p = self#get_pos in
    (*note that this may be technically backward, but symmetric matrix so OK *)
    adj_matrix.(fst(p)).(snd(p)) 
end

(* start individual pieces *)
  class pawn (init_p : position) (c: color) :
  object
    inherit piece_i
    method can_passant : bool
  end =
  object (self)
    inherit piece init_p c as super
    method can_passant = true
  
    method get_name = "pawn"
    method get_char = 'P'
    method get_value = 1000 + 30 * super#pos_adj
    method can_move p = 
      let path = Board.path_to self#get_pos p in
      let neg = match c with
	|White -> 1
	|Black -> -1
      in

      match fst path with
      |Diagonal -> if not (List.is_empty (snd path)) then
                     false
                   else
		   if not (neg * (snd p) > neg * (snd (self#get_pos))) then
		     false
		   else
                  (match Board.find_pos p with
                  |None -> false
                  |Some x -> not (x#get_color = c))
      |File ->
	if not (neg * (snd p) > neg * (snd (self#get_pos))) then
	  false
	else
	(match snd path with
               |[] -> (match Board.find_pos p with
                      |None -> true
                      |Some x -> false)
               |[x] -> let y = (match c with Black -> 6 | White -> 1) in
                       (snd self#get_pos = y) &&
                       (Board.find_pos x = None) &&
                       (Board.find_pos p = None)
	       |_ -> false)
      |_ -> false
  end
  
  class knight (init_p : position) (c: color) : piece_i =
  object (self)
  inherit piece init_p c as super
    method get_name = "knight"
    method get_char = 'N'
    method get_value = 3000 + 30 * super#pos_adj
    method can_move p = 
      let (p1x,p1y) = self#get_pos in
      let (p2x,p2y) = p in
        if ((p1x = p2x) || (p1y = p2y)) then false
        else
        if not ((abs (p1x - p2x)) + (abs (p1y - p2y)) = 3) then false
        else
        match Board.find_pos p with
        |None -> true
        |Some x -> not (x#get_color = c)
  end
  
  class bishop (init_p : position) (c: color) : piece_i =
  object (self)
    inherit piece init_p c as super
    method get_name = "bishop"
    method get_char = 'B'
    method get_value = 3200 + 20 * super#pos_adj
    method can_move p = 
      let path = Board.path_to self#get_pos p in
      match fst path with
      |Diagonal -> (match List.filter ~f:(fun x -> not (Board.find_pos x = None))
				     (snd path) with
		   |[] -> (match Board.find_pos p with
			 | None -> true
			 | Some x -> not (x#get_color = c)
			  )
		   |_ -> false)
      |_ -> false
  end
  
  class rook (init_p : position) (c: color) :
  object
    inherit piece_i
    method has_moved : bool
  end =
  object (self)
    inherit piece init_p c as super
    method has_moved = false (* TO DO *) 
    method get_name = "rook"
    method get_char = 'R'
    method get_value = 5000 + 10 * super#pos_adj
    method can_move p = 
      let path = Board.path_to self#get_pos p in
      match fst path with
      |File
      |Rank -> (match List.filter ~f:(fun x -> not (Board.find_pos x = None))
				     (snd path) with
		   |[] -> (match Board.find_pos p with
               | None -> true
               | Some x -> not (x#get_color = c)
              )
		   |_ -> false)
      | _  -> false
  end
  
  class queen (init_p : position) (c: color) : piece_i =
  object (self)
    inherit piece init_p c as super
    method get_name = "queen"
    method get_char = 'Q'
    method get_value = 9000 + 10 * super#pos_adj
    method can_move p = 
      let path = Board.path_to self#get_pos p in
      match fst path with
      |Diagonal
      |File
      |Rank -> (match List.filter ~f:(fun x -> not (Board.find_pos x = None))
				     (snd path) with
		   |[] -> (match Board.find_pos p with
              | None -> true
               | Some x -> not (x#get_color = c)
              )
		   |_ -> false)
      |_ -> false
  end
  
  class king (init_p : position) (c: color) : piece_i =
  object (self)
    inherit piece init_p c as super
    method get_name = "king"
    method get_char = 'K'
    method get_value = 1000000 - 5 * super#pos_adj
    method can_move p =
      let path = Board.path_to self#get_pos p in
      match fst path with
      |Diagonal 
      |File 
      |Rank -> if (List.is_empty (snd path)) then
       (match Board.find_pos p with
       | None -> true
       | Some x -> not (x#get_color = c)
       ) else false
      |_ -> false
  end
(* end individual pieces*)


let main () =
  let plist = ref [] in
    for i = 0 to 7 do
      let pw = new pawn (i,1) White in
      let pb = new pawn (i,6) Black in
      plist := 
      (pb :> piece_i) :: (pw :> piece_i) :: !plist;
    done;
    let rw1 = new rook (0,0) White in
    let rw2 = new rook (7,0) White in
    let rb1 = new rook (0,7) Black in
    let rb2 = new rook (7,7) Black in
    let nw1 = new knight (1,0) White in
    let nw2 = new knight (6,0) White in
    let nb1 = new knight (1,7) Black in
    let nb2 = new knight (6,7) Black in
    let bw1 = new bishop (2,0) White in
    let bw2 = new bishop (5,0) White in
    let bb1 = new bishop (2,7) Black in
    let bb2 = new bishop (5,7) Black in
    let qw = new queen (3,0) White in
    let qb = new queen (3,7) Black in
    let kw = new king (4,0) White in
    let kb = new king (4,7) Black in
    plist :=
      (rw1 :> piece_i) :: (rw2 :> piece_i) ::
      (rb1 :> piece_i) :: (rb2 :> piece_i) ::
      nw1 :: nw2 :: nb1 :: nb2 :: bw1 :: bw2 :: bb1 :: bb2 ::
      qw :: qb :: kw :: kb :: !plist;
    Board.init_board !plist


(* minmax algorithm
 - node: contains board position value
 - depth: how deep we're looking for
 - color: whose turn
 - returns the best node 


so....user goes
then computer:
  -  folds over all pieces
  -  looks at list of moves each piece can make
  -  pretends to move (Board.do_move) and evaluates that position 

let move_to_string (movement: move) : string = 
  let int_to_letter (num: int) : string =  Char.to_string(char_of_int (96+num)) in
  let int_to_ascii (num: int) : string =  Char.to_string(char_of_int(48+num)) in
  match movement with
  | ((a,b), (c,d)) -> int_to_letter a ^ int_to_ascii b ^ int_to_letter c ^ int_to_ascii d
  
let rec eval_moves (moves: move list) : int list = 
  match moves with
  | [] -> []
  | hd :: tl -> Board.do_move (move_to_string hd); Board.evaluate() :: eval_moves tl

fold

let rec minmax (depth: int) (color: color) (moves: move): board = 
  let movelist = 
  match depth with
  | 0 -> None
  | 1 -> let best_move = List.max (eval_moves (* the move list generated *)) in
	 
 *)
