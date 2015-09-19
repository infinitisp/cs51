open Core.Std
open Util
open CrawlerServices
open Order
open Pagerank


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)

let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =

  if n = 0 then d
  else
  
  (* could find this anywhere else so helper func to build LinkSet from list *)
  let rec build_linkset (l: link list) : LinkSet.set = 
    match l with
    |[] -> LinkSet.empty
    |x::xs -> if not (LinkSet.member visited x) then 
		LinkSet.insert x (build_linkset xs)
	      else build_linkset xs
  in

  (* helper function to add url to each word key's value list in dict d *)
  let rec add_url (l: link) (wds: string list) (d: WordDict.dict) =
    match wds with
    |[] -> d
    |x :: xs -> (match WordDict.lookup d x with
		|None -> add_url l xs (WordDict.insert d x 
				      (LinkSet.insert l LinkSet.empty))
		|Some y -> add_url l xs (WordDict.insert d x
				        (LinkSet.insert l y))
		)
  in

  (* choose some link from frontier *)
  match LinkSet.choose frontier with 
  |None ->  let _ = Printf.printf "\n Frontier: %s" (LinkSet.string_of_set frontier) in 
    let _ = Printf.printf "\n Chose None!" in 
	   d (* if empty, terminate *)
  |Some (current, frontier') -> ( (* else visit that link *)
    if LinkSet.member visited current then 
      crawl n frontier' visited d
    else
    let visited' = LinkSet.insert current visited in
    match CrawlerServices.get_page current with
    |None -> crawl n frontier' visited' d 
    |Some p -> (
      let frontier'' = LinkSet.union frontier' (build_linkset p.links) in
      let d' = add_url p.url p.words d in
      crawl (n-1) frontier'' visited' d'
    )
 
  )
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
