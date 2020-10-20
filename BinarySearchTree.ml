open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l



(* Helper function(s) *)
let rec int_common_helper t x y = 
   match t with 
  |IntLeaf -> invalid_arg "int_common"
  |IntNode (w, l, r) when (w < x && w < y) -> (int_common_helper r x y)
  |IntNode (w, l, r) when (w > x && w > y) -> (int_common_helper l x y)
  |IntNode (w, l, r) -> w 

(* Implement the functions below. *)

let rec int_size t = 
  match t with 
  |IntLeaf -> 0
  |IntNode (y,l,r) -> 1 + (int_size l) + (int_size r)

let rec int_max t = 
  match t with 
  |IntLeaf -> invalid_arg "int_max"
  |IntNode (y, l, r) -> match r with 
                        |IntLeaf -> y 
                        |IntNode(_,_,_) -> (int_max r) 

let rec int_common t x y = 
  if ((int_mem x t) && (int_mem y t)) then (int_common_helper t x y) else invalid_arg "int_common"



(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec help_pinsert compfn x t = 
    match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node (y, l, r) when (compfn x y) > 0 -> Node (y, l, (help_pinsert compfn x r))
  | Node (y, l, r) when (compfn x y) = 0 -> t
  | Node (y, l, r) -> Node (y, (help_pinsert compfn x l), r)

let rec help_mem compfn x t = 
  match t with
  | Leaf -> false
  | Node (y, l, r) when (compfn x y) > 0 -> (help_mem compfn x r)
  | Node (y, l, r) when (compfn x y) = 0 -> true
  | Node (y, l, r) -> (help_mem compfn x l)

let rec help_list node = 
    match node with 
    |Leaf -> [] 
    |Node (y, l, r) -> (help_list l) @ [y] @ (help_list r)

let pinsert x t = match t with 
                      | (compfn, tree) -> (compfn, (help_pinsert compfn x tree))
   

let rec pmem x t = match t with 
                      | (compfn, tree) -> (help_mem compfn x tree)                                                                             

let pinsert_all lst t = fold (fun a x -> pinsert x a) t lst

let rec p_as_list t = 
    match t with 
    |(_, a) -> (help_list a)

let rec pmap f t = let list = map f (p_as_list t) in 
                    match t with 
                    |(a, node) -> pinsert_all list (a , Leaf)


(***************************)
(* Part 4: Variable Lookup *)
(***************************)

let rec lookup_helper name lst = 
  match lst with 
  |[] -> -100
  |h :: t -> match h with 
            |(string, value) -> if (string = name) then value else (lookup_helper name t)


type lookup_table = (string * int) list list 

let empty_table () : lookup_table = []

let push_scope (table: lookup_table) : lookup_table = [] :: table 

let pop_scope (table: lookup_table) : lookup_table = 
    match table with 
    |[] -> failwith "No scopes remain!"
    |h :: t -> t 

let add_var name value (table: lookup_table) : lookup_table =   
    match table with 
    |[] -> failwith "There are no scopes to add a variable to!"
    |h :: t -> ((name, value) :: h) :: t
  
let rec lookup name (table: lookup_table) = 
  match table with 
  |[] -> failwith "Variable not found!"
  |h :: t -> if (lookup_helper name h) = -100 then (lookup name t) else (lookup_helper name h)


