(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)


(*reversing tuples *)
let rev_tup tup = 
    match tup with 
    |(x, y , z) -> (z, y, x)
  
(*creating absolute value function *)
let abs x = 
    if x < 0 then (-1 * x) else x 

(*getting the area of a rectangle given the coordinates of two corners *)
let area x y = 
    match (x,y) with 
    |((a,b),(c,d)) -> let product = (a-c) * (b-d) in abs(product) 

let volume x y = 
    match (x,y) with 
    |((a,b,c), (d,e,f)) -> let product = (a - d) * (b - e) * (c - f) in abs(product)
    


let equiv_frac (a, b) (x, y) = 
    if (b = 0 || y = 0) then false 
    else
        let float_ab = ((float_of_int a) /. (float_of_int b)) in
        let float_xy = ((float_of_int x) /. (float_of_int y)) in
        if float_ab = float_xy then true else false 

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

(* recursive factorial function *)
let rec factorial x = 
    match x with 
    |0 -> 1 
    |_ -> x * factorial(x - 1)

(* recursive power function *)
let rec pow x y = 
    match y with 
    |0 -> 1 
    |_ -> x * (pow x (y-1))

let rec len x = 
    match x/10 with
    |0 -> 1
    |_ -> 1 + (len (x / 10)) 

let rec tail x num = if (len x) < num then x else (x mod (pow 10 num))

let rec contains sub x = 
    if (len sub <= (len x) && x > 0) then
        let size = len sub in 
        if (tail x size) == sub then true
        else (contains sub (x/10))
    else false 

(*****************)
(* Part 3: Lists *)
(*****************)

(* getting an element given the index  *)
let rec get idx lst = 
    match (idx,lst) with 
    |(0,h :: t) -> h 
    |(a,[]) -> failwith "Out of bounds"
    |(b, h :: t) -> (get (idx - 1) t)


let rec combine lst1 lst2 = 
    match (lst1) with 
    |[] -> lst2 
    |x :: xs -> x :: (combine xs lst2)

(* reverse list  *)
let rec reverse lst = 
    match lst with 
    |[] -> [] 
    |x :: xs -> combine (reverse xs) (x::[])

(* rotate list  *)
let rec rotate shift lst = 
    match (shift, lst) with 
    |(_,[]) -> [] 
    |(0,a) -> a
    |(_, x :: xs) -> rotate (shift - 1) (combine xs (x::[])) 
    
