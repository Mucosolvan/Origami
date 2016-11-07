type point = float * float;;
type kartka = point -> int;;

(* cross product of vectors from a to b and from a to x *)
let wektor a b x =
	let x0 = fst(a) and x1 = fst(b) and x2 = fst(x)
	and y0 = snd(a) and y1 = snd(b) and y2 = snd(x) in
		((x1 -. x0) *. (y2 -. y0)) -. ((x2 -. x0) *. (y1 -. y0))

(* dot product of vectors from a to b and from a to x *)	
let skalar a b x = 
	let x0 = fst(a) and x1 = fst(b) and x2 = fst(x)
	and y0 = snd(a) and y1 = snd(b) and y2 = snd(x) in
		(((x2 -. x0) *. (x1 -. x0)) +. ((y2 -. y0) *. (y1 -. y0))) /.
		(((x0 -. x1) *. (x0 -. x1)) +. ((y0 -. y1) *. (y0 -. y1))) ;;

(* perpedincular projection of point x on line ab *)
let rzut a b x = 
	let x0 = fst(a) and x1 = fst(b)
	and y0 = snd(a) and y1 = snd(b) and u = skalar a b x in
		(((x1 -. x0) *. u) +. x0, ((y1 -. y0) *. u) +. y0)

(* reflection of point x across the line ab *)
let odbicie a b x = 
	let x0 = fst(x) and x1 = fst(rzut a b x)
	and y0 = snd(x) and y1 = snd(rzut a b x) in
		((2. *. x1) -. x0, (2. *. y1) -. y0)

let kwadrat x = x *. x

(* distance between points p1 p2 *)
let odl p1 p2 = 
	let a = fst(p1) and b = fst(p2) and c = snd(p1) and d = snd(p2) in
		kwadrat(a -. b) +. kwadrat(c -. d)
			
let prostokat p1 p2 = 
	fun x -> if fst(x) >= fst(p1) && fst(x) <= fst(p2) &&
				snd(x) >= snd(p1) && snd(x) <= snd(p2) then 1
				else 0
	
let kolko p r = 
	fun x -> if (odl x p) <= kwadrat r then 1 else 0
	
let zloz p1 p2 k = 
	fun x -> if wektor p1 p2 x > 0. then k (odbicie p1 p2 x) + k x
			 else if wektor p1 p2 x = 0. then k x
			 else 0

let skladaj l k = 
	List.fold_left (fun a h -> zloz (fst(h)) (snd(h)) a) k l;;
