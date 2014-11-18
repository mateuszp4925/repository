type wartosc = (float * float) list;;

let wartosc_od_do x y =	
	if x < 0. && y > 0.
	then [(x, -0.); (0., y)]
	else if (x < 0. && y = 0.) then [(x, -0.)] else [(x, y)];;

let wartosc_dokladnosc x y = 
	let poc = x -. (y /. 100.0 *. x) in
	let kon = x +. (y /. 100.0 *. x) in
		wartosc_od_do poc kon;;		

let wartosc_dokladna x = 
	wartosc_od_do x x;;
	
let rec in_wartosc przedzial liczba =
	match przedzial with
	| [] -> false
	| (x, y)::t -> (liczba >= x && liczba <= y) || (in_wartosc t liczba);;
	
let rec min_wartosc przedzial = 
	match przedzial with
	| [] -> infinity
	| (x, y)::t -> min x (min_wartosc t);;
	
let rec max_wartosc przedzial =
	match przedzial with
	| [] -> neg_infinity
	| (x, y)::t -> max y (max_wartosc t);;
let sr_wartosc przedzial = ((min_wartosc przedzial) +. (max_wartosc przedzial)) /. 2.;;
	
let czyNaN x = not (x >= neg_infinity && x <= infinity);; 
let min a b = if czyNaN a then b else if czyNaN b then a else if a < b then a else b;;
let max a b = if czyNaN a then b else if czyNaN b then a else if a > b then a else b;;
let komp f a b c d = f (f a b) (f c d);;

let rec pom f (a, b) przedzial =
	match przedzial with
	| [] -> []
	| (x, y)::t -> (wartosc_od_do (komp min (f a x) (f a y) (f b x) (f b y)) (komp max (f a x) (f a y) (f b x) (f b y))) @ (pom f (a, b) t);;

let rec operator f przedzialA przedzialB =
	match przedzialA with
	| [] -> []
	| h::t -> (pom f h przedzialB) @ (operator f t przedzialB);;
	
let rec plus przedzialA przedzialB = operator (+.) przedzialA przedzialB;;
let rec minus przedzialA przedzialB = operator (-.) przedzialA przedzialB;;	
let rec razy przedzialA przedzialB = operator ( *.) przedzialA przedzialB;;
let rec podzielic przedzialA przedzialB = operator (/.) przedzialA przedzialB;;
