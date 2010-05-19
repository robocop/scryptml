let ($) f g = f g;;
let (|>) f g = fun x -> f (g x);;

(*
let quotient a b = (a - (a mod b)) / b;;
quotient a b is a / b
*)

let pos_mod a b = let r = a mod b in if r < 0 then r+(abs b) else r;;
let bezout a b = 
  let rec bezout' un0 vn0 un1 vn1 rn0 rn1 = 
    let rn2 = rn0 mod rn1 in
      if rn2 = 0 then (un1, vn1)
      else 
	let qn2 = rn0 / rn1 in
	let un2 = un0 - un1*qn2 in
	let vn2 = vn0 - vn1*qn2 in
	  bezout' un1 vn1 un2 vn2 rn1 rn2
  in
    if a mod b = 0 then (0,1,b)
    else if b mod a = 0 then (1,0,a)
    else if a mod b = 1 then (1,-a/b, 1)
    else if b mod a = 1 then (-b/a, 1, 1)
    else
      let r0 = a mod b in
      let u0, v0 = 1, -a / b in
      let q0, q1 = a/b, b / r0 in
      let u1,v1 = -b / r0, 1+q0*q1 in
      let u, v = bezout' u0 v0 u1 v1 r0 (b mod r0) in
	(u, v, a*u + b*v)
;;
let solve a b c = 
  let u, v, pgcd = bezout a b in
  let k = c / pgcd in
    (u*k, v*k, pgcd*k)
;;
let rec gcd a b = 
  if a mod b = 0 then b
  else 
    gcd b (a mod b)
;;
(* Find the n-ième number coprime with a *)
let findcp_n a n = 
  let rec coprime' a p n= 
    if gcd a p = 1 then 
      if n <= 0 then p
      else coprime' a (p+1) (n-1)
    else
      coprime' a (p+1) n
  in
    coprime' a 1 n
;;
(* With the n-ième number coprime with a, find n*)
(* p = findcp_n a n and find_cp_p a p = n  *)
let findcp_p a p = 
  let rec coprime' a p n = 
    if gcd a p = 1 then 
      if p <= 1 then n
      else 
	coprime' a (p-1) (n+1)
    else
      coprime' a (p-1) n
  in
    coprime' a p 0
;;
let rec map_string f s = 
  let arr = Array.make (String.length s) (f (s.[0])) in
  for i = 0 to String.length s -1 do
    arr.(i) <- f $ s.[i]
  done;
    arr
;;

let copy_key key n = 
  let len = String.length key in
  let s = String.make n ' ' in
    for i = 0 to n-1 do
      s.[i] <- key.[i mod len];
    done;
    s
;;

let crypt key msg = 
  let len = String.length msg in
  let k = copy_key key len in
  let arrk =  map_string (findcp_n 255|> int_of_char) k in
  let arrm =  map_string int_of_char msg in
  let r = String.make len ' ' in
    for i = 0 to len -1 do
      r.[i] <- char_of_int $ arrm.(i) * arrk.(i) mod 255
    done;
    r
;;  
let decrypt key msg = 
  let len = String.length msg in
  let k = copy_key key len in
  let arrk, arrm =  map_string (findcp_n 255|> int_of_char) k, map_string int_of_char msg in
  let r = String.make len ' ' in
    for i = 0 to len -1 do
      let k,_,_ = solve arrk.(i) (-255) 1 in
      r.[i] <- char_of_int $ mymod (k*arrm.(i)) 255;
    done;
    r
;;


decrypt "mdp~" $ crypt "mdp~" "Lorem ipsum dolor sit amet.";;

