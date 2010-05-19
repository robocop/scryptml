(*
let quotient a b = (a - (a mod b)) / b;;
quotient a b is a / b
*)
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

let rec find_prime_with a n p = 
  let is_prime =  p mod a = 1 in
    if is_prime && n = 0 then p
    else find_prime_with a (if is_prime then n-1 else n) (p+1)
;;
find_prime_with 26 24 2;;
