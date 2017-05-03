type corde = {origine : float*float; longueur : float; mutable state : int};;

let toggleState corde =
  corde.state <- corde.state+1
;;

let isInCercle balle (cx,cy) longueur =
  let (bx,by) = balle.position in
  sqrt ((bx-.cx)**2.0+.(by-.cy)**2.0) <= longueur
;;
  
       
let isCordeTendue balle corde =
  let (bx,by) = balle.position in
  let (cx,cy) = corde.origine in
  sqrt ((bx-.cx)**2.0+.(by-.cy)**2.0) >= corde.longueur
;;

let calculForceCorde corde balle forcesActuelles =
  let (fx,fy) = (sommeForceX forcesActuelles balle, sommeForceY forcesActuelles balle) in
  let (bx,by) = balle.position in
  let (cx,cy) = corde.origine in
  let coteAdj = (abs_float (bx-.cx))in
  let cosAngle = coteAdj/.corde.longueur in
  let angle = acos cosAngle in
  let sinAngle = sin angle in
  fun balle -> (fx*.cosAngle,fy*.sinAngle)
;;


(* rs représente le point le plus a gauche, u v le plus a droite*)
let calculConstanteCat (r,s) (u,v) longueur = 			   
  let rec solveZ z =
    if ((sinh z )/. z >= sqrt((longueur**2.0)-.(v-.s)**2.0)/.(u-.r)) then z
    else solveZ z+.0.001
  in
  solveZ 0.0
;;

let drawCorde corde balle =
  let (cx,cy) = corde.origine in
  let (bx,by) = balle.position in
  let (r,s) = if (cx<bx) then (cx,cy) else (bx,by) in
  let (u,v) = if (cx>=bx) then (cx,cy) else (bx,by) in					     
  let z = calculConstanteCat (r,s) (u,v) corde.longueur in
  let a = (u-.r)/.2.0/.z in
  let p = (r+.u-.a*. (log ((corde.longueur +.v-.s)/.(corde.longueur -.v+.s))) )/.2.0 in
  let q = (v+.s-.corde.longueur*.(sinh z))/.2.0 in
  for x = (int_of_float r) to (int_of_float u) do
    Graphics.plot x (int_of_float (a*.cosh ((float_of_int x-.p)/.a)+.q))
  done
;;
    
