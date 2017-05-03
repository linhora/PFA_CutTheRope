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
    if ((sinh z )/. z >= sqrt((longueur**2.0)-(
    
