open Graphics;; 

let ball = 30;;
let mass = 0.005;;
let bounce= 0.7;;
let maxVelocity= 15.0;;

open_graph " 900x800"
;;

Graphics.auto_synchronize false
;;



type balle = {mutable position : float*float;mutable vitesse : float*float;masse : float; taille : int};; 

type props = {id : int ; mutable pos : float*float ; contact :  (balle -> (float*float) -> bool) ; force : balle -> (float*float); draw : float*float -> unit};; 

let evalForces balle listForces = 
  begin
    let rec evaluation balle = function 
      |[] -> []
      |f :: [] -> f balle :: []
      |f :: l -> f balle :: evaluation balle l
    in
    evaluation balle listForces 
end ;;
let sommeForceX listeForces balle = 
  begin
    let rec sommeX = function 
      |[]->0.0
      |(x,_)::l->x +. sommeX l
    in
    sommeX (evalForces balle listeForces)
  end;;

let sommeForceY listeForces balle = 
  begin
    let rec sommeY = function
      |[]->0.0
      |(_,y)::l->y+.sommeY l
    in
    sommeY (evalForces balle listeForces)
  end;;
    
let nextFrame balle listForces=
    begin
      let acceleration = ((sommeForceX listForces balle) /. balle.masse,(sommeForceY listForces balle) /. balle.masse) in
      let (ax,ay) = acceleration in
      let (vx,vy) = balle.vitesse in
      let (x,y) = balle.position in
      balle.vitesse <- (vx+.ax,vy+.ay);
      balle.position <- (x+.vx,y+.vy);
    end;;




let draw_ball x y size =
	fill_circle x y size
;;


;;

(*
let new_speed (vx,vy) (ax,ay) =
	if(vy > -.maxVelocity)
		then (vx+.ax,vy+.ay)
	else
		(vx+.ax,vy+.ay)
;;

let new_position (bx,by) (vx,vy) =
	(bx+.vx,by+.vy)
;;


let detect_bounce (bx,by) (vx,vy) =
	if by -. float_of_int ball/.2.0 < 1.0
		then  (vx,(vy*. -.0.8))
	else
		(vx,vy)
;;


let detect_bounce2 (bx,by) (vx,vy) platfrom =
	if by -. float_of_int ball <= 0.0
		then  ((bx+.(vx*.(abs_float (by-.(float_of_int ball))/.vy)),(float_of_int ball)+.((vy*. -.bounce)*.(abs_float (by-.(float_of_int ball))/.vy))),(vx,(vy*. -.bounce)))
	else
	((bx,by),(vx,vy))
;;
*)


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
    (*Printf.printf "TEST z: %f \n" z ;*)
    if ((sinh z )/. z >= sqrt((longueur**2.0)-.(v-.s)**2.0)/.(u-.r)) then z
    else solveZ (z+.0.001)
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
  for x = (int_of_float r)+1 to (int_of_float u)-1 do
    fill_circle x (int_of_float (a*.cosh ((float_of_int x-.p)/.a)+.q)) 1
  done
;;
    




(*************)



let rec get_list_force listProps balle=
		match listProps with
		[]  -> []
		| p::r -> 	if p.contact balle (fst p.pos,snd p.pos)  
						then p.force :: get_list_force r balle
					else
						get_list_force r balle;
		
;;



let rec draw_props listProps=
		match listProps with
		[]  -> ()
		| p::r -> 	p.draw (fst p.pos,snd p.pos);draw_props r;
		
;;

let calc_acceleration =
	(0.0,-.mass)
;;
(*
let detect_bounce (bx,by) (vx,vy) =
	if by -. float_of_int ball <= 0.0
		then  ((bx+.(vx*.(abs_float (by-.(float_of_int ball))/.vy)),(float_of_int ball)+.((vy*. -.bounce)*.(abs_float (by-.(float_of_int ball))/.vy))),(vx,(vy*. -.bounce)))
	else
	((bx,by),(vx,vy))
;;
*)
(*#####################################*)

let rec wait n = 
	if n=0 then Graphics.synchronize () else wait (n-1)
;;


let bob= {id = 1 ; pos = (200.0,10.0) ; contact =  (fun balle (x,y) -> (((snd balle.position)-.30.0<y) && (((fst balle.position)> x-.50.0)&&((fst balle.position)<x+.50.0))))  ; force = (fun balle -> (balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse))) ;(0.0,(-.2.0)*.(snd balle.vitesse))); draw = (fun (x,y)-> (draw_rect ((int_of_float x)-50) (int_of_float y) 100 2))};;

let gravite= {id = 0 ; pos = (-.1.0,-.1.0) ; contact = (fun balle (x,y) ->true) ; force = (fun balle -> (0.0,-.0.001)); draw = (fun (x,y)-> ())};;

let listeDeProps = [bob;gravite];;


let cordeNo1 = {origine = (200.0,200.0); longueur = 100.0; state = 1};;

let rec game balle =

	Graphics.clear_graph ();
	
	
	if(isCordeTendue balle cordeNo1)
		then
			nextFrame balle ((calculForceCorde cordeNo1 balle (get_list_force listeDeProps balle))::(get_list_force listeDeProps balle)  )
	else
		nextFrame balle (get_list_force listeDeProps balle);
	
	let(x,y) = balle.position in 
	let(vx,vy) = balle.vitesse in 
	
	draw_props listeDeProps;
	
	draw_ball (int_of_float x) (int_of_float y) ( balle.taille);
	
	(*drawCorde cordeNo1 balle;*)
	
	
	wait 500000;
	
	
	Printf.printf "BEF y: %f vy : %f  \n" y vy ;


	
	game balle
	
;;


game {position=(250.0,200.0);vitesse=(0.0,-0.05);masse=1.0;taille=ball};;


