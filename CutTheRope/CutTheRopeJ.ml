open Graphics;; 
open Str;;

let ball = 30;;
let mass = 0.005;;
let bounce= 0.7;;
let maxVelocity= 15.0;;

open_graph " 900x800";;

Graphics.auto_synchronize false;;

(***************************************************************************)

type balle = {mutable position : float*float;mutable vitesse : float*float;masse : float; taille : int};; 

let evalForces balle listForces = 
  begin
    let rec evaluation balle = function 
      |[] -> []
      |f :: [] -> f balle :: []
      |f :: l -> f balle :: evaluation balle l
    in
    evaluation balle listForces 
end ;;
let rec sommeForceX forcesCalcules  = 
  match forcesCalcules with  
  |[]->0.0
  |(x,_)::l->x +. sommeForceX l
;;

let rec sommeForceY forcesCalcules = 
  match forcesCalcules with
  |[]->0.0
  |(_,y)::l->y+.sommeForceY l
;;
    
let nextFrame balle listForces =
  let forcesCalcules = evalForces balle listForces in
  let acceleration = ((sommeForceX forcesCalcules) /. balle.masse,(sommeForceY forcesCalcules) /. balle.masse) in
  let (ax,ay) = acceleration in
  let (vx,vy) = balle.vitesse in
  let (x,y) = balle.position in
  balle.vitesse <- (vx+.ax,vy+.ay);
  balle.position <- (x+.vx,y+.vy);
;;

(***************************************************************************)

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
  let forcesCalcules = evalForces balle forcesActuelles in
  let (fx,fy) = (sommeForceX forcesCalcules, sommeForceY forcesCalcules) in
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
    
(***************************************************************************)


type props = 
		{id : int ; mutable pos : float*float ; contact :  (balle -> (float*float) -> bool) ; force : balle -> (float*float); draw : float*float -> unit}
;;

type full_props = Prop of props|PropC of corde;;

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

(***************************************************************************)

let draw_ball x y size =
	fill_circle x y size
;;

let rec wait n = 
	if n=0 then Graphics.synchronize () else wait (n-1)
;;


(*****)

let load_level level = 
	let canal_entree = open_in level in
	let ligne1 = input_line canal_entree in
	print_string ligne1;
	

;;

let rec readline level = 
	let canal_entree = open_in level in
	let ligne1 = input_line canal_entree in
	print_string ligne1;
	

;;
(*****)


let bob= {id = 1 ; pos = (200.0,10.0) ; contact =  (fun balle (x,y) -> if(((snd balle.position)-.30.0<y) && (((fst balle.position)> x-.70.0)&&((fst balle.position)<x+.70.0)))then (balle.position <- ((fst balle.position,(y+.( float_of_int ball))+.(-.snd balle.vitesse))) ; true) else false )  ; force = (fun balle ->(0.0,(-.2.0)*.(snd balle.vitesse))); draw = (fun (x,y)-> (draw_rect ((int_of_float x)-50) (int_of_float y) 100 2))};;

let gravite= {id = 0 ; pos = (-.1.0,-.1.0) ; contact = (fun balle (x,y) ->true) ; force = (fun balle -> (0.0,-.0.001)); draw = (fun (x,y)-> ())};;

let cordeNo1 = {origine = (200.0,200.0); longueur = 100.0; state = 1};;


(*

let bob= Prop{id = 1 ; pos = (200.0,10.0) ; contact =  (fun balle (x,y) -> (((snd balle.position)-.30.0<y) && (((fst balle.position)> x-.70.0)&&((fst balle.position)<x+.70.0))))  ; force = (fun balle -> (balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse)*.2.0)) ;(0.0,(-.2.0)*.(snd balle.vitesse))); draw = (fun (x,y)-> (draw_rect ((int_of_float x)-50) (int_of_float y) 100 2))};;

let gravite= Prop{id = 0 ; pos = (-.1.0,-.1.0) ; contact = (fun balle (x,y) ->true) ; force = (fun balle -> (0.0,-.0.001)); draw = (fun (x,y)-> ())};;

let cordeNo1 = PropC{origine = (200.0,200.0); longueur = 100.0; state = 1};;

let listeDeProps2 = [bob;gravite;cordeNo1];;

*)


let rec print_list l = match l with
 |[] -> ()
 |e::f -> print_int e; print_string " "; print_list f;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let rec lireF canal_entree =
	
	let ligne = input_line canal_entree in
	
	try
		Printf.printf "Ligne : %s\n " ligne ;
		
		let x =List.map int_of_string (Str.split (Str.regexp "[^0-9]+") ligne) in
		print_list x;
		Printf.printf "\n " ;
		Printf.printf "%d \n " (List.length x ) ;
		
		
		lireF canal_entree;
		
	with End_of_file -> 0. ;;
;;


let lireFichier fichier =
	let canal_entree = open_in fichier in
	
	lireF canal_entree;

	close_in canal_entree;;
;;


let listeDeProps = [bob;gravite];;

let rec game balle =

	Graphics.clear_graph ();
	
	
	(*if(isCordeTendue balle cordeNo1)
		then
			nextFrame balle ((calculForceCorde cordeNo1 balle (get_list_force listeDeProps balle))::(get_list_force listeDeProps balle)  )
	else*)
		nextFrame balle (get_list_force listeDeProps balle);
	
	let(x,y) = balle.position in 
	
	draw_props listeDeProps;
	
	draw_ball (int_of_float x) (int_of_float y) ( balle.taille);
	
	(*drawCorde cordeNo1 balle;*)
	
	wait 250000;
		
	

	game balle
	
;;

lireFichier "test.txt";;



game {position=(200.0,200.0);vitesse=(0.0,-0.05);masse=1.0;taille=ball};;


