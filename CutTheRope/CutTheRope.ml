open Graphics;;
open Unix;;  

let ball = 20;;

let maxVelocity= 15.0;;
  
open_graph " 900x800" ;;
Graphics.auto_synchronize false ;;
Graphics.set_line_width 2 ;;
Graphics.set_color black ;;

type balle = {mutable position : float*float;mutable vitesse : float*float;masse : float; taille : int};; 


type props = {
			  id            : int 
			; mutable pos   : float*float 
			; contact       : balle -> props -> bool 
			; force         : balle -> props -> (float*float)
			; draw 	        : balle -> props -> unit
			; size          : int
			; mutable state : int
			};; 

let rec draw_props balle listProps=
		match listProps with
		[]  -> ()
		| p::r -> 	(p.draw balle (p));draw_props balle r;
;;

let rec wait n = 
  if n=0 then Graphics.synchronize () else wait (n-1)
;;


let evalForces balle listProp = 
  begin
  
    let rec evaluation balle list = match list with 
      |[] -> []
      |f :: l -> 
		if (f.contact balle f) then 
			f.force balle f :: evaluation balle l 
		else 
			(0.,0.) :: evaluation balle l
    in
    evaluation balle listProp 
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
    
let nextFrame balle listProps =
  let forcesCalcules = evalForces balle listProps in
  let acceleration = ((sommeForceX forcesCalcules) /. balle.masse,(sommeForceY forcesCalcules) /. balle.masse) in
  let (ax,ay) = acceleration in
  let (vx,vy) = balle.vitesse in
  let (x,y) = balle.position in
  balle.vitesse <- (vx+.ax,vy+.ay);
  balle.position <- (x+.vx,y+.vy);
;;

let draw_ball x y size =
	fill_circle x y size
;;

(***********************************************************************)

let isInCercle balle (cx,cy) longueur =
  let (bx,by) = balle.position in
  sqrt ((bx-.cx)**2.0+.(by-.cy)**2.0) <= longueur
;;

  
let isCordeTendue balle prop =
  let (bx,by) = balle.position in
  let (cx,cy) = prop.pos in
  
  if sqrt ((bx-.cx)**2.0+.(by-.cy)**2.0) >= (float_of_int prop.size)
  then
	(true)
  else
	(if prop.state = 0 then
		prop.state <- 1;
	false)
;;

let calculForceCorde balle prop =
  if prop.state = 1 then
    (
      let (bx,by) = balle.position in
      let (cx,cy) = prop.pos in
      let (dx,dy)=(cx-.bx,cy-.by) in
      let r = (float_of_int prop.size) in
      let (vx,vy)= balle.vitesse in
      let cLen=sqrt ((cx-.bx)**2.+.(cy-.by)**2.) in
      let k = 0.6 in
      (((dx/.r)*.(r-.cLen)*.(-.k))-.k*.1.2*.vx,((dy/.r)*.(r-.cLen)*.(-.k))-.k*.1.2*.vy)
    )
  else
    (0.,0.)
;;

let drawLine (ax,ay) (bx,by) =
  (*Printf.printf "Line call\n";*)
  Graphics.moveto (int_of_float ax) (int_of_float ay);
  Graphics.lineto (int_of_float bx) (int_of_float by); 
;;
  
let drawCorde balle prop =
  (*Printf.printf "Draw call\n";*)
  let (x1,y1) = prop.pos in
  let (x2,y2) = balle.position in
  let (cx,cy) = (x1,y1) in
  let (bx,by) = (x2,y2) in
  let (r,s) = if (cx<bx) then (cx,cy) else (bx,by) in
  let (u,w) = if (cx>=bx) then (cx,cy) else (bx,by) in
  if (float_of_int prop.size)**2. < ((r-.u)**2. +. (s-.w)**2.) then (drawLine (cx,cy) (bx,by);)
  else (
    if bx=cx then drawLine (cx,cy) (bx,by)
    else (
      let rec solveZ z =
        if ((sinh z )/.z)>=((sqrt ((float_of_int prop.size)*.(float_of_int prop.size)-.(w-.s)*.(w-.s)))/.(u-.r))
        then z
        else solveZ (z+.0.001)
      in
      let z = solveZ 0.001 in
      (*Printf.printf "z %f" z;*)
      let a = (u-.r)/.2./.z in
      let p = (r+.u-.a*.log (((float_of_int prop.size)+.w-.s)/.((float_of_int prop.size)-.w+.s)))/.2. in
      let q = (w+.s-.(float_of_int prop.size)*.(cosh z)/.(sinh z))/.2. in
      let rec drawSegment x y =
        if (floor x=floor u) then (Graphics.lineto (int_of_float x) (int_of_float y);())
	else (
	  Graphics.lineto (int_of_float x) (int_of_float y);
	  drawSegment (x+.1.) ((a*.(cosh ((x-.p)/.a) ))+.q);
        )
      in
      Graphics.moveto (int_of_float r) (int_of_float s);
      drawSegment r s;
    )
  )
         
;;

let affichageCorde balle prop =
  let (x,y) = prop.pos in
  Graphics.draw_circle (int_of_float x) (int_of_float y) 2;
  if prop.state = 0 then
    Graphics.draw_circle (int_of_float x) (int_of_float y) (prop.size)
  else (if prop.state = 1 then
          drawCorde balle prop
        else
          ())
;;

(****************************************************************************)


let circleRectTouch balle prop =
	
	let (bx,by) = balle.position in
	let (px,py) = prop.pos in	
	let circleDistanceX = abs_float (bx -. px+.((float_of_int prop.size)/.2.)) in
    let circleDistanceY = abs_float (by -. (py-.1.)) in

    (px < bx && px+.(float_of_int prop.size)>bx) &&
    ((circleDistanceX <= (float_of_int balle.taille)) || (circleDistanceY <= (float_of_int balle.taille)) ||
    (((((circleDistanceX -. ((float_of_int prop.size)/.2.))*.(circleDistanceX -. ((float_of_int prop.size)/.2.)))) +. ((circleDistanceY -. 1.)*.(circleDistanceY -. 1.))) <= ((float_of_int balle.taille)*.(float_of_int balle.taille))));
;;

let toucheBouncer balle prop =
	circleRectTouch balle prop  
;;

let forceBounce balle bob = 
	
	if (snd balle.vitesse) > 0.
	then 
		(
		(balle.position <- (fst balle.position,(snd balle.position)-.(snd balle.vitesse*.2.)));
		(0.0,(-.2.0)*.(snd balle.vitesse))
		)
	else
		(
		(balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse*.2.)));
		(0.0,(-.2.0)*.(snd balle.vitesse))
		)
;;
	
let drawBouncer balle bob = 
	(draw_rect ((int_of_float (fst bob.pos))) (int_of_float (snd bob.pos)) (bob.size) 1)
;;


(***)

let drawGoal balle bob = 
	 set_color (rgb 22 225 100);
	(fill_rect ((int_of_float (fst bob.pos))) (int_of_float (snd bob.pos)) (bob.size) (bob.size));
	 set_color (rgb 0 0 0);
;;

let toucheGoal balle prop =
	let (bx,by) = balle.position in
	let (px,py) = prop.pos in	
	let circleDistanceX = abs_float (bx -. px+.((float_of_int prop.size)/.2.)) in
    let circleDistanceY = abs_float (by -. (py+.((float_of_int prop.size)/.2.))) in
	
    (px < bx && px +.(float_of_int prop.size)>bx) &&
    (((circleDistanceX-.(float_of_int balle.taille)) <= (float_of_int prop.size/.2.)) ||
    ((circleDistanceY-.(float_of_int balle.taille)) <= (float_of_int prop.size/.2.)) ||
    (((((circleDistanceX -. ((float_of_int prop.size)/.2.))*.(circleDistanceX -. ((float_of_int prop.size)/.2.)))) +. ((circleDistanceY -. 1.)*.(circleDistanceY -. 1.))) <= ((float_of_int balle.taille)*.(float_of_int balle.taille))));
;;


(****)


let isInBubble balle prop =
  let (bx,by) = balle.position in
  let (px,py) = prop.pos in
  Printf.printf "dx %f dy %f \n" (sqrt ((bx-.px)**2.0+.(by-.py)**2.0)) 0.1 ;
  if  sqrt ((bx-.px)**2.0+.(by-.py)**2.0) <= (float_of_int prop.size)
  then
  (prop.state<-1;
  true
  )
  else
  false
;;

let forceBublle balle prop = 
	
	if prop.state=1
	then 
		(let (bx,by) = balle.position in
		prop.pos <- (bx,by);
		if (snd balle.vitesse) <1.
		then
			(0.0,(0.02))
		else
			(0.,0.005))
	else
		(
		(0.,0.)
		)
;;

let drawBublle balle prop = 
	let (px,py) = prop.pos in
	set_color (rgb 222 25 100);
	draw_circle (int_of_float px) (int_of_float py) ( prop.size);
	set_color (rgb 0 0 0);
;;


(****************************************************************************)

let rec print_list l = match l with
 |[] -> ()
 |e::f -> print_int e; print_string " "; print_list f;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;






let createGravity list =
	{
 	 id        = 0 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (0.,0.)
 	 ; contact = (fun balle gravite ->true) 
	 ; force   = (fun balle gravite-> (0.0,-.0.005))
	 ; draw    = (fun balle gravite-> ())
	 ; size    = if (List.length list ) > 3 then (List.nth list 3) else 0
	 ; state   = if (List.length list ) > 4 then (List.nth list 4) else 1
    }
;;

let createGoal list =
	{
 	 id        = 1 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (0.,0.)
 	 ; contact = (toucheGoal) 
	 ; force   = (fun balle gravite-> (set_color (rgb 0 100 255);(0.,0.)))
	 ; draw    = (drawGoal)
	 ; size    = if (List.length list ) > 3 then (List.nth list 3) else 100
	 ; state   = if (List.length list ) > 4 then (List.nth list 4) else 1
    }
;;
	
let createRope list =
	{
	id        = 2 
	; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (0.,0.)
	; contact =  (isCordeTendue)  
	; force   = (calculForceCorde)
	; draw    = (affichageCorde)
	; size    = if (List.length list ) > 3 then (List.nth list 3) else 100
	; state   = if (List.length list ) > 4 then (List.nth list 4) else 0
   }
;;

let createBublle list =
	{
     id        = 3 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (100.,100.)  
	 ; contact = (isInBubble)  
	 ; force   = (forceBublle)
	 ; draw    = (drawBublle)
	 ; size    = if (List.length list ) > 3 then (List.nth list 3) else 30
	 ; state   = if (List.length list ) > 4 then (List.nth list 4) else 0
	}
;;
	

let createbouncer list =
	{
     id        = 4 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (100.,100.)  
	 ; contact = (toucheBouncer)  
	 ; force   = (forceBounce)
	 ; draw    = (drawBouncer)
	 ; size    = if (List.length list ) > 3 then (List.nth list 3) else 0
	 ; state   = if (List.length list ) > 4 then (List.nth list 4) else 1
	}
;;









let rec createProps list =
	if (List.length list ) > 0 then 
		match (List.nth list 0) with
		0 -> createGravity list
		|1 -> createGoal list
		|2 -> createRope list
		|3 -> createBublle list
		|4 -> createbouncer list
		|_ -> {id=0;pos=(0.,0.);contact=(fun balle gravite ->true);force=(fun balle gravite->(0.,0.));draw=(fun balle gravite->());size=0;state=1}

	else {id=0;pos=(0.,0.);contact=(fun balle gravite ->true);force=(fun balle gravite->(0.,0.));draw=(fun balle gravite->());size=0;state=1}
;;

let rec lireF canal_entree =
	
	let ligne = input_line canal_entree in
	
	try
		let x =List.map int_of_string (Str.split (Str.regexp "[^0-9]+") ligne) in
		print_list x;
		Printf.printf "\n" ;

		createProps x :: lireF canal_entree;
		
	with End_of_file -> (close_in canal_entree;[])
;;


let lireFichier fichier =
	let canal_entree = open_in fichier in
	lireF canal_entree;
;;

let listofProps = lireFichier "test.txt";;
Printf.printf "%d \n" (List.length listofProps );;


(*****************************************************************************)



let rec game balle =

  Graphics.clear_graph ();

  nextFrame balle listofProps;

  let(x,y) = balle.position in 

 draw_ball (int_of_float x) (int_of_float y) ( balle.taille);
  draw_props balle listofProps;
 
  
  wait 700000;
  
  game balle
       
;;



game {position=(280.0,300.0);vitesse=(0.0,0.4);masse=1.0;taille=ball};;
