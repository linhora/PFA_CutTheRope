open Graphics;;
open Unix;;

let ball = 15;;
let mass = 0.5;;
let bounce= 0.7;;

let score = ref 0;;

let maxVelocity= 15.0;;
  
open_graph " 900x800" ;;
Graphics.auto_synchronize false ;;
Graphics.set_line_width 2 ;;
Graphics.set_color black ;;

type infoShared = {mutable isClick: bool;mutable isOver : bool;mutable clickOrigine : int * int;mutable clickActuel : int * int};;  

let sharedCut =  {isClick=false;isOver=false;clickOrigine = (0,0);clickActuel = (0,0)};;

type balle = {mutable position : float*float;mutable vitesse : float*float;masse : float; taille : int};; 

let balle = {position=(100.0,780.0);vitesse=(0.0,0.0);masse=mass;taille=ball};;
  
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
  balle.vitesse <- (min (vx+.ax) maxVelocity,min (vy+.ay) maxVelocity);
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
      let (dx,dy)=(bx-.cx,by-.cy) in
      let (vx,vy)= balle.vitesse in
      let r = (float_of_int prop.size) in
      let cLen=sqrt (((dx)*.(dx))+.((dy)*.(dy))) in
      (*let tension = (r-.cLen)/.(cLen) in*)
      (*((-.1.)*.tension*.(dx),(-.1.)*.tension*.(dy))*)
      let cosAngleVerticale = dy/.cLen  in
      let cosAngleHorizontale = dx/.cLen in
      let k = 1. in                  
      let amortissement = 0.7 in
      let produitScalaire = dx*.vx+.dy*.vy in
      let projectionVitesse =  produitScalaire/.cLen in
      (*let fX = -.*.k/.balle.masse +.(vx*.vx*.amortissement) in
      let fY = -.dy*.k/.balle.masse +.(vy*.vy*.amortissement) in*)
      let fX = -.((min (k*.(cLen-.r)) 0.3)-.(min (projectionVitesse*.projectionVitesse*.amortissement*.(cLen-.r)) 0.2))*.cosAngleHorizontale in
      let fY = -.((min (k*.(cLen-.r)) 0.3)-.(min (projectionVitesse*.projectionVitesse*.amortissement*.(cLen-.r)) 0.2))*.cosAngleVerticale in
(*
      Printf.printf "cLen-r %f cLen %f fx %f fy %f \n" (cLen-.r) cLen fX fY;
*)
      balle.position <- ((cx+.(r-.1.)*.cosAngleHorizontale),(cy+.(r-.1.)*.cosAngleVerticale));
      (fX,fY)
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
  if (float_of_int prop.size)**2. <= ((r-.u)**2. +. (s-.w)**2.)+.200. then (drawLine (cx,cy) (bx,by);)
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
		(0.0,(-.1.0)*.(snd balle.vitesse))
		)
	else
		(
		(balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse*.2.)));
		(0.0,(-.1.0)*.(snd balle.vitesse))
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
	let circleDistanceX = abs_float (bx -. px-.((float_of_int prop.size)/.2.)) in
    let circleDistanceY = abs_float (by -. (py+.((float_of_int prop.size)/.2.))) in
	
    (px < bx && px +.(float_of_int prop.size)>bx) &&
    ((((circleDistanceX-.(float_of_int balle.taille)) <= (float_of_int prop.size/.2.)) &&
    ((circleDistanceY-.(float_of_int balle.taille)) <= (float_of_int prop.size/.2.))) ||
    (((((circleDistanceX -. ((float_of_int prop.size)/.2.))*.(circleDistanceX -. ((float_of_int prop.size)/.2.)))) +. ((circleDistanceY -. (float_of_int prop.size/.2.))*.(circleDistanceY -. (float_of_int prop.size/.2.)))) <= ((float_of_int balle.taille)*.(float_of_int balle.taille))));
;;


(****)



let toucheBlower balle prop =
	let (bx,by) = balle.position in
	let (px,py) = prop.pos in
	Printf.printf "TOUCHE? : %f %f %f %f %b \n" bx by px py (by> py && by < (py+.(float_of_int prop.size)) && (if prop.state = 1 then (bx>px)else (bx<px)));	
	(by> py && by < (py+.(float_of_int prop.size)) && (if prop.state = 1 then (bx > px) else (bx < px) ))
	
;;

let forceBlower balle prop = 
	match prop.state with
	1 ->(0.002,0.)
	|_ ->(-.0.002,0.)
;;

let drawBlower balle prop = 
	if prop.state = 1
	then
	(set_color (rgb 102 125 200);
	(fill_rect ((int_of_float (fst prop.pos))) (int_of_float (snd prop.pos)) (20) (prop.size));
	set_color (rgb 52 175 250);
	(fill_rect ((int_of_float (fst prop.pos))) (int_of_float (snd prop.pos)) (10) (prop.size));
	set_color (rgb 0 0 0);
	(fill_rect ((int_of_float (fst prop.pos))) (int_of_float (snd prop.pos)) (1) (prop.size));)
	else
	(set_color (rgb 102 125 200);
	(fill_rect ((int_of_float (fst prop.pos))) (int_of_float (snd prop.pos)) (20) (prop.size));
	set_color (rgb 52 175 250);
	(fill_rect ((int_of_float (fst prop.pos))+10) (int_of_float (snd prop.pos)) (10) (prop.size));
	set_color (rgb 0 0 0);
	(fill_rect ((int_of_float (fst prop.pos))+19) (int_of_float (snd prop.pos)) (1) (prop.size));)	
;;

(****)


let isInBubble balle prop =
  let (bx,by) = balle.position in
  let (px,py) = prop.pos in
(*
  Printf.printf "dx %f dy %f \n" (sqrt ((bx-.px)**2.0+.(by-.py)**2.0)) 0.1 ;
*)
  if  sqrt ((bx-.px)**2.0+.(by-.py)**2.0) <= (float_of_int prop.size) && prop.state <> 2
  then
  (prop.state<-1;
  true
  )
  else
  false
;;

let touchStar balle prop =
  let (bx,by) = balle.position in
  let (px,py) = prop.pos in
(*
  Printf.printf "dx %f dy %f \n" (sqrt ((bx-.px)**2.0+.(by-.py)**2.0)) 0.1 ;
*)
  if  sqrt ((bx-.px)**2.0+.(by-.py)**2.0) <= (float_of_int prop.size) && prop.state == 0
  then
  (prop.state<-1;
  (score := !score+1);
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
			((copysign 1. (fst balle.vitesse)*. (-.0.001)),(0.02))
		else
			((copysign 1. (fst balle.vitesse)*. (-.0.001)),0.005))
	else
		(
		(0.,0.)
		)
;;

let drawBublle balle prop = 
	let (px,py) = prop.pos in
	if prop.state <> 2 then
	(set_color (rgb 222 25 100);
	draw_circle (int_of_float px) (int_of_float py) ( prop.size);
	set_color (rgb 0 0 0);)
;;

let drawStar balle prop =
	let (px,py) = prop.pos in
	if prop.state=0 then 
	(set_color (rgb 222 175 50);
	draw_ball (int_of_float px) (int_of_float py) ( prop.size);
	set_color (rgb 0 0 0))
	else
	()
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
	 ; force   = (fun balle gravite-> (0.0,-.0.01))
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
	 ; force   = (fun balle gravite-> (	Printf.printf "YOU WIN DOOD score : %d \n" !score;(sharedCut.isOver <- true);(0.,0.)))
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
	

let createStar list =
	{
     id        = 4 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (100.,100.)  
	 ; contact = (touchStar)  
	 ; force   = (fun balle gravite-> (0.,0.))
	 ; draw    = (drawStar)
	 ; size    = if (List.length list ) > 3 then (List.nth list 3) else 20
	 ; state   = if (List.length list ) > 4 then (List.nth list 4) else 0
	}
;;

let createbouncer list =
	{
     id        = 5 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (100.,100.)  
	 ; contact = (toucheBouncer)  
	 ; force   = (forceBounce)
	 ; draw    = (drawBouncer)
	 ; size    = if (List.length list ) > 3 then (List.nth list 3) else 50
	 ; state   = if (List.length list ) > 4 then (List.nth list 4) else 1
	}
;;

let createBlower list =
	{
     id        = 6 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (100.,100.)  
	 ; contact = (toucheBlower)  
	 ; force   = (forceBlower)
	 ; draw    = (drawBlower)
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
		|4 -> createStar list
		|5 -> createbouncer list
		|6 -> createBlower list
		|_ -> {id=0;pos=(0.,0.);contact=(fun balle gravite ->true);force=(fun balle gravite->(0.,0.));draw=(fun balle gravite->());size=0;state=1}

	else {id=0;pos=(0.,0.);contact=(fun balle gravite ->true);force=(fun balle gravite->(0.,0.));draw=(fun balle gravite->());size=0;state=1}
;;

let rec lireF canal_entree =
	
	let ligne = input_line canal_entree in
	
	try
		let x =List.map int_of_string (Str.split (Str.regexp "[^0-9]+") ligne) in
(*
		print_list x;
		Printf.printf "\n" ;
*)

		createProps x :: lireF canal_entree;
		
	with End_of_file -> (close_in canal_entree;[])
;;


let lireFichier fichier =
	let canal_entree = open_in fichier in
	lireF canal_entree;
;;
let param = 
	if Array.length Sys.argv >1
	then Sys.argv.(1)
	else
	"test.txt"

let listofProps = lireFichier param;;

(*
Printf.printf "%d \n" (List.length listofProps );;
*)

(*****************************************************************************)
exception End;;

let detecterCut x1 y1 x2 y2 corde=
  let (xa,ya)=balle.position in
  let (xb,yb)=corde.pos in
  if not (x1=x2 && y1=y2)&& not (xa=xb && ya=yb) then(
    if (x1=x2) then(
      if (xa<=x1&&xb>=x1)||(xa>=x1&&xb<=x1) then
        (
          let a = (yb-.ya)/.(xb-.xa) in
          let b =  ya-.(a*.xa)in
          let yInter = a*.x1+.b in
          if (yInter <= ya && yInter >= yb)||(yInter <= yb && yInter >= ya) then (
            corde.state <- 2;))
    )else
      if (xa=xb) then(
        if (x1<=xa&&x2>=xa)||(x1>=xa&&x2<=xa) then
          (
            let a = (y2-.y1)/.(x2-.x1) in
            let b =  y1-.(a*.x1)in
            let yInter = a*.x1+.b in
            if (yInter <= y1 && yInter >= y2)||(yInter <= y2 && yInter >= y1) then (
              corde.state <- 2;))
      );
    let a1 = (y2-.y1)/.(x2-.x1) in
    let a2 = (yb-.ya)/.(xb-.xa) in
    let b1 = y1-.(a1*.x1) in
    let b2 = ya-.(a2*.xa)in
    if (a1<>a2) then (
      let xcom = (b2-.b1)/.(a1-.a2) in
      if (xcom<=x1&&xcom>=x2&&xcom<=xa&&xcom>=xb)||(xcom<=x2&&xcom>=x1&&xcom<=xa&&xcom>=xb)||(xcom<=x1&&xcom>=x2&&xcom<=xb&&xcom>=xa)||(xcom<=x2&&xcom>=x1&&xcom<=xb&&xcom>=xa) then
        (corde.state <-2)
  ));
  ()
;;

let detecterPop x1 y1 x2 y2 bulle = 
	let (xa, ya) = bulle.pos in
	let r = (float_of_int bulle.size) in
	Printf.printf "r2 %f  dx2 %f dy2 %f %d\n" (r*.r) (((x1-.xa)*.(x1-.xa)+.(y1-.ya)*.(y1-.ya))) ((y1-.ya)*.(y1-.ya)) (bulle.state);
	if ( ((x1-.xa)*.(x1-.xa)+.(y1-.ya)*.(y1-.ya)))< r*.r then (
		(
		Printf.printf "Pop";
		bulle.state <- 2;)
		)
	else (
		if (((x2-.xa)*.(x2-.xa)+.(y2-.ya)*.(y2-.ya)))< r*.r then
			(
			Printf.printf "Pop2";
			bulle.state <- 2;
			)
		else
		(
		Printf.printf "Pas Pop \n";
			())
		)
;;

let rec iterCutProps liste =
  match liste with
    [] -> ()
   |p::l -> 
     begin
       let (x1,y1) = sharedCut.clickOrigine in
       let (x2,y2) = sharedCut.clickActuel in
       if  (p.state = 1) then (
		match p.id with
			|3 -> detecterPop (float_of_int x1) (float_of_int y1) (float_of_int x2) (float_of_int y2) p;
			|2 -> detecterCut (float_of_int x1) (float_of_int y1) (float_of_int x2) (float_of_int y2) p;
			|_ -> (		Printf.printf "invalide :%d\n" p.id;);
		);
       iterCutProps l
     end
;;


let isOOB balle =
	let (bx,by) = balle.position in
	if(bx > 1500. || bx < -.100. || by > 1200. || by < -.100. )
	then sharedCut.isOver <- true
;;

let sleep sec =
  let start = Unix.gettimeofday () in
  let rec delay t =
        let now = Unix.gettimeofday () in
        let remaining = start +. t -. now in
        if remaining > 0.0 then delay remaining 
  in
  delay sec
;;
  
let rec game balle =
  Graphics.clear_graph ();
  isOOB balle;
  nextFrame balle listofProps;
  let(x,y) = balle.position in 
  draw_props balle listofProps;
  draw_ball (int_of_float x) (int_of_float y) ( balle.taille);
  if (Graphics.key_pressed()) then
    (
      let c = Graphics.read_key () in
      if c = 'q' then
        sharedCut.isOver <- true
    );
  if (not sharedCut.isClick) then
    if (Graphics.button_down()) then
      (
      sharedCut.isClick <- true;
	  sharedCut.clickOrigine<- Graphics.mouse_pos()
	  );
  
  if ( sharedCut.isClick && (not (Graphics.button_down())) ) then
    (
    sharedCut.clickActuel <- (Graphics.mouse_pos());
    iterCutProps listofProps;
    sharedCut.isClick <- false;
    );
  
  (*sleep 0.05;
  Graphics.synchronize();*)
  wait 900000;
  try  
    (
      if sharedCut.isOver then
        raise End;
      game balle
    )
  with
    End -> ();
           
;;

game balle;;
