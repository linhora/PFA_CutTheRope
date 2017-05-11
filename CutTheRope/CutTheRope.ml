open Graphics;;
open Unix;;
let ball = 15;;
let mass = 0.005;;
let bounce= 0.7;;
let maxVelocity= 15.0;;
  
open_graph " 900x800" ;;
Graphics.auto_synchronize false ;;
Graphics.set_line_width 2 ;;
Graphics.set_color black ;;

type balle = {mutable position : float*float;mutable vitesse : float*float;masse : float; taille : int};; 

let balle = {position=(150.0,300.0);vitesse=(0.0,-0.0);masse=1.0;taille=ball};;
  
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
      let (dx,dy)=(bx-.cx,by-.cy) in
      let (vx,vy)= balle.vitesse in
      let r = (float_of_int prop.size) in
      let cLen=sqrt (((dx)*.(dx))+.((dy)*.(dy))) in
      let tension = (r-.cLen)/.(cLen) in
    (*((-.1.)*.tension*.(dx),(-.1.)*.tension*.(dy))*)
      let angle = atan2 dy dx in
      let cosAngle = cos angle in
      let sinAngle = sin angle in
      let k = 0.001 in                  
      let amortissement = 0.05 in
      (*let fX = -.*.k/.balle.masse +.(vx*.vx*.amortissement) in
      let fY = -.dy*.k/.balle.masse +.(vy*.vy*.amortissement) in*)
      let fX = -.k*.(cLen-.r)*.(sinAngle)-.((vx*.vx+.vy*.vy)*.amortissement*.sinAngle) in
      let fY = -.k*.(cLen-.r)*.(cosAngle)-.((vx*.vx+.vy*.vy)*.amortissement*. cosAngle) in
      Printf.printf "r %f cLen %f fx %f cos %f sin %f\n" r cLen fX (cosAngle) (sinAngle);
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

let rec bob={
		     id        = 1 
			 ; pos     = (150.0,10.0) 
			 ; contact =  (fun balle bob -> (((snd balle.position)-.30.0<(snd bob.pos)) && (((fst balle.position)> (fst bob.pos)-.50.0)&&((fst balle.position)<(fst bob.pos)+.50.0))))  
			 ; force   = (fun balle bob-> (balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse*.2.))) ;(0.0,(-.2.0)*.(snd balle.vitesse)))
			 ; draw    = (fun balle bob-> (draw_rect ((int_of_float (fst bob.pos))-50) (int_of_float (snd bob.pos)) 100 2))
			 ; size    = 100
			 ; state   = 1
		    };;

let rec gravite={
			 	 id        = 0 
			 	 ; pos     = (-.1.0,-.1.0) 
			 	 ; contact = (fun balle gravite ->true) 
			 	 ; force   = (fun balle gravite-> (0.0,-.0.05))
			 	 ; draw    = (fun balle gravite-> ())
			 	 ; size    = 0
			 	 ; state   = 1
			    };;

let corde1={
			id        = 2 
			; pos     = (200.0,200.0) 
			; contact =  (isCordeTendue)  
			; force   = (calculForceCorde)
			; draw    = (affichageCorde)
			; size    = 185
			; state   = 0
		   };;

let corde2={
			id        = 2 
			; pos     = (100.0,210.0) 
			; contact =  (isCordeTendue)  
			; force   = (calculForceCorde)
			; draw    = (affichageCorde)
			; size    = 85
			; state   = 0
		   };;

let listeDeProps = [bob;gravite;corde1;corde2];;

(*****************************************************************************)


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

let createbouncer list =
	{
     id        = 1 
	 ; pos     = if (List.length list ) > 2 then (float_of_int(List.nth list 1),float_of_int(List.nth list 2)) else (100.,100.)  
	 ; contact =  (fun balle bob -> (((snd balle.position)-.30.0<(snd bob.pos)) && (((fst balle.position)> (fst bob.pos)-.50.0)&&((fst balle.position)<(fst bob.pos)+.50.0))))  
	 ; force   = (fun balle bob-> (balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse*.2.))) ;(0.0,(-.2.0)*.(snd balle.vitesse)))
	 ; draw    = (fun balle bob-> (draw_rect ((int_of_float (fst bob.pos))-50) (int_of_float (snd bob.pos)) 100 2))
	 ; size    = if (List.length list ) > 3 then (List.nth list 3) else 0
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

let rec createProps list =
	if (List.length list ) > 0 then 
		match (List.nth list 0) with
		0 -> createGravity list
		|1 -> createbouncer list
		|2 -> createRope list
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
(*
<<<<<<< HEAD
let ajoutForceCorde corde balle liste=
  if isCordeTendue balle corde then
    (calculForceCorde corde balle)::liste
  else
    liste
;;*)
type infoShared = {mutable isClick: bool;mutable isOver : bool;mutable clickOrigine : int * int;mutable clickActuel : int * int};;  

let listofProps = lireFichier "test.txt";;
Printf.printf "%d \n" (List.length listofProps );;

let sharedCut =  {isClick=false;isOver=false;clickOrigine = (0,0);clickActuel = (0,0)};;
(*****************************************************************************)
(*
let balle =  {position=(150.0,300.0);vitesse=(0.0,-0.0);masse=1.0;taille=ball};;
let listeDeProps = [gravite];;

let cordeNo1 = {origine = (300.0,300.0); longueur = 170.0; state = 0};;
let cordeNo2 = {origine = (00.,00.); longueur = 85.;state = 0};;
 *)
exception End;;
(*
let exeThread () =
  Format.printf "Thread lancé\n@.";
  let traitementKeyP key =
    if key = 'q' then
      raise End
  in
  let detecterCut x1 y1 x2 y2 corde=
    Mutex.lock m;
    let (xa,ya)=balle.position in
    let (xb,yb)=corde.origine in
    if (x1=x2) then(
      if (xa<=x1&&xb>=x1)||(xa>=x1&&xb<=x1) then
        toggleState corde
    )else
      if (xa=xb) then(
        if (x1<=xa&&x2>=xa)||(x1>=xa&&x2<=xa) then
          toggleState corde
      );
    let a1 = (y2-.y1)/.(x2-.x1) in
    let a2 = (yb-.ya)/.(xb-.xa) in
    let b1 = y1-.(a1*.x1) in
    let b2 = ya-.(a2*.xa)in
    if (a1<>a2) then (
      let xcom = (b2-.b1)/.(a1-.a2) in
      if (xcom<=x1&&xcom>=x2&&xcom<=xa&&xcom>=xb)||(xcom<=x2&&xcom>=x1&&xcom<=xa&&xcom>=xb)||(xcom<=x1&&xcom>=x2&&xcom<=xb&&xcom>=xa)||(xcom<=x2&&xcom>=x1&&xcom<=xb&&xcom>=xa) then
        toggleState corde
    );
    Mutex.unlock m
  in
  
  try
    while true do
      try
        let s = Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed] in
        if (s.Graphics.keypressed) then (traitementKeyP s.Graphics.key)
        else
          if s.Graphics.button
          then (
            let (x1,y1) = (float_of_int s.Graphics.mouse_x,float_of_int s.Graphics.mouse_y) in
            Printf.printf "waiting clic out";
            let s2 = Graphics.wait_next_event [Graphics.Button_up] in
            let (x2,y2) = (float_of_int s2.Graphics.mouse_x,float_of_int s2.Graphics.mouse_y) in
            detecterCut x1 y1 x2 y2 cordeNo1;
            detecterCut x1 y1 x2 y2 cordeNo2;
          )
      with
        End -> raise End
       |e -> Printf.printf "exception non traitée levée"
    done
  with
    End -> (Mutex.lock m;
            sharedCut.isOver <- true;
           Mutex.unlock m)
  
;;
 *)

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

let rec iterCutProps liste =
  match liste with
    [] -> ()
   |p::l -> 
     begin
       let (x1,y1) = sharedCut.clickOrigine in
       let (x2,y2) = sharedCut.clickActuel in
       if p.id = 2 && p.state = 1 then
         detecterCut (float_of_int x1) (float_of_int y1) (float_of_int x2) (float_of_int y2) p;
       iterCutProps l
     end
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
      (sharedCut.isClick <- true;
       sharedCut.clickOrigine<- Graphics.mouse_pos());
  if ( sharedCut.isClick && (not (Graphics.button_down())) ) then
    (
      sharedCut.clickActuel <- (Graphics.mouse_pos());
      iterCutProps listofProps;
      sharedCut.isClick <- false;
    );
  
  
  
  (*sleep 0.05;
  Graphics.synchronize();*)
  wait 700000;
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
