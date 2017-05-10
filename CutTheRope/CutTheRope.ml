open Graphics;;
open Unix;;
open Mutex;;

let ball = 15;;
let mass = 0.005;;
let bounce= 0.7;;
let maxVelocity= 15.0;;
  
open_graph " 900x800"
;;
  
Graphics.auto_synchronize false
;;
  
Graphics.set_line_width 2
;;
Graphics.set_color black
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
  corde.state <- corde.state+1 ;
  ()
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

let calculForceCorde corde balle =
  if corde.state = 1 then
    (
      let (bx,by) = balle.position in
      let (cx,cy) = corde.origine in
      let (dx,dy)=(cx-.bx,cy-.by) in
      let r = corde.longueur in
      let (vx,vy)= balle.vitesse in
      let cLen=sqrt ((cx-.bx)**2.+.(cy-.by)**2.) in
      let k = 0.6 in
      fun balle -> (((dx/.r)*.(r-.cLen)*.(-.k))-.k*.1.2*.vx,((dy/.r)*.(r-.cLen)*.(-.k))-.k*.1.2*.vy)
    )
  else
    fun balle -> (0.,0.)
;;

let drawLine (ax,ay) (bx,by) =
  (*Printf.printf "Line call\n";*)
  Graphics.moveto (int_of_float ax) (int_of_float ay);
  Graphics.lineto (int_of_float bx) (int_of_float by); 
;;
  
let drawCorde corde balle =
  (*Printf.printf "Draw call\n";*)
  let (x1,y1) = corde.origine in
  let (x2,y2) = balle.position in
  let (cx,cy) = (x1,y1) in
  let (bx,by) = (x2,y2) in
  let (r,s) = if (cx<bx) then (cx,cy) else (bx,by) in
  let (u,w) = if (cx>=bx) then (cx,cy) else (bx,by) in
  if corde.longueur**2. < ((r-.u)**2. +. (s-.w)**2.) then (drawLine (cx,cy) (bx,by);)
  else (
    if bx=cx then drawLine (cx,cy) (bx,by)
    else (
      let rec solveZ z =
        if ((sinh z )/.z)>=((sqrt (corde.longueur*.corde.longueur-.(w-.s)*.(w-.s)))/.(u-.r))
        then z
        else solveZ (z+.0.001)
      in
      let z = solveZ 0.001 in
      (*Printf.printf "z %f" z;*)
      let a = (u-.r)/.2./.z in
      let p = (r+.u-.a*.log ((corde.longueur+.w-.s)/.(corde.longueur-.w+.s)))/.2. in
      let q = (w+.s-.corde.longueur*.(cosh z)/.(sinh z))/.2. in
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
  


let affichageCorde corde balle =
  let (x,y) = corde.origine in
  Graphics.draw_circle (int_of_float x) (int_of_float y) 2;
  if corde.state = 0 then
    Graphics.draw_circle (int_of_float x) (int_of_float y) (int_of_float corde.longueur)
  else (if corde.state = 1 then
          drawCorde corde balle
        else
          ())
;;


let checkCordeState corde balle =
  if corde.state = 0 && (isInCercle balle corde.origine corde.longueur) then
    toggleState corde
  else
    ()
;;
  
(*
    Printf.printf "0 r:%f s:%f u:%f v:%f l:%f" r s u v corde.longueur;
    let z = calculConstanteCat (r,s) (u,v) corde.longueur in
    Printf.printf "1";
    let a = (u-.r)/.2.0/.z in
    Printf.printf "2";
    let p = (r+.u-.a*. (log ((corde.longueur +.v-.s)/.(corde.longueur -.v+.s))) )/.2.0 in
    Printf.printf "3";
    let q = (v+.s-.corde.longueur*.(sinh z))/.2.0 in
    Printf.printf "4";
    for x = (int_of_float r)+1 to (int_of_float u)-1 do (
      Printf.printf "";
      fill_circle x (int_of_float (a*.cosh ((float_of_int x-.p)/.a)+.q)) 2 ;
      Printf.printf "";)
    done *)



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
  (*Printf.printf "TEST";
  Unix.select [] [] [] (float_of_int n);
  Printf.printf "TEST";
  Graphics.synchronize ()*)
;;

let ajoutForceCorde corde balle liste=
  if isCordeTendue balle corde then
    (calculForceCorde corde balle)::liste
  else
    liste
;;
type infoShared = {mutable isCut: bool;mutable isOver : bool};;  

let bob= {id = 1 ; pos = (200.0,10.0) ; contact =  (fun balle (x,y) -> (((snd balle.position)-.30.0<y) && (((fst balle.position)> x-.50.0)&&((fst balle.position)<x+.50.0))))  ; force = (fun balle -> (balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse*.2.))) ;(0.0,(-.2.0)*.(snd balle.vitesse))); draw = (fun (x,y)-> (draw_rect ((int_of_float x)-50) (int_of_float y) 100 2))};;

let gravite= {id = 0 ; pos = (-.1.0,-.1.0) ; contact = (fun balle (x,y) ->true) ; force = (fun balle -> (0.0,-.0.005)); draw = (fun (x,y)-> ())};;

let balle =  {position=(150.0,300.0);vitesse=(0.0,-0.05);masse=1.0;taille=ball};;
let listeDeProps = [gravite];;
let sharedCut =  {isCut=false;isOver=false};;
let cordeNo1 = {origine = (200.0,200.0); longueur = 85.0; state = 0};;
let cordeNo2 = {origine = (120.,210.); longueur = 85.;state = 0};;

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

let rec game balle =
  Graphics.clear_graph ();
  checkCordeState cordeNo1 balle;
  checkCordeState cordeNo2 balle;
  let liste = get_list_force listeDeProps balle in
  let liste1 = ajoutForceCorde cordeNo1 balle liste in
  let liste2 = ajoutForceCorde cordeNo2 balle liste1 in
  nextFrame balle liste2;
  let(x,y) = balle.position in  
  let(cx,cy) = cordeNo1.origine in
  draw_ball (int_of_float cx) (int_of_float cy) 1;
  draw_props listeDeProps;
  
  draw_ball (int_of_float x) (int_of_float y) ( balle.taille);
  affichageCorde cordeNo1 balle;
  affichageCorde cordeNo2 balle;
  if (Graphics.key_pressed()) then
    (
      let c = Graphics.read_key () in
      if c = 'q' then
        sharedCut.isOver <- true
    );
  
  
(*  let _ = Unix.select [] [] [] 0.1 in*)wait 500000;
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


