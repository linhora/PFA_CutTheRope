open Graphics;;
open Unix;;  
  

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

let calculForceCorde corde balle forcesActuelles =
  if corde.state = 1 then
    (
      (*let forcesCalcules = evalForces balle forcesActuelles in
      let (fx,fy) = (sommeForceX forcesCalcules, sommeForceY forcesCalcules) in
      let (bx,by) = balle.position in
      let (cx,cy) = corde.origine in
      let coteAdj = (bx-.cx) in
      let cosAngle = coteAdj/.corde.longueur in
      let angle = acos cosAngle in
      let sinAngle = sin angle in
      let projectionLocalX = (fx/.(cos angle)+.fy/.(sin angle)) in
     (* fun balle -> (-.projectionLocalX/.(cos angle),-.projectionLocalX/.(sin angle)))*)
  fun balle -> ((((sinAngle)*.(snd balle.vitesse))+.((cosAngle)*.(fst balle.vitesse))-.(cosAngle)*.projectionLocalX),(-.sinAngle*.((snd balle.vitesse)+.(fst balle.vitesse)-.projectionLocalX))))*)
      let (bx,by) = balle.position in
      let (cx,cy) = corde.origine in
      let (dx,dy)=(cx-.bx,cy-.by) in
      let r = corde.longueur in
      let (vx,vy)= balle.vitesse in
      let cLen=sqrt ((cx-.bx)**2.+.(cy-.by)**2.) in
      let k = 0.5 in(*
      balle.position<-((fst balle.position)+.1.*.dx/.r,(snd balle.position)+.1.*.dy/.r);*)
      fun balle -> (((dx/.r)*.(r-.cLen)*.(-.k))-.k*.1.5*.vx,((dy/.r)*.(r-.cLen)*.(-.k))-.k*.1.5*.vy)
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


let bob= {id = 1 ; pos = (200.0,10.0) ; contact =  (fun balle (x,y) -> (((snd balle.position)-.30.0<y) && (((fst balle.position)> x-.50.0)&&((fst balle.position)<x+.50.0))))  ; force = (fun balle -> (balle.position <- (fst balle.position,(snd balle.position)+.(-.snd balle.vitesse*.2.))) ;(0.0,(-.2.0)*.(snd balle.vitesse))); draw = (fun (x,y)-> (draw_rect ((int_of_float x)-50) (int_of_float y) 100 2))};;

let gravite= {id = 0 ; pos = (-.1.0,-.1.0) ; contact = (fun balle (x,y) ->true) ; force = (fun balle -> (0.0,-.0.005)); draw = (fun (x,y)-> ())};;

let listeDeProps = [gravite];;


let cordeNo1 = {origine = (200.0,200.0); longueur = 85.0; state = 0};;

let rec game balle =

  Graphics.clear_graph ();
  checkCordeState cordeNo1 balle;
  if(isCordeTendue balle cordeNo1)
  then
    nextFrame balle ((calculForceCorde cordeNo1 balle (get_list_force listeDeProps balle))::(get_list_force listeDeProps balle)  )
  else 
    nextFrame balle (get_list_force listeDeProps balle);
  
  let(x,y) = balle.position in 
  let(vx,vy) = balle.vitesse in 
  let(cx,cy) = cordeNo1.origine in
  draw_ball (int_of_float cx) (int_of_float cy) 1;
  draw_props listeDeProps;
  
  draw_ball (int_of_float x) (int_of_float y) ( balle.taille);
  affichageCorde cordeNo1 balle;
  
  
  wait 200000;
  
  
  (*Printf.printf "\n BEF y: %f vy : %f  \n" y vy ;*)
  
  
  
  game balle
       
;;


game {position=(150.0,300.0);vitesse=(0.0,-0.05);masse=1.0;taille=ball};;


