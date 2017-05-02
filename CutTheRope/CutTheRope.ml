open Graphics;; 


let ball = 30;;
let mass = 0.005;;
let bounce= 0.7;;
let maxVelocity= 15.0;;

open_graph " 900x800"
;;

Graphics.auto_synchronize false
;;

type balle = {mutable position : float*float;mutable vitesse : float*float;masse : float; taille : float};; 

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

let calc_acceleration =
	(0.0,-.mass)

let detect_bounce (bx,by) (vx,vy) =
	if by -. float_of_int ball <= 0.0
		then  ((bx+.(vx*.(abs_float (by-.(float_of_int ball))/.vy)),(float_of_int ball)+.((vy*. -.bounce)*.(abs_float (by-.(float_of_int ball))/.vy))),(vx,(vy*. -.bounce)))
	else
	((bx,by),(vx,vy))
;;

(*#####################################*)

let rec wait n = 
	if n=0 then Graphics.synchronize () else wait (n-1)
;;


let rec game balle =

	Graphics.clear_graph ();
	let(x,y) = balle.position in 
	let(vx,vy) = balle.vitesse in 
	draw_ball (int_of_float x) (int_of_float y) (int_of_float balle.taille);
	
	
	wait 400000;
	
	
	nextFrame balle [fun balle -> (0.0,-.0.001)];
	game balle
	
;;

game {position=(100.0,100.0);vitesse=(0.0,-0.05);masse=1.0;taille=10.0};;
