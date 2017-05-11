open Graphics;;

let left = 0;;
let right = 300;;
let down = 0;;
let up = 500;;

let paddle = 100;;
let thick = 4;;

let ball = 30;;
let mass = 0.01;;
let bounce= 0.80;;
let maxVelocity= 15.0;;

open_graph " 900x800"
;;


Graphics.auto_synchronize false
;;



let draw_ball x y =
	fill_circle x y ball
;;


let calc_acceleration =
	(0.0,-.mass)
;;



let new_speed (vx,vy) (ax,ay) =
	if(vy > -.maxVelocity)
		then (vx+.ax,vy+.ay)
	else
		(vx+.ax,vy+.ay)
;;

let new_position (bx,by) (vx,vy) =
	(bx+.vx,by+.vy)
;;

(*
let detect_bounce (bx,by) (vx,vy) =
	if by -. float_of_int ball/.2.0 < 1.0
		then  (vx,(vy*. -.0.8))
	else
		(vx,vy)
;;
*)

let abs_f x =
	if x<0.0
		then -.x
	else x

let detect_bounce (bx,by) (vx,vy) =
	if by -. float_of_int ball <= 0.0
		then  
		((bx+.(vx*.(abs_float (by-.(float_of_int ball))/.vy))
		,(float_of_int ball)+.((vy*. -.bounce)*.(abs_float (by-.(float_of_int ball))/.vy))),(vx,(vy*. -.bounce)))
	else
	((bx,by),(vx,vy))
;;


let rec wait n = 
	if n=0 then Graphics.synchronize () else wait (n-1)
;;


let loose y =
	if y < 0
		then exit 0
;;

let rec game ((x,y),(vx,vy)) =
	Graphics.clear_graph ();
	draw_ball (int_of_float x) (int_of_float y);
	
	
	
	(*loose y;*)
	
	wait 800000;
	
	Printf.printf "x: %f y: %f  p: %f vy: %f \n" x y vx vy;
	
	
	game (detect_bounce(new_position (x,y) (new_speed (vx, vy) calc_acceleration)) ((new_speed (vx,vy) calc_acceleration)))
	
	(*	
	game (new_position (x,y) (detect_bounce (x,y) (new_speed (vx, vy) calc_acceleration))) (detect_bounce (x,y) (new_speed (vx,vy) calc_acceleration))
	*)
;;



game ((100.0,600.0),(0.0,0.0));;

