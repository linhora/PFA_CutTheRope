open Graphics;;

let left = 0;;
let right = 300;;
let down = 0;;
let up = 500;;

let paddle = 100;;
let thick = 4;;

let ball = 15;;
let mass = 1;;

open_graph " 900x700"
;;


Graphics.auto_synchronize false
;;



let draw_ball x y =
	fill_circle x y ball
;;


let calc_acceleration =
	(0,-mass)
;;



let new_speed (vx,vy) (ax,ay) =
	if(vy > -15)
		then (vx+ax,vy+ay)
	else
		(vx+ax,vy)
;;

let new_position (bx,by) (vx,vy) =
	(bx+vx,by+vy)
;;


let detect_bounce (bx,by) (vx,vy) =
	if by-15/2 < 1
		then (vx,-vy)
	else
		(vx,vy)
;;



let rec wait n = 
	if n=0 then Graphics.synchronize () else wait (n-1)
;;


let loose y =
	if y < 0
		then exit 0
;;

let rec game (x,y) (vx,vy) =
	Graphics.clear_graph ();
	draw_ball x y;
	
	
	
	
	Printf.printf "x: %d y: %d  p: %d vy: %d \n" x y vx vy;
	
	
	
	
	
	(*loose y;*)
	
	wait 7000000;
	
	game (new_position (x,y) (detect_bounce (x,y) (new_speed (vx,vy) calc_acceleration))) (detect_bounce (x,y) (new_speed (vx,vy) calc_acceleration))
;;

game (100,400) (0,0);;

