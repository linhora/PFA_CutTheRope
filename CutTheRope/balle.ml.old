class balle (x,y) (vx,vy)=
object(self)
  val mutable position = (x,y)
  val mutable vitesse = (vx,vy)
  val mutable listForces = []
  val taille = 15.0
  val masse = 1.0
  method position = position
  method vitesse = vitesse
  method taille = taille
  method masse = masse
  method updateForces lF = listForces <- lF
  method nextFrame =
    begin
      let acceleration = ((self#sommeForceX)/.masse,(self#sommeForceY)/.masse) in
      let (ax,ay) = acceleration in
      let (vx,vy) = vitesse in
      let (x,y) = position in
      vitesse <- (vx+.ax,vy+.ay);
      position <- (x+.vx,y+.vy);
    end
  method private sommeForceX = 
    begin
      let rec sommeX = function 
        |[]->0.0
        |(x,_)::l->x +. sommeX l
      in
      sommeX self#evalForces
    end
  method private sommeForceY = 
    begin
      let rec sommeY = function
        |[]->0.0
        |(_,y)::l->y+.sommeY l
      in
      sommeY self#evalForces
    end
  method private evalForces = 
    begin
      let rec evaluation = function 
        |[] -> []
        |f :: [] -> f self :: []
        |f :: l -> f self :: evaluation l
      in
      evaluation listForces 
    end                                   
end;;

(*let baballe = new balle (100.0,100.0) (0.0,0.0);;
baballe#updateForces [fun o->(0.0,-1.0)];;

baballe#nextFrame;;

match baballe#position with
(x,y)-> Printf.printf "%f %f" x y;;*)
