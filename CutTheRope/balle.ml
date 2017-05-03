
type balle = {mutable position : float*float;mutable vitesse : float*float;masse : float; taille : float};; 
  
let evalForces balle listForces = 
  begin
    let rec evaluation balle = function 
      |[] -> []
      |f :: [] -> f balle :: []
      |f :: l -> f balle :: evaluation balle l
    in
    evaluation balle listForces 
  end 
let sommeForceX listeForces balle = 
  begin
    let rec sommeX = function 
      |[]->0.0
      |(x,_)::l->x +. sommeX l
    in
    sommeX (evalForces balle listeForces)
  end
    
let sommeForceY listeForces balle = 
  begin
    let rec sommeY = function
      |[]->0.0
      |(_,y)::l->y+.sommeY l
    in
    sommeY (evalForces balle listeForces)
  end
    
let nextFrame balle listForces=
  begin
    let acceleration = ((sommeForceX listForces balle) /. balle.masse,(sommeForceY listForces balle) /. balle.masse) in
    let (ax,ay) = acceleration in
    let (vx,vy) = balle.vitesse in
    let (x,y) = balle.position in
    balle.vitesse <- (vx+.ax,vy+.ay);
    balle.position <- (x+.vx,y+.vy);
  end

