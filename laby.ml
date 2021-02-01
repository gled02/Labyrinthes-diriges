(* #load "graphics.cma";; *)
open Graphics ;;

Random.self_init ();;

let size = 800;;

let window = open_graph (" "^(string_of_int size)^"x"^(string_of_int size));;
set_window_title("Labyrinthes");;

open List;;

(* -------Macroconstantes------- *)
let cote_laby = 16;;
let cote_salle = size/cote_laby;;

(* -------Types------- *)
type coord = {x : int; y : int};;

type salle = {
	nord	: bool;
	sud		: bool;
	est 	: bool;
	ouest : bool
	};;

type chemin = coord list;;

type 'a quadtree = Vide | Feuille of 'a | Noeud of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree;;

type labyrinthe = {
	cote : int;
	lab : salle quadtree
	};;

(* -------Fonctions------- *)

let rec pow_of_2 = function
	| 1 -> true
	| n when (n mod 2 = 0) -> pow_of_2 (n/2)
	| _-> false;;

let g_Room () = {
	nord = Random.bool();
  sud = Random.bool();
	est = Random.bool();
	ouest = Random.bool()
};;

let g_Tree cote =
	if not(pow_of_2 cote) then failwith "Pas une puissance de 2"
	else
		let rec aux = function
			| 1 -> Feuille (g_Room())
			| n -> Noeud(aux (n/2), aux (n/2), aux (n/2), aux (n/2))
		in aux cote;;

let rec count = function
	| Feuille _ -> 1
	| Noeud(a, b, c, d) -> (count a) + (count b) + (count c) + (count d)
	|_-> failwith "Impossible";; 

let draw_room salle coor = 
		let width = cote_salle in
			let x = coor.x * width and y = coor.y * width in
		set_color black;
		draw_rect x y width width;
		if salle.sud then 
			(set_color (rgb 0 153 0); 
			 fill_rect (x + (width/4)) (y + 1) (width/2) 2)
		else
			(set_color (rgb 153 0 0); fill_rect (x + (width/4)) (y + 1) (width/2) 2);
  	if salle.ouest then
			(set_color (rgb 0 153 0); 
		  fill_rect (x + 1) (y + (width/4)) 2 (width/2))
		else
			(set_color (rgb 153 0 0); fill_rect (x + 1) (y + (width/4)) 2 (width/2));
		if salle.nord then 
			(set_color (rgb 0 153 0); 
		  fill_rect (x + (width/4)) (y + (width - 3)) (width/2) 2)
		else
			(set_color (rgb 153 0 0); fill_rect (x + (width/4)) (y + (width - 3)) (width/2) 2);
		if salle.est then
			(set_color (rgb 0 153 0); 
		  fill_rect (x + (width - 3)) (y + (width/4)) 2 (width/2))
		else
		  (set_color (rgb 153 0 0); fill_rect (x + (width - 3)) (y + (width/4)) 2 (width/2));
	({x = coor.x; y = coor.y},salle);;
	
let draw_laby laby coor  =
	if not(pow_of_2 (laby.cote)) then failwith "Pas une puissance de 2"
	else
		let rec aux coord acc c = function
			| Vide -> rev acc
			| Feuille x -> (draw_room x coord)::acc
			| Noeud(so, se, ne, no) -> let n = c/2 in 
																(aux {x = coord.x; y = coord.y} acc n so)@  
																(aux {x = coord.x + n; y = coord.y} acc n se)@                                  
																(aux {x = coord.x + n; y = coord.y + n} acc n ne)@                              
																(aux {x = coord.x; y = coord.y + n} acc n no)
	in aux coor [] (laby.cote) (laby.lab);;

let labin = {cote = cote_laby; lab = (g_Tree cote_laby)};;
let liste_lab = draw_laby labin {x = 0; y = 0};;

(* ------------------------------------------------------------------------------------------------------------------------- *)

let g_bool cote =
	if not(pow_of_2 cote) then failwith "Pas une puissance de 2"
	else
		let rec aux = function
			| 1 -> Feuille (false)
			| n -> Noeud(aux (n/2), 
									aux (n/2), 
									aux (n/2), 
									aux (n/2))
		in aux cote;;

let g_coor cote =
	let laby = g_bool cote in
	let rec aux coor n var = function
		| Vide -> failwith "Vide"
		| Feuille a -> Feuille (coor,a)
		| Noeud(so,se,ne,no) -> let v = (var/2) in	Noeud(
												aux coor (n/2) v so, 
												aux {x = coor.x + v; y = coor.y} (n/2) v se, 
												aux {x = coor.x + v; y = coor.y + v} (n/2) v ne, 
												aux {x = coor.x; y = coor.y + v} (n/2) v no)
	in aux {x = cote_salle/2; y = cote_salle/2} cote size laby;;

let test coor = {x = (coor.x - (cote_salle/2))  / cote_salle; y = (coor.y - (cote_salle/2))  / cote_salle};;

let rec cherche coor liste = 
	match liste with
		| a::l when (fst a).x = (test coor).x && (fst a).y = (test coor).y  -> (snd a)
		| _::l -> cherche coor l
		| _ -> failwith "Impossible de trouver les coordonnees";;

let update coor_current laby = 
	let rec aux n = function
		| Vide -> failwith "Vide"
		| Feuille a -> if (fst a) = coor_current then Feuille (coor_current,true) else Feuille a
		| Noeud(so, se, ne, no) -> Noeud(aux (n/2) so, aux (n/2) se, aux (n/2) ne, aux (n/2) no)
	in aux cote_laby laby;;

let rec visited coor_current laby = 
	let rec aux n = function
		| Vide -> failwith "Vide"
		| Feuille a when (fst a) = coor_current && (snd a) = true -> true
		| Feuille a -> false
		| Noeud(so, se, ne, no) -> (aux (n/2) so) || (aux (n/2) se) || (aux (n/2) ne) || (aux (n/2) no)
	in aux cote_laby laby;;

let goal coor = 
	let salle = cherche coor liste_lab in
	if (coor.x < cote_salle && salle.ouest) || (coor.x >= size - cote_salle && salle.est) || (coor.y < cote_salle && salle.sud) || (coor.y >= size - cote_salle && salle.nord) then true
	else false;;

let rec neighbours coor acc = function
	| a when a.ouest -> neighbours coor ({x = coor.x - cote_salle; y = coor.y}::acc) {nord = a.nord; sud = a.sud; est = a.est; ouest = false}
	| a when a.est -> neighbours coor ({x = coor.x + cote_salle; y = coor.y}::acc) {nord = a.nord; sud = a.sud; est = false; ouest = a.ouest}
	| a when a.nord-> neighbours coor ({x = coor.x; y = coor.y + cote_salle}::acc) {nord = false; sud = a.sud; est = a.est; ouest = a.ouest}
	| a when a.sud -> neighbours coor ({x = coor.x; y = coor.y - cote_salle}::acc) {nord = a.nord; sud = false; est = a.est; ouest = a.ouest}
	| a -> rev acc;;

let path coors f =
	let rec aux coor a n tree liste = 
		if goal coor then (liste:chemin) else
		match n with
		| [] -> ([]:chemin)
		| x::[] -> let feuille = (cherche x liste_lab) in
								if visited x tree then [] else
									let acc = (aux x feuille (neighbours x [] feuille) (update x tree) (x::liste)) in
									if acc = [] then (aux coor a (neighbours coor [] a) (update x tree) liste) else acc
								
		| x::y::[] -> let feuille1 = (cherche x liste_lab) in 
									let feuille2 = (cherche y liste_lab) in
									if visited x tree then 
										if visited y tree then [] else
											let acc2 = (aux y feuille2 (neighbours y [] feuille2) (update y tree) (y::liste)) in
											if acc2 = [] then (aux coor a (neighbours coor [] a) (update y tree) liste) else acc2
									else
										let acc = (aux x feuille1 (neighbours x [] feuille1) (update x tree) (x::liste)) in
										if acc = [] && not (visited y tree) then (aux y feuille2 (neighbours y [] feuille2) (update y tree) (y::liste)) else acc
								
		| x::y::z::[] -> let feuille1 = (cherche x liste_lab) in
										 let feuille3 = (cherche z liste_lab) in
										 let feuille2 = (cherche y liste_lab) in
										 if visited x tree then 
										 	if visited y tree then 
												if visited z tree then [] else
													let acc3 = (aux z feuille3 (neighbours z [] feuille3) (update z tree) (z::liste)) in
													if acc3 = [] then (aux coor a (neighbours coor [] a) (update z tree) liste) else acc3
											else 
												let acc2 = (aux y feuille2 (neighbours y [] feuille2) (update y tree) (y::liste)) in
												if acc2 = [] && not (visited z tree) then (aux z feuille3 (neighbours z [] feuille3) (update z tree) (z::liste)) else acc2
										 else
												let acc =	(aux x feuille1 (neighbours x [] feuille1) (update x tree) (x::liste)) in
												if acc = [] && not (visited y tree) then (aux y feuille2 (neighbours y [] feuille2) (update y tree) (y::liste)) else acc
											
		| x::y::z::t::[] -> let feuille1 = (cherche x liste_lab) in
												let feuille4 = (cherche t liste_lab) in
												let feuille2 = (cherche y liste_lab) in
												let feuille3 = (cherche z liste_lab) in 
												if visited x tree then 
													if visited y tree then 
														if visited z tree then 
															if visited t tree then [] else
																let acc4 = (aux t feuille4 (neighbours t [] feuille4) (update t tree) (t::liste)) in
																if acc4 = [] then (aux coor a (neighbours coor [] a) (update t tree) liste) else acc4
														else
															let acc3 = (aux z feuille3 (neighbours z [] feuille3) (update z tree) (z::liste)) in
															if acc3 = [] && not (visited t tree) then (aux t feuille4 (neighbours t [] feuille4) (update t tree) (t::liste)) else acc3
													else
														let acc2 = (aux y feuille2 (neighbours y [] feuille2) (update y tree) (y::liste)) in
														if acc2 = [] && not (visited z tree) then (aux z feuille3 (neighbours z [] feuille3) (update z tree) (z::liste)) else acc2
												else
													let acc =	(aux x feuille1 (neighbours x [] feuille1) (update x tree) (x::liste)) in
													if acc = [] && not (visited y tree) then (aux y feuille2 (neighbours y [] feuille2) (update y tree) (y::liste)) else acc
													
		| _ -> failwith "Probleme"
	in aux coors f (neighbours coors [] f) (update coors (g_coor cote_laby)) [coors];;

(* ------------------------------------------------------------------------------------------------------------------------- *)

let perso coor r c =
		set_color c;
		fill_circle coor.x coor.y r;;

let delete (coor,r) = 
		set_color white;
		fill_circle coor.x coor.y r;;

let rec draw_path c liste = 
	set_color c;
	match liste with
	| a::b::ll when a.x > b.x && a.y = b.y ->  
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y + (cote_salle / 10)|];
                 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y - (cote_salle / 10)|];
	               draw_path c (b::ll)
  | a::b::ll when a.x < b.x && a.y = b.y -> 
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x - (cote_salle / 10), a.y + (cote_salle / 10), a.x, a.y|];
                 draw_segments[|a.x - (cote_salle / 10), a.y - (cote_salle / 10), a.x, a.y|];
	               draw_path c (b::ll)
  | a::b::ll when a.x = b.x && a.y < b.y -> 
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x - (cote_salle / 10), a.y -(cote_salle / 10), a.x, a.y|];
                 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y - (cote_salle / 10)|];
	               draw_path c (b::ll)
  | a::b::ll when a.x = b.x && a.y > b.y ->  
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x - (cote_salle / 10), a.y + (cote_salle / 10), a.x, a.y|];
								 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y + (cote_salle / 10)|];
	               draw_path c (b::ll)
	| a::[] ->    draw_segments[|a.x - (cote_salle / 10), a.y + (cote_salle / 10), a.x + (cote_salle / 10), a.y - (cote_salle / 10)|];
								draw_segments[|a.x - (cote_salle / 10), a.y - (cote_salle / 10), a.x + (cote_salle / 10), a.y + (cote_salle / 10)|];

	| _ -> ();;

let draw_indice c liste = 
	let rec aux n l = 
		set_color c;
		match l with
		| a::b::ll when n > 0 && a.x > b.x && a.y = b.y ->  
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y + (cote_salle / 10)|];
                 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y - (cote_salle / 10)|];
	               aux (n - 1) (b::ll)
  	| a::b::ll when n > 0 && a.x < b.x && a.y = b.y -> 
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x - (cote_salle / 10), a.y + (cote_salle / 10), a.x, a.y|];
                 draw_segments[|a.x - (cote_salle / 10), a.y - (cote_salle / 10), a.x, a.y|];
	               aux (n - 1) (b::ll)
 	 | a::b::ll when n > 0 && a.x = b.x && a.y < b.y -> 
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x - (cote_salle / 10), a.y -(cote_salle / 10), a.x, a.y|];
                 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y - (cote_salle / 10)|];
	               aux (n - 1) (b::ll)
  	| a::b::ll when n > 0 && a.x = b.x && a.y > b.y ->  
		             draw_segments[|a.x, a.y, b.x, b.y|];
								 draw_segments[|a.x - (cote_salle / 10), a.y + (cote_salle / 10), a.x, a.y|];
								 draw_segments[|a.x, a.y, a.x + (cote_salle / 10), a.y + (cote_salle / 10)|];
	               aux (n - 1) (b::ll)
		| a::b::l when n = 0 ->
								 draw_segments[|a.x - (cote_salle / 10), a.y + (cote_salle / 10), a.x + (cote_salle / 10), a.y - (cote_salle / 10)|];
							 	 draw_segments[|a.x - (cote_salle / 10), a.y - (cote_salle / 10), a.x + (cote_salle / 10), a.y + (cote_salle / 10)|];
		| _ -> ()
	in aux 4 (rev liste);;

let rec clean_list = function
	| [] -> ()
	| x::l-> clean_list l;;

let bouton coor cote =
	let salle = cherche coor liste_lab and l_path = (path coor (cherche coor liste_lab)) in
		match read_key() with
		| 'p' -> close_graph();coor
		| 'z' when coor.y + cote < size  && salle.nord ->		
			                                    clear_graph();
																					clean_list (draw_laby labin {x = 0; y = 0});
																					{x = coor.x; y = coor.y + cote}
																					
		| 's' when coor.y - cote > 0 && salle.sud -> 			
			                                    clear_graph();
																					clean_list (draw_laby labin {x = 0; y = 0});
																				  {x = coor.x; y = coor.y - cote}
																					
		| 'q' when coor.x - cote > 0 && salle.ouest ->				
			                                    clear_graph();
																					clean_list (draw_laby labin {x = 0; y = 0});
																				  {x = coor.x - cote; y = coor.y}
																					
		| 'd' when coor.x + cote < size && salle.est ->  	
			                                    clear_graph();
																					clean_list (draw_laby labin {x = 0; y = 0});
																					{x = coor.x + cote; y = coor.y}
		| 'a' when l_path <> [] -> draw_path red (rev l_path); {x = coor.x; y = coor.y}
		| 'i' when l_path <> [] -> draw_indice red l_path; {x = coor.x; y = coor.y}
		| _-> coor;;

let rec move jou cote liste laby =
	let coor = (fst jou) and r = (snd jou) in
	let salle = cherche {x = coor.x; y = coor.y} liste in
	let l_path = (path coor (cherche coor liste_lab)) in
	if l_path = [] then 
		(delete jou; 
		perso coor r (rgb 128 128 128);
	  move ((bouton coor cote),r) cote liste laby;
	 )
  else
		perso {x = coor.x ; y = coor.y} r blue;
	if (coor.y + cote >= size && salle.nord) || (coor.y - cote <= 0 && salle.sud) || (coor.x + cote >= size && salle.est) || (coor.x - cote <= 0 && salle.ouest) then
		(delete jou; 
		perso coor r green;
		match read_key() with
		| 'p' -> close_graph()
		| 'e' -> close_graph()
		| _-> move ({x = coor.x; y = coor.y}, r) cote liste laby
		)
	else 
		let coors = bouton coor cote in
		      delete jou;
	     	  perso coors r blue;
		      move (coors,r) cote liste laby;;

(* ------------------------------------------------------------------------------------------------------------------------- *)

let r = cote_salle/2 - 8;;
let length = cote_salle/2 * (cote_laby - 1);;
move ({x = length; y = length}, r) cote_salle liste_lab labin;;