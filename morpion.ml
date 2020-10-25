(* Les types *)
type joueur = J1 | J2 ;;
type pion = Croix | Rond | Vide ;;
type grille = G of pion*pion*pion * pion*pion*pion * pion*pion*pion ;;
type coup = A1 | B1 | C1 | A2 | B2 | C2 | A3 | B3 | C3 ;;

(* Fonction pour passer du type pion au type char *)
let pion_to_char = function
      Croix -> 'X'
    | Rond -> 'O'
    | Vide -> ' ' ;;

(* Fonctions pour changer un pion de la grille *)
(* A1 *)
let setA1 = fun (G (_, b1, c1, a2, b2, c2, a3, b3, c3)) -> function
      Croix -> G (Croix, b1, c1, a2, b2, c2, a3, b3, c3)
    | Rond -> G (Rond, b1, c1, a2, b2, c2, a3, b3, c3)
    | Vide -> G (Vide, b1, c1, a2, b2, c2, a3, b3, c3) ;;

(* B1 *)
let setB1 = fun (G (a1, _, c1, a2, b2, c2, a3, b3, c3)) -> function
      Croix -> G (a1, Croix, c1, a2, b2, c2, a3, b3, c3)
    | Rond -> G (a1, Rond, c1, a2, b2, c2, a3, b3, c3)
    | Vide -> G (a1, Vide, c1, a2, b2, c2, a3, b3, c3) ;;

(* C1 *)
let setC1 = fun (G (a1, b1, _, a2, b2, c2, a3, b3, c3)) -> function
      Croix -> G (a1, b1, Croix, a2, b2, c2, a3, b3, c3)
    | Rond -> G (a1, b1, Rond, a2, b2, c2, a3, b3, c3)
    | Vide -> G (a1, b1, Vide, a2, b2, c2, a3, b3, c3) ;;

(* A2 *)
let setA2 = fun (G (a1, b1, c1, _, b2, c2, a3, b3, c3)) -> function
      Croix -> G (a1, b1, c1, Croix, b2, c2, a3, b3, c3)
    | Rond -> G (a1, b1, c1, Rond, b2, c2, a3, b3, c3)
    | Vide -> G (a1, b1, c1, Vide, b2, c2, a3, b3, c3) ;;

(* B2 *)
let setB2 = fun (G (a1, b1, c1, a2, _, c2, a3, b3, c3)) -> function
      Croix -> G (a1, b1, c1, a2, Croix, c2, a3, b3, c3)
    | Rond -> G (a1, b1, c1, a2, Rond, c2, a3, b3, c3)
    | Vide -> G (a1, b1, c1, a2, Vide, c2, a3, b3, c3) ;;

(* C2 *)
let setC2 = fun (G (a1, b1, c1, a2, b2, _, a3, b3, c3)) -> function
      Croix -> G (a1, b1, c1, a2, b2, Croix, a3, b3, c3)
    | Rond -> G (a1, b1, c1, a2, b2, Rond, a3, b3, c3)
    | Vide -> G (a1, b1, c1, a2, b2, Vide, a3, b3, c3) ;;

(* A3 *)
let setA3 = fun (G (a1, b1, c1, a2, b2, c2, _, b3, c3)) -> function
      Croix -> G (a1, b1, c1, a2, b2, c2, Croix, b3, c3)
    | Rond -> G (a1, b1, c1, a2, b2, c2, Rond, b3, c3)
    | Vide -> G (a1, b1, c1, a2, b2, c2, Vide, b3, c3) ;;
(* B3 *)
let setB3 = fun (G (a1, b1, c1, a2, b2, c2, a3, _, c3)) -> function
      Croix -> G (a1, b1, c1, a2, b2, c2, a3, Croix, c3)
    | Rond -> G (a1, b1, c1, a2, b2, c2, a3, Rond, c3)
    | Vide -> G (a1, b1, c1, a2, b2, c2, a3, Vide, c3) ;;
(* C3 *)
let setC3 = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, _)) -> function
      Croix -> G (a1, b1, c1, a2, b2, c2, a3, b3, Croix)
    | Rond -> G (a1, b1, c1, a2, b2, c2, a3, b3, Rond)
    | Vide -> G (a1, b1, c1, a2, b2, c2, a3, b3, Vide) ;;

(* Fonction qui initialise la grille *)
let init = fun () ->
    G ( Vide, Vide, Vide, Vide, Vide, Vide, Vide, Vide, Vide) ;;

(* Fonction qui affiche la grille *)
let afficheGrille = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) ->
    print_string "\n     A   B   C\n" ;
    print_string "   +---+---+---+\n" ;
    print_string " 1 | " ;
    print_char (pion_to_char a1) ;
    print_string " | " ;
    print_char (pion_to_char b1) ;
    print_string " | " ;
    print_char (pion_to_char c1) ;
    print_string " |\n" ;
    print_string "   +---+---+---+\n" ;
    print_string " 2 | " ;
    print_char (pion_to_char a2) ;
    print_string " | " ;
    print_char (pion_to_char b2) ;
    print_string " | " ;
    print_char (pion_to_char c2) ;
    print_string " |\n" ;
    print_string "   +---+---+---+\n" ;
    print_string " 3 | " ;
    print_char (pion_to_char a3) ;
    print_string " | " ;
    print_char (pion_to_char b3) ;
    print_string " | " ;
    print_char (pion_to_char c3) ;
    print_string " |\n" ;
    print_string "   +---+---+---+\n" ;;

(* Fonction qui affiche le gagnant *)
let msg = function
      Croix -> print_string " Victoire du joueur 1 !\n\n"
    | Rond -> print_string " Victoire du joueur 2 !\n\n"
    | Vide -> print_string " La partie continue...\n\n" ;;

(* Fonction qui verifie si un joueur a gagne *)
let gagne = fun (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) ->
    if (a1 = a2 && a2 = a3) then msg a3      (* Vertical *)
    else if (b1 = b2 && b2 = b3) then msg b3
    else if (c1 = c2 && c2 = c3) then msg c3
    else if (a1 = b1 && b1 = c1) then msg c1 (* Horizontal *)
    else if (a2 = b2 && b2 = c2) then msg c2
    else if (a3 = b3 && b3 = c3) then msg c3
    else if (a1 = b2 && b2 = c3) then msg c3 (* Diagonales *)
    else if (a3 = b2 && b2 = c1) then msg c1
    else msg Vide ;;

(* Fontion qui joue une croix *)
let joueur1 = fun gr -> match gr with (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) -> function
      A1 -> if a1 = Vide then let g = setA1 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | B1 -> if b1 = Vide then let g = setB1 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | C1 -> if c1 = Vide then let g = setC1 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | A2 -> if a2 = Vide then let g = setA2 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | B2 -> if b2 = Vide then let g = setB2 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | C2 -> if c2 = Vide then let g = setC2 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | A3 -> if a3 = Vide then let g = setA3 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | B3 -> if b3 = Vide then let g = setB3 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | C3 -> if c3 = Vide then let g = setC3 gr Croix in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case" ;;

(* Fonction qui joue un rond *)
let joueur2 = fun gr -> match gr with (G (a1, b1, c1, a2, b2, c2, a3, b3, c3)) -> function
      A1 -> if a1 = Vide then let g = setA1 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | B1 -> if b1 = Vide then let g = setB1 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | C1 -> if c1 = Vide then let g = setC1 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | A2 -> if a2 = Vide then let g = setA2 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | B2 -> if b2 = Vide then let g = setB2 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | C2 -> if c2 = Vide then let g = setC2 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | A3 -> if a3 = Vide then let g = setA3 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | B3 -> if b3 = Vide then let g = setB3 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case"
    | C3 -> if c3 = Vide then let g = setC3 gr Rond in afficheGrille g; gagne g; g
            else failwith "Cette case n'est pas vide, choisissez une autre case" ;;

(* Fontion de debut du jeu *)
let morpion = fun _ ->
    let g = init() in afficheGrille g; (g, J1) ;;

(* Fonction pour jouer un coup *)
let jouer = fun (g, j) c -> match j with
      J1 -> (joueur1 g c, J2)
    | J2 -> (joueur2 g c, J1) ;; 
