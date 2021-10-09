(* TYP *)
(* Leaf reprezentuje pusta kolejke *)
(* Node zawiera informacje odnosnie jego wartosci, jego lewego i prawego
poddrzewa oraz prawej wysokosci drzewa *)
type 'a queue = 
  | Node of 'a * 'a queue * 'a queue * int
  | Leaf

(* DODATKOWA FUNKCJA *)
(* zwraca prawa wysokosc drzewa q *)
(* gdy drzewo to Leaf, jego prawa wysokosc jest rowna -1 *)
let height q =
  match q with
  | Leaf -> -1
  | Node (_, _, _, wys) -> wys

(* funkcja tworzaca puste drzewo *)
let empty = Leaf

(*DEKLARACJA WYJATKU *)
exception Empty

(* dodatkowy wyjatek wychwytujacy ewentualny blad w dzialaniu funkcji join *)
exception WA

(* funkcja laczaca dwie kolejki q1 i q2 w jedna *)
(* polaczenie dwoch Leafow zwraca Leaf *)
(* polaczenie Leafa i niepustej kolejki zwraca wlasnie ta kolejke *)
(* przy laczeniu dwoch niepustych kolejek korzen tworzy sie nowa niepusta kolejka *)
(* zmienne na, nl, nr, nh okreslaja kolejke bedaca jednym z poddrzew nowej kolejki *)
let rec join q1 q2 =
  match q1, q2 with
  | Leaf, queue -> queue
  | queue, Leaf -> queue
  | Node (a, l1, r1, h1), Node (b, l2, r2, h2) ->
    if a >= b then let new_q = join r2 q1 in
      match new_q with
      | Leaf -> raise WA
      | Node (na, nl, nr, nh) ->
        if nh >= height l2 then Node (b, new_q, l2, height l2 + 1)
        else Node (b, l2, new_q, nh + 1)
    else join q2 q1

(* funkcja dodajaca element a do kolejki q *)
let add a q = join (Node (a, Leaf, Leaf, 0)) q

(* funckja usuwajaca najmniejszy element w kolejce q i laczaca jej poddrzewa *)
(* gdy kolejka jest Leafem, podnosi wyjatek *)
let delete_min q =
  match q with
  | Leaf -> raise Empty
  | Node (a, pl, pp, _) -> (a, join pl pp)

(* funkcja sprawdzajaca czy kolejka jest pusta *)
let is_empty q = q = Leaf

(* TESTY *)
(* let a = empty;;
assert (is_empty a = true);;

let b = add 1 empty;;
assert (is_empty b = false);;

let c = join a b;;
assert (c = Node (1, Leaf, Leaf, 0));;

assert (is_empty c = false);;

let d = join c (add 8 empty);;
assert (d = Node (1, Node (8, Leaf, Leaf, 0), Leaf, 0));;

let e = join (join (add 1000 empty) (add 102 empty)) (add 6 empty);;
assert (e = Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0));;

let f = join e d;;
assert (f = Node (1,
   Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
   Node (8, Leaf, Leaf, 0), 1));;

let par1 = try delete_min f with Empty -> (-1, Leaf);;
assert (par1 = (1,
   Node (6, Node (8, Leaf, Leaf, 0),
    Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1)));;

let g = join f f;;
assert (g = Node (1,
   Node (1, Node (8, Node (8, Leaf, Leaf, 0), Leaf, 0),
    Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0), 1),
   Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0), 1));;

assert (is_empty g = false);;

let h = join (join (join e b) (join f d)) (join (add 10101010 g) (join c f));;
assert (h = Node (1,
   Node (1,
    Node (1,
     Node (1,
      Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
      Node (8, Leaf, Leaf, 0), 1),
     Node (8, Leaf, Leaf, 0), 1),
    Node (1, Node (8, Leaf, Leaf, 0),
     Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0), 1),
    2),
   Node (1,
    Node (1,
     Node (6,
      Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
      Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
     Node (8, Node (8, Leaf, Leaf, 0), Leaf, 0), 1),
    Node (6, Node (10101010, Leaf, Leaf, 0),
     Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
    2),
   3));;

assert (is_empty h = false);;

let par2 = try delete_min h with Empty -> (-1, Leaf);;
assert (par2 = (1,
   Node (1,
    Node (1,
     Node (1,
      Node (6,
       Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0),
        Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
       Node (10101010, Leaf, Leaf, 0), 1),
      Node (8, Leaf, Leaf, 0), 1),
     Node (1,
      Node (1,
       Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
       Node (8, Leaf, Leaf, 0), 1),
      Node (8, Leaf, Leaf, 0), 1),
     2),
    Node (1,
     Node (6,
      Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
      Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
     Node (8, Node (8, Leaf, Leaf, 0), Leaf, 0), 1),
    2)));;

let par3 = try delete_min a with Empty -> (-1, Leaf);;
assert (par3 = (-1, Leaf));;

let i = join (join h g) (join d g);;
assert (i =  Node (1,
   Node (1,
    Node (1,
     Node (1,
      Node (6,
       Node (6, Node (10101010, Leaf, Leaf, 0),
        Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
       Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
      Node (1,
       Node (6,
        Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
        Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
       Node (8, Node (8, Leaf, Leaf, 0), Leaf, 0), 1),
      2),
     Node (1,
      Node (1,
       Node (1,
        Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
        Node (8, Leaf, Leaf, 0), 1),
       Node (8, Leaf, Leaf, 0), 1),
      Node (1, Node (8, Leaf, Leaf, 0),
       Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0), 1),
      2),
     3),
    Node (1,
     Node (1,
      Node (6,
       Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
       Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), 1),
      Node (8, Node (8, Leaf, Leaf, 0), Leaf, 0), 1),
     Node (8, Node (8, Leaf, Leaf, 0), Leaf, 0), 1),
    2),
   Node (1,
    Node (6, Node (102, Node (1000, Leaf, Leaf, 0), Leaf, 0), Leaf, 0),
    Node (8, Leaf, Leaf, 0), 1),
   2));;

assert (is_empty i = false);; *)