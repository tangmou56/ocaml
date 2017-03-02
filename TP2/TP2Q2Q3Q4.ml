(*Tang YUKE*)

type 'a liste = vide | cons of  'a* 'a liste;;
let rec lignes = function canal ->
          try let une_ligne = input_line canal in
                cons(une_ligne, lignes canal)
          with End_of_file -> vide;;
let lire = function nom ->
       let canal = open_in nom in
          let resu = lignes canal in
               close_in canal; resu;;
let l_st=lire "donnees2.dat";;
let tete=function cons(a,_)-> a|_ ->failwith "impossible";;
let suite=function  cons(_,a) ->a|_->failwith "impossible";;
let rec q2=function l -> if l <> vide then 
cons((tete l,int_of_string (tete(suite l))),q2 (suite (suite l)))
else vide;;
let re=q2 l_st;;

let rec q3=fun l nom-> if l=vide then 0
		else if nom=(fst(tete l)) then (snd(tete l))+(q3 (suite l) nom)
		else (q3 (suite l) nom);;

let q3f=fun nomfic nom ->q3 (q2 (lire nomfic)) nom;;

q3f "donnees2.dat" "a";;

let max=fun a b->if a>b then a else b;;

let rec q4=fun l->if l=vide then 0 
		else max (snd(tete l)) (q4(suite l));; 

