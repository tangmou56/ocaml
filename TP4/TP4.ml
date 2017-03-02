
(*TANG YUKE*)
type 'a liste = vide | cons of  'a* 'a liste;;
let lire_car =function nom ->
 let canal=open_in nom in
  let rec lirea =function  () -> 
         try let a = input_char canal in 
            cons (a ,lirea ())  with
       End_of_file -> begin close_in canal;vide end
      in    lirea ();;
let l_st=lire_car "Libe.txt";;

let tete=function cons(a,_)-> a|_ ->failwith "impossible";;
let suite=function  cons(_,a) ->a|_->failwith "impossible";;

let is_sep = function ` `| `\n` -> true
                     | _ -> false;;


let rec saute_mot = function vide -> vide
        | cons(car,reste) as l -> if is_sep car then l
                  else saute_mot reste;;

let char2str = function car -> make_string 1 car;;

let rec  premier_mot = function vide -> ""
     | cons(car,reste) -> if is_sep car then ""
                else char2str car ^  premier_mot reste;;

let rec analyselex = function vide->vide
  |cons (car,reste) as l -> if is_sep car then
            analyselex reste
    else cons(premier_mot l , analyselex (saute_mot l));;


let tout= function nom -> analyselex(lire_car nom);;


let l=analyselex l_st;;

let max=fun a b->if a > b then a else b;;

let rec cnb=fun l mot->if l=vide then 0 
			else if mot=(tete l) then 1+cnb(suite l) mot
			else 0+cnb(suite l) mot;;

let rec consl=fun l->if l=vide then vide 
		else cons((cnb l (tete l),tete l),consl (suite l));;

let rec supmotb=fun l mot->if mot=snd(tete l) then supmotb (suite l) mot
			else cons((tete l),supmotb (suite l) mot);;
let rec supmot=fun l mot->if mot =snd(tete l) then cons((tete l),(supmotb (suite l) mot))
			else cons(tete l,supmot(suite l) mot);;
let l=consl l;;


(*TP4*)
type arbre=V|N of (int*string)*arbre*arbre;;

let tete=function cons(a,_)-> a|_ ->failwith "impossible";;
let suite=function  cons(_,a) ->a|_->failwith "impossible";;


let a=N((1,"b"),(N((2,"a"),V,V)),(N((3,"d"),V,V)));;

let rec q1l=fun l->if l=vide then 0 else
		fst(tete l)+q1l(suite l);;
q1l l;;

let rec q1a=fun V->failwith"erreur"
		|(N(r,V,V))->fst r
		|(N(r,ag,V))->(fst r)+(q1a ag)
		|(N(r,V,ad))->(fst r)+(q1a ad)
		|(N(r,ag,ad))->(fst r)+(q1a ag)+(q1a ad);;
q1a a;;

let rec q2r1=fun l x->cons(x,l);;		
q2r1 l (5,"c");;

let rec q2r2=fun l x->if l = vide then cons(x,vide) 
			else if snd(tete l)<(snd x)then cons((tete l),(q2r2(suite l) x))
			else cons(x,l);;
q2r2 l (5,"b");;

let rec q2r3=fun V x->failwith"erreur"
		|(N(r,V,V)) x->if (snd x)>(snd r) then N(r,V,(N(x,V,V))) else  N(r,(N(x,V,V)),V)
		|(N(r,ag,V)) x-> if (snd x)<(snd r) then N(r,(q2r3 ag x),V) else N(r,ag,(N(x,V,V)))
		|(N(r,V,ad)) x->if (snd x)<(snd r) then N(r,(N(x,V,V)),ad) else N(r,V,(q2r3 ad x))
		|(N(r,ag,ad)) x->if (snd x)<(snd r) then N(r,(q2r3 ag x),ad) else N(r,ag,(q2r3 ad x));;
q2r3 a	(5,"c");;	

let rec q3a=fun V res ->res
		|(N(r,ag,ad)) res ->q3a ag (cons(r,(q3a ad res)));;
let q3=fun a ->q3a a vide;;
q3 a;;

let max=fun a b->if a>b then a else b;;
let rec q4a=fun V->failwith"erreur"
		|(N(r,V,V))->fst r
		|(N(r,ag,V))->max (fst r) (q4a ag)
		|(N(r,V,ad))->max (fst r) (q4a ad)
		|(N(r,ag,ad))->max (q4a ad) (max (fst r) (q4a ag));;
let rec q4b=fun l plus->
if fst(tete l)=plus then snd (tete l) else q4b (suite l) plus;; 
let q4=function a->q4b (q3 a) (q4a a);;
q4 a;;
