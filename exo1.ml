type liste=vide|cons of (int*string)*liste;;
type arbre=V|N of (int*string)*arbre*arbre;;

let tete=function cons(a,_)-> a|_ ->failwith "impossible";;
let suite=function  cons(_,a) ->a|_->failwith "impossible";;

let l=cons((2,"a"),cons((3,"c"),vide));;
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

let q4




