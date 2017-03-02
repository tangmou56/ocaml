(*TANG Yuke*)

type 'a liste = vide | cons of  'a* 'a liste;;
type terme=po|pf|op of char|entier of int;;
type arbre=V|N of terme*arbre*arbre;;
let op2fun=function 
		op  `+`-> prefix +
		|op `-`->prefix -
		|op `*`->prefix *
		|op `/`->prefix /
		|_->failwith"impossible";; 

let lire_car =function nom ->
 let canal=open_in nom in
  let rec lirea =function  () -> 
         try let a = input_char canal in 
            cons (a ,lirea ())  with
       End_of_file -> begin close_in canal;vide end
      in    lirea ();;

let rec saut=function vide->vide 
			|cons(tete,reste)->if tete=` `||tete=`\n` then saut reste
					else cons(tete,(saut reste));;


let ch2term=function `(`->po
	|`)`->pf
	|(`+`|`-`|`*`|`/`)as ope ->op ope
	|v->entier ((int_of_char v)-48);;

let rec trans=fun f vide->vide
	|f (cons(tete,reste))->
			cons((f tete),(trans f reste));;

let l=trans ch2term (saut(lire_car "prefixe.txt"));;

let rec enleve=fun vide->failwith"impossible"
			|(cons(entier v,reste))->reste
			|(cons((op ope),reste))->enleve(enleve reste);;


let rec q3=fun vide ->failwith"impossible"
		|(cons(entier v,reste))->N(entier v,V,V)
		|(cons(op ope,reste))->N(op ope,(q3 reste),(q3 (enleve reste)));;
q3 l;;
