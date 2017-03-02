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


let l=trans ch2term (saut(lire_car "exprinv.txt"));;



let reduire=fun ch (cons(entier v2,cons(entier v1,reste)))->
cons(entier ((op2fun ch) v1 v2),reste)
		|_ _ -> failwith"impossible";;

let rec q1r=fun vide (cons(a,vide))->a
		|(cons(entier v,reste)) pile->q1r reste (cons(entier v,pile))
		|(cons(op ope,reste)) pile->q1r reste (reduire (op ope) pile)
		|_ _->failwith"impossible";;


let q1=fun nom->q1r (trans ch2term (saut(lire_car nom))) vide;;



q1 "exprinv.txt";;


let reduireq2=fun ch (cons(n1,cons(n2,reste)))->

			cons(N(ch,n2,n1),reste)

			|_ _ -> failwith"impossible";;



let rec q2r=fun vide (cons(a,vide))->a

		|(cons((entier v),reste)) pile->q2r reste (cons((N(entier v,V,V)),pile))

		|(cons((op ope),reste)) pile-> q2r reste (reduireq2 (op ope) pile)

		|_ _->failwith"impossible";; 



let q2=function nom->q2r (trans ch2term (saut(lire_car nom))) vide;;

q2 "exprinv.txt";;

