type 'a liste = vide | cons of  'a* 'a liste;;
type terme=po|pf|op|pg of string|entier of int;;
type arbre=V|N of terme*arbre*arbre;;

let lire_car =function nom ->
 let canal=open_in nom in
  let rec lirea =function  () -> 
         try let a = input_char canal in 
            cons (a ,lirea ())  with
       End_of_file -> begin close_in canal;vide end
      in    lirea ();;

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

let ch2terme=function "("->po
	|")"->pf
	|("+"|"-"|"*"|"/")as ope ->op ope
	|("p"|"f")as p ->pg p
	|v->entier ((int_of_string v));;

let rec trans=fun f vide->vide
	|f (cons(tete,reste))->
			cons((f tete),(trans f reste));;

let tout= function nom -> analyselex(lire_car nom);;


let op2fun=function 
		op  "+"-> prefix +
		|op "-"->prefix -
		|op "*"->prefix *
		|op "/"->prefix /
		|_->failwith"impossible";; 
let tete=function cons(a,_)-> a|_ ->failwith "impossible";;
let suite=function  cons(_,a) ->a|_->failwith "impossible";;


let reduire=fun ch (cons(entier v2,cons(entier v1,reste)))->
cons(entier ((op2fun ch) v1 v2),reste)
		|_ _ -> failwith"impossible";;

let rec traiter=fun vide pile->pile
		|(cons(entier v,reste)) pile->traiter reste (cons(entier v,pile))
		|(cons(pg p,reste)) pile->traiter reste (cons(pg p,pile))
		|(cons(op ope,reste)) pile->traiter reste (reduire (op ope) pile)
		|_ _->failwith"impossible";;





let rec lire_ligne = function () ->
let a=input_char(stdin) in
if (a = `\n` ) then
cons(a,vide)
else
cons(a,lire_ligne());;

let rec evalue =function pile ->
try let a = lire_ligne() in
if (tete pile)=pg p then print_int (tete (suite pile))
else 
evalue (traiter (trans ch2terme (analyselex a)) pile )

with end_of_file -> exit 0;;
evalue vide;;

