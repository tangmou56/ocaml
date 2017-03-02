(* TANG YUKE *)
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

let rec motplus=function l->if l=vide then 0
			else max (cnb l (tete l)) (motplus(suite l));;
let rec motplusf=function l-> if (cnb l (tete l))=(motplus l) then (tete l)
				else motplusf(suite l);;
motplusf l;;

