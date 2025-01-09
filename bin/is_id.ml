open Ast

let main e =
  print_string "Hello World\n"
  (* Go throught the AST and run is_fun_id on every function def *)
;;

let is_fun_id e =
  let is_fun_id_aux e_c e_g =
    (* Reach normal form on e_c *)
    if e_c == e_g then true 
    else false
    (* Apply derivation rules *)
  in
  is_fun_id_aux e (fun x -> x)
;;

let rec in_var_l x var_l = 
  match var_l with
  | EVar(var) -> x = var
  | ECouple(vl1, vl2) -> (in_var_l x vl1) || (in_var_l x vl2)
;;

let rec subst e2 x e1 = 
  let k e = subst e x e1 in
  match e2 with 
  | LetIn (var_l, e'1, e'2) ->  if in_var_l x var_l then failwith "Substitution illégale" else LetIn(var_l, k e'1, k e'2)
  | Fun (var_l, e) -> if in_var_l x var_l then failwith "Substitution illégale" else  Fun(var_l, k e) 
  | Assoc (var, _) when x = var -> failwith "Substitution illégale"
  | Match(_, _) -> failwith "todo"

  | Arth (op, a1, a2) -> Arth(op, k a1, k a2)
  | Boop(op, b1, b2) -> Boop(op, k b1, k b2)
  | Comp(op, a1, a2) -> Comp(op, k a1, k a2)
  | Not b -> Not (k b)
  | IfThenElse (b, a1, a2) -> IfThenElse(k b, k a1, k a2)
  | Print a -> Print (k a)
  | Appl (e'1, e'2) -> Appl (k e'1, k e'2)
  | Ref e -> Ref (k e)
  | Bang e -> Bang (k e)
  | Couple(e1, e2) -> Couple(k e1, k e2)
  | Assoc (var, e) -> Assoc(var, k e)

  | Var var when var = x -> e1

  | _ -> e2
;;

let rec normalize e = 
  match e with
  | Const i -> Const i
  | Bool b -> Bool b
  | Arth (_, _, _) -> failwith "stuff" 
  | Boop(_, _, _) ->  failwith "stuff"
  | Comp(_, _, _) -> failwith "stuff"
  | Not b -> Not (normalize b)
  | IfThenElse (_, _, _) ->  failwith "complicated stuff"
  | Print _ ->  failwith "effet de bord -> boom"
  | Var v -> Var v
  | LetIn (_, _, _) -> failwith "todo" (* normalize (subst e2 var_l e1) mais avec une expr-match*)
  | Fun (arg, e1) -> Fun(arg, e1)
  | Appl (_, _) -> failwith "todo"
  | Ref _ -> failwith "effet de bord -> boom"
  | Bang _ -> failwith "todo"
  | Assoc _ -> failwith "effet de bord -> boom"
  | Couple(e1, e2) -> Couple(normalize e1, normalize e2)
  | Match _ -> failwith "complicated stuff"
;;

