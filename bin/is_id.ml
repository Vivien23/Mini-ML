open Ast

let rec expr_match_to_expr var_l =
match var_l with 
| EVar(var) -> Var(var)
| ECouple(vl1, vl2) -> Couple(expr_match_to_expr vl1, expr_match_to_expr vl2)
;;

let rec in_var_l x var_l = 
  match var_l with
  | EVar(var) -> x = var
  | ECouple(vl1, vl2) -> (in_var_l x vl1) || (in_var_l x vl2)
;;

let rec subst_var_l e2 var_l e1 = 
  match var_l with
  | EVar(var) -> subst e2 var e1
  (* When calling subst_var_l, e1 must already be in normal form - We want to match it in the couple case *)
  | ECouple(vl1, vl2) -> match e1 with | Couple(e1_1, e1_2) -> subst_var_l (subst_var_l e2 vl1 e1_1) vl2 e1_2 
                                       | _ -> failwith "not typed"
and subst e2 x e1 = 
  let k e = subst e x e1 in
  match e2 with 
  | LetIn (var_l, e'1, e'2) ->  if in_var_l x var_l then failwith "Substitution illégale" else LetIn(var_l, k e'1, k e'2)
  | Fun (var_l, e) -> if in_var_l x var_l then failwith "Substitution illégale" else  Fun(var_l, k e) 
  | Assoc (var, _) when x = var -> failwith "Substitution illégale"
  | Match(_, _) -> failwith "todo compliqué"

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
  | LetIn (var_l, e1, e2) -> let e1_n = normalize e1 in normalize (subst_var_l e2 var_l e1_n)
  | Fun (arg, e1) -> Fun(arg, normalize e1)
  | Appl (Fun(var_l, e1), e2) -> let e1_n = normalize e1 in normalize (subst_var_l e2 var_l e1_n) 
  | Appl _ -> failwith "Global env call" (* Choix important : comment traiter les variables définies plus globalement ? Pour l'instant, elles ne sont pas explorées *)
  | Ref _ -> failwith "effet de bord -> boom"
  | Bang _ -> failwith "todo"
  | Assoc _ -> failwith "effet de bord -> boom"
  | Couple(e1, e2) -> Couple(normalize e1, normalize e2)
  | Match _ -> failwith "complicated stuff"
;;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const i1, Const i2-> (i1 = i2)
  | Bool b1, Bool b2  -> (b1 = b2)
  | Not b1, Not b2 -> (b1 = b2)

  | Var v1, Var v2 -> (v1 = v2)
  | LetIn (var_l1, e11, e12), LetIn (var_l2, e21, e22) -> (expr_eq e11 e21) && (expr_eq e12 (LetIn(var_l1, e21, subst_var_l e22 var_l2 (expr_match_to_expr var_l1))))
  | Fun (vl1, e1), Fun(vl2, e2) -> expr_eq e1 (subst_var_l e2 vl2 (expr_match_to_expr vl1))

  | Appl (_, _), Appl(_, _) -> failwith "todo" (* Choix important : comment traiter les variables définies plus globalement ? Pour l'instant, elles ne sont pas explorées *)
  | Bang _, Bang _ -> failwith "todo"
  | Couple(e11, e12), Couple(e21, e22) -> (expr_eq e11 e21) && (expr_eq e12 e22)


  | Arth _, Arth _-> failwith "stuff" 
  | Boop _, Boop _ ->  failwith "stuff"
  | Comp _, Comp _ -> failwith "stuff"

  | Ref _, Ref _ -> failwith "effet de bord -> boom"
  | Assoc _, Assoc _ -> failwith "effet de bord -> boom"
  | Print _, Print _ ->  failwith "effet de bord -> boom"

  | Match _, Match _ -> failwith "complicated stuff"
  | IfThenElse _, IfThenElse _ ->  failwith "complicated stuff"

  | _ -> false
;;
;;

let is_fun_id e =
  let is_fun_id_aux e_c e_g =
    (* Reach normal form on e_c *)
    let e_c = normalize e_c in 
    if !Expr.debug then
    begin
    print_string "e_c = "; let _ =  Affichage.affiche_expr e_c in
    print_string "; ";
    print_string "e_g = "; let _ =  Affichage.affiche_expr e_g in
    print_string "\n";
    end;
    if (expr_eq e_c  e_g) then print_string "true\n"
    else print_string "false\n"

    (* Apply derivation rules *)
  in
  is_fun_id_aux e (Fun(EVar "x", Var "x"))
;;

let main e =
  (* Go throught the AST and run is_fun_id on every function def *)
  let rec parcours e = 
  match e with
  | Arth (_, a1, a2) -> parcours a1; parcours a2
  | Boop(_, b1, b2) ->  parcours b1; parcours b2
  | Comp(_, a1, a2) -> parcours a1; parcours a2
  | Not b -> parcours b
  | IfThenElse (b, e1, e2) -> parcours b; parcours e1; parcours e2
  | Print a -> parcours a
  | LetIn (_, e1, e2) -> parcours e1; parcours e2
  | Appl (e1, e2) -> parcours e1; parcours e2 (* maybe something to do here *)
  | Ref e -> parcours e
  | Bang e -> parcours e
  | Assoc (_, e) -> parcours e
  | Couple(e1, e2) -> parcours e1; parcours e2
  | Match(_, _) -> failwith "todo (match)"

  | Var _ -> ()
  | Const _ -> ()
  | Bool _ -> ()

  | Fun (_, e1) -> print_string "Entering\n"; is_fun_id e; parcours e1
  in
  parcours e
;; 