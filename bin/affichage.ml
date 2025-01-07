open Ast

(* fonction d'affichage *)
let rec aff_var v =
  match v with
  | EVar(s) -> s
  | ECouple(v1, v2) -> (aff_var v1) ^ ", " ^ (aff_var v2) 

let rec affiche_expr e =
  let aff_una s a =
      begin
	print_string s;
	affiche_expr a;
	print_string ")"
      end
  in
  let aff_bin s a b = 
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end
  in
  let aff_ter s a b c =
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ", ";
	affiche_expr c;
	print_string ")"
      end
  in
  let rec aff_liste l =
    match l with
    | [] -> ()
    | (v, e) :: q -> begin
                     aff_una ("(" ^ (aff_var v) ^ ", ") e;
                     aff_liste q
                    end
  in
  let aff_match a l =
    begin
  print_string "Match(";
	affiche_expr a;
	print_string ", ";
  aff_liste l;
	print_string ")";
    end
  in
  match e with
  | Const k -> print_int k
  | Arth(op, e1, e2) -> (match op with
    | Add -> aff_bin "Add(" e1 e2
    | Mul -> aff_bin "Mul(" e1 e2
    | Min -> aff_bin "Min(" e1 e2
    | Div -> aff_bin "Div(" e1 e2)
  | Bool b -> print_string (string_of_bool b)
  | Not(e1) -> aff_una "Not(" e1
  | Boop(op, e1, e2) -> (match op with
    | Or -> aff_bin "Or(" e1 e2
    | And -> aff_bin "And(" e1 e2)
  | Comp(op, e1, e2) -> (match op with
    | Geq -> aff_bin "Geq(" e1 e2
    | Leq -> aff_bin "Leq(" e1 e2
    | Diff -> aff_bin "Diff(" e1 e2
    | Equal -> aff_bin "Equal(" e1 e2
    | Less -> aff_bin "Less(" e1 e2
    | Greater -> aff_bin "Greater(" e1 e2)
  | IfThenElse(e1, e2, e3) -> aff_ter "IfThenElse(" e1 e2 e3
  | Print(e1) -> aff_una "Print(" e1
  | Var v -> print_string v
  | LetIn(v, e2, e3) -> aff_bin ("LetIn(" ^ (aff_var v) ^ ", ") e2 e3
  | Appl(e1, e2) -> aff_bin "Appl(" e1 e2
  | Fun(a, e1) -> aff_una ("Fun(" ^ (aff_var a) ^ ", ") e1
  | Ref e -> aff_una "Ref(" e
  | Bang e -> aff_una "Bang(" e
  | Assoc (v, e) -> aff_una ("Assoc(" ^ v ^ ", ") e
  | Couple(e1, e2) -> aff_bin ("Couple(") e1 e2
  | Match(e, mexpr_list) -> aff_match e mexpr_list 

let rec affiche_src e =
  let rec affiche_var v =
    match v with
    | EVar(s) -> print_string s
    | ECouple(v1, v2) -> affiche_var v1;
                        print_string ", ";
                        affiche_var v2
  in
  let rec aff_list l str = match l with
      | [] -> ()
      | a::q ->
          begin
            print_string str;
            affiche_src a;
            aff_list q str
          end
  in
  let rec aff_list_match l = match l with
      | [] -> ()
      | (v, e)::q ->
          begin
            print_string "| ";
            affiche_var v;
            print_string " -> ";
            affiche_src e;
            aff_list_match q
          end
  in
  let aff_fun f args =
      begin
        print_string "(";
	print_string f;
	aff_list args " ";
	print_string ")"
      end
  in
  let aff_op_bin op a b = 
      begin
	print_string "(";
	affiche_src a;
	print_string op;
	affiche_src b;
	print_string ")"
      end
  in
  let aff_ter s1 s2 s3 a b c =
      begin
        print_string "(";
	print_string s1;
	affiche_src a;
	print_string s2;
	affiche_src b;
	print_string s3;
	affiche_src c;
	print_string ")"
      end
  in

  match e with
  | Const k -> print_int k
  | Arth(op, e1, e2) -> (match op with
    | Add -> aff_op_bin " + " e1 e2
    | Mul -> aff_op_bin " * " e1 e2
    | Min -> aff_op_bin " - " e1 e2
    | Div -> aff_op_bin " / " e1 e2)
  | Bool b -> print_string (string_of_bool b)
  | Not(e1) -> aff_fun "not" [e1]
  | Boop(op, e1, e2) -> (match op with
    | Or -> aff_op_bin " || " e1 e2
    | And -> aff_op_bin " && " e1 e2)
  | Comp(op, e1, e2) -> (match op with
    | Geq -> aff_op_bin " >= " e1 e2
    | Leq -> aff_op_bin " <= " e1 e2
    | Diff -> aff_op_bin " <> " e1 e2
    | Equal -> aff_op_bin " = " e1 e2
    | Less -> aff_op_bin " < " e1 e2
    | Greater -> aff_op_bin " > " e1 e2)
  | IfThenElse(e1, e2, e3) -> aff_ter "if " " then " " else " e1 e2 e3
  | Print(e1) -> aff_fun "prInt" [e1]
  | Var v -> print_string v
  | LetIn(v, e1, e2) -> print_string "let "; affiche_var v; print_string " = "; affiche_src e1; print_string " in "; affiche_src e2
  | Appl(e1, e2) -> aff_op_bin " " e1 e2
  | Fun(a, e1) -> aff_fun ("fun " ^ (aff_var a) ^ " -> ") [e1]
  | Ref e -> aff_fun "ref" [e]
  | Bang e -> aff_fun "!" [e]
  | Assoc (v, e) -> aff_fun (v ^ " := ") [e]
  | Couple (e1, e2) -> aff_op_bin "," e1 e2
  | Match (e, mexpr_list) -> print_string "match "; affiche_src e; print_string " with :"; aff_list_match mexpr_list

let rec affiche_val v =
 match v with
 | Unit -> print_string "()"
 | VInt(k) -> print_int k
 | VBool(b) -> print_string (string_of_bool b)
 | VFun (arg, expr, _) -> print_string ("fun " ^ (aff_var arg) ^ " -> ("); affiche_src expr; print_string ")"
 | VRef (b, v) -> print_string "ref(";
                  print_string (string_of_bool b);
                  print_string ", ";
                  affiche_val v;
                  print_string ")";
 | VCouple (v1, v2) -> print_string "(";
                       affiche_val v1;
                       print_string ", ";
                       affiche_val v2;
                       print_string ")";
;;

                   
