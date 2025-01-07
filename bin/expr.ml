open Ast

(* On ne choisi de ne pas typer ici les constructeurs :
Add of expr * expr et non Add of aexpr * aexpr *)

let debug = ref false
let is_id = ref false
let showsrc = ref false

let empty_env = [("prInt", VFun(EVar("a"), Print (Var "a"), []))]
let ref_array = Array.make 100 ("_", VBool false)
  
(* sémantique oprationnelle  grands pas *)
let arth_bin op a1 a2 = 
match a1, a2 with
| VInt k1, VInt k2 ->
VInt(match op with
        | Add -> k1 + k2
        | Mul -> k1 * k2
        | Min -> k1 - k2
        | Div -> if k2=0 then failwith "Division par zero" else k1 / k2)
| _ -> failwith "Wrong type (arth_bin)"

let comp_bin op a1 a2 = 
match a1, a2 with
| VInt k1, VInt k2 ->
VBool(match op with
        | Leq -> k1 <= k2
        | Geq -> k1 >= k2
        | Less -> k1 < k2
        | Greater -> k1 > k2
        | Equal -> k1 = k2
        | Diff -> k1 != k2)   
| _ -> failwith "Wrong type (comp_bin)"

let boop_bin op b1 b2 =
match b1, b2 with
| VBool b1, VBool b2 ->
VBool(match op with
        | Or -> b1 || b2
        | And -> b1 && b2)
| _ -> failwith "Wrong type (boop_bin)"

let notval e = match e with
        | VBool b -> VBool (not b)
        | _ -> failwith "Wrong type (notval)"

let print e = match e with
        | VInt k -> if (not !showsrc) || !debug then (print_int k; print_string "\n"); VInt k
        | _ -> failwith "Wrong type (print)"

let rec getval v env = match env with
        | (v1, value)::_ when (v = v1) -> value
        | _::q -> getval v q
        | [] -> failwith "Var not in env"

let bang va = match va with
        | VRef (b, v) -> (match (b,v) with
                | (false, value) -> value
                | (true, VInt i) -> snd ref_array.(i)
                | _ -> failwith "Let's pretend that didn't happen")
        | _ -> failwith "Wrong type (bang)"

let ref_add v value = let i = ref 0 in
        while (!i < 100 && fst ref_array.(!i) <> "_" && fst ref_array.(!i) <> v) do
                i := !i + 1;
        done;
        if !i = 100 
                then failwith "Fouine is a functionnal language, why so many refs!?" 
                else ref_array.(!i) <- (v, value);
        !i

let assoc v value = let i = ref 0 in
        while (!i < 100 && fst ref_array.(!i) <> v) do
                i := !i + 1;
        done;
        if !i = 100 
                then failwith "Var (ref) undefined" 
                else ref_array.(!i) <- (v, value)

let rec eval_fun f v = match f with
        | VFun (arg, exp, env) -> let rec nov_env arg v env =
                                  (match arg with
                                   | EVar(s) -> ((s, v)::env)
                                   | ECouple(x, y) -> (match v with 
                                                       | VCouple(vx, vy) -> (nov_env x vx (nov_env y vy env))
                                                       | _ -> failwith "Wrong type (pair expected)"))
                                   in
                                   eval exp (nov_env arg v env)
        | _ -> failwith "Too many arguments"

and eval e env = match e with
        | Const i -> VInt i
        | Arth (op, a1, a2) -> arth_bin op (eval a1 env) (eval a2 env)
        | Bool b -> VBool b
        | Boop(op, b1, b2) -> boop_bin op (eval b1 env) (eval b2 env)
        | Comp(op, a1, a2) -> comp_bin op (eval a1 env) (eval a2 env)
        | Not b -> notval (eval b env)
        | IfThenElse (b, a1, a2) -> (match (eval b env) with
                | VBool true -> eval a1 env
                | VBool false -> eval a2 env
                | _ -> failwith "Wrong type (ifte)")
        | Print a -> print (eval a env)
        | Var v -> getval v env
        | LetIn (v, e1, e2) -> let va = eval e1 env in 
                               let rec f v va env =  (* Elle renvoie le nouvel environnement avec v à la valeur va *)
                               (match v with
                                | EVar("_") -> env
                                | ECouple(n1, n2) -> (* Cas des lets déconstructifs*) 
                                        (match va with
                                        | VCouple(v1, v2) -> (f n1 v1 (f n2 v2 env)) (* Moralement (n1, v1) :: (n2, v2) :: env *)
                                        | _  -> failwith "Wrong type (letin couple)")
                                | EVar(s) -> (match va with
                                        | VRef (false, value) -> ((s, VRef (true, VInt (ref_add s value)))::env)
                                        | VRef (true, value) -> ((s, VRef (true, value))::env)
                                        | _ -> ((s, va)::env)))
                                in eval e2 (f v va env)
        | Fun (arg, e1) -> VFun (arg, e1, env) (* Arg peut être de la forme (x, y) *)
        | Appl (e1, e2) -> eval_fun (eval e1 env) (eval e2 env)
        | Ref e -> VRef (false, eval e env)
        | Bang e -> bang (eval e env)
        | Assoc (var, e) -> assoc var (eval e env); Unit
        | Couple(e1, e2) -> let v2 = eval e2 env in
                            let v1 = eval e1 env in
                            VCouple(v1, v2)
        | Match(e, mexpr_list) -> let va = eval e env in
                                  let rec nov_env t v env =
                                        (match t with
                                        | EVar(s) -> ((s, v)::env)
                                        | ECouple(x, y) -> (match v with 
                                                            | VCouple(vx, vy) -> (nov_env x vx (nov_env y vy env))
                                                            | _ -> failwith "Wrong type (pair expected)"))
                                        in
                                   let rec aux va mexpr_list env =
                                   (match mexpr_list with
                                   | [] -> failwith "Matching non exhaustif"
                                   | t :: q -> let t_v, t_e = t in
                                               (match va, t_v with
                                               | VCouple(v1, v2), ECouple(t1, t2) -> eval t_e (nov_env t1 v1 (nov_env t2 v2 env))
                                               | _, EVar(s) -> eval t_e ((s, va) :: env)
                                               | _ -> aux va q env))
                                   in 
                                   aux va mexpr_list env 
