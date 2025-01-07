type var = string

type a_op = 
  Add
  | Mul
  | Min
  | Div

type b_op =
  Or
  | And
 
type comp =
  Leq
  | Geq
  | Less
  | Greater
  | Diff
  | Equal

type expr =
  Const of int
  | Arth of a_op*expr*expr
  | Bool of bool
  | Boop of b_op*expr*expr
  | Comp of comp*expr*expr
  | Not of expr
  | IfThenElse of expr*expr*expr
  | Print of expr
  | Var of string
  | Ref of expr
  | LetIn of expr_match*expr*expr
  | Fun of expr_match*expr
  | Appl of expr*expr
  | Bang of expr
  | Assoc of string*expr
  | Couple of expr*expr
  | Match of expr * (expr_match * expr) list

and expr_match = 
  EVar of string
  | ECouple of expr_match*expr_match

type valeur = Unit | VInt of int | VBool of bool | VFun of expr_match*expr*env | VRef of bool*valeur | VCouple of valeur*valeur
                    
and env = (var*valeur) list