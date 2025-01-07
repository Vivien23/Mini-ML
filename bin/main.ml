open Expr
open Affichage


(* "incantations" qu'il n'est pas nécessaire de comprendre dans un premier
   temps : on récupère l'entrée, dans un fichier ou sur le clavier *)

let nom_fichier = ref ""

let recupere_entree () =
  let optlist = [
    ("-showsrc", Arg.Set showsrc, "Affiche le programme écrit en entrée");
    ("-debug", Arg.Set debug, "Active le mode de debuggage")
  ] in

  let help = "Aide de fouine" in

  Arg.parse optlist (fun s -> nom_fichier := s) help;
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse () 
  with e -> (Printf.printf "problème de saisie\n"; raise e)

(* le traitement d'une expression en entrée *)   
let execute e =
  begin
    if !showsrc || !debug then begin affiche_src e; print_newline() end;
    if !debug then begin affiche_expr e; print_newline() end;
    let v =  Expr.eval e Expr.empty_env in
    if !debug then begin affiche_val v; print_newline() end;
  end

(* la boucle principale *)
let calc () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e


let _ = calc()
