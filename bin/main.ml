open Ubtree

let join_2_side_by_side img1 img2 output =
  let cmd =
    Format.asprintf "montage %s %s -tile 2x1 -geometry +0+0 %s" img1 img2
      output
  in
  match Sys.command cmd with
  | 0 -> Format.printf "Created combined image: %s\n" output
  | code -> Format.printf "Command failed with exit code %d\n" code

let time label f x =
  let start_time = Unix.gettimeofday () in
  let result = f x in
  let end_time = Unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  Format.printf "%s took %.6f seconds\n%!" label elapsed ;
  result

let generate_all ~(pp_trans : char -> string) ~suffix ~alphabet tree =
  let base = if suffix = "" then Fun.id else fun s -> s ^ "_" ^ suffix in
  to_png_bust (base "tree") tree ;
  let regexp = regexp_of_bust alphabet tree in
  Regexp.to_png (base "regexp") regexp ;
  let automata = time "Automata.of_regexp" Automata.of_regexp regexp in
  Automata.to_png
    ~pp_trans:(function
      | None -> Format.asprintf "ε" | Some c -> Format.asprintf "%c" c )
    (base "automata") automata ;
  let determinized = time "determinize" Automata.determinize automata in
  Automata.to_png ~pp_trans (base "determinized") determinized ;
  let minimized = time "minimize" Automata.minimize determinized in
  Automata.to_png ~pp_trans (base "minimized") minimized ;
  join_2_side_by_side
    (base "regexp" ^ ".png")
    (base "minimized" ^ ".png")
    (base "generations" ^ ".png") ;
  Format.printf "Imbalance: %f%%\n" (Ubtree.imbalance_percentage tree) ;
  Format.printf "Automata: %i nodes + %i transitions\n"
    (Automata.nb_states minimized)
    (Automata.nb_transitions minimized)

let () =
  Random.self_init () ;
  let p_bin = 0.8 in
  let size = 10 in
  (* Read size from command line if provided *)
  let size =
    if Array.length Sys.argv > 1 then
      match int_of_string_opt Sys.argv.(1) with
      | None ->
          Format.eprintf "Invalid size '%s', using default (%d).\n%!"
            Sys.argv.(1) size ;
          size
      | Some s -> s
    else size
  in
  let alphabet = ['a'; 'b'; 'c'; 'd'] in
  Format.printf "\nUniform generation\n-------------------\n" ;
  let tree = Ubtree.random_uniform_tree size in
  generate_all ~pp_trans:(Format.sprintf "%c") ~suffix:"uniform" ~alphabet
    tree ;
  Format.printf "\nBalanced generation\n-------------------\n" ;
  let tree = Ubtree.random_bust_of_size p_bin size in
  generate_all ~pp_trans:(Format.sprintf "%c") ~suffix:"balanced" ~alphabet
    tree
