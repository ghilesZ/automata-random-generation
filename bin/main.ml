let show_time = ref false

let keep_intermediate = ref false

let uniform = ref false

let balanced = ref false

let size = ref 10

let verbose = ref false

let parse_args () =
  let usage_msg = "Usage: program [options]" in
  let speclist =
    [ ("-time", Arg.Set show_time, "Show timing information")
    ; ("-verbose", Arg.Set verbose, "Prints some statistics)")
    ; ( "-keep"
      , Arg.Set keep_intermediate
      , "Keep intermediate outputs (png/svg)" )
    ; ("-uniform", Arg.Set uniform, "Enable uniform tree generation")
    ; ("-balanced", Arg.Set balanced, "Enable balanced tree generation")
    ; ( "-size"
      , Arg.Set_int size
      , "Set the size of the generated trees (default: 10)" ) ]
  in
  let anon_fun arg =
    Printf.eprintf "Ignoring anonymous argument: %s\n" arg
  in
  Arg.parse speclist anon_fun usage_msg

let time label f x =
  let start_time = Unix.gettimeofday () in
  let result = f x in
  let end_time = Unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  Format.printf "%s took %.6f seconds\n%!" label elapsed ;
  result

let wrap label f x = if !show_time then time label f x else f x

let generate_all ~(pp_trans : char -> string) ~suffix ~alphabet tree =
  if !verbose then
    Format.printf
      "Generation using %s method\n------------------------------\n" suffix ;
  let base = if suffix = "" then Fun.id else fun s -> s ^ "_" ^ suffix in
  if !keep_intermediate then Ubtree.to_png_bust (base "tree") tree ;
  let regexp = Ubtree.to_regexp alphabet tree in
  if !keep_intermediate then Regexp.to_png (base "regexp") regexp ;
  if !verbose then
    Format.printf "%s\n%!" (Regexp.to_string (String.make 1) regexp) ;
  let automata = wrap "Automata.of_regexp" Automata.of_regexp regexp in
  if !keep_intermediate then
    Automata.to_svg
      ~pp_trans:(function
        | None -> Format.asprintf "ε" | Some c -> Format.asprintf "%c" c )
      (base "automata") automata ;
  let determinized = wrap "determinize" Automata.determinize automata in
  if !keep_intermediate then
    Automata.to_svg ~pp_trans (base "determinized") determinized ;
  let minimized = wrap "minimize" Automata.minimize determinized in
  Automata.to_svg ~pp_trans (base "minimized") minimized ;
  if !verbose then
    Format.printf "Imbalance: %f%%\nAutomata: %i nodes + %i transitions\n"
      (Ubtree.imbalance_percentage tree)
      (Automata.nb_states minimized)
      (Automata.nb_transitions minimized)

let () =
  Random.self_init () ;
  parse_args () ;
  let p_bin = 0.8 in
  let alphabet = ['a'; 'b'] in
  ( if !uniform then
      let tree = Ubtree.random_uniform_tree !size in
      generate_all ~pp_trans:(Format.sprintf "%c") ~suffix:"uniform"
        ~alphabet tree ) ;
  if !balanced then
    let tree = Ubtree.random_bust_of_size p_bin !size in
    generate_all ~pp_trans:(Format.sprintf "%c") ~suffix:"balanced" ~alphabet
      tree
