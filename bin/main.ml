let show_time = ref false

let svg = ref false

let keep_intermediate = ref false

let uniform = ref false

let balanced = ref true

let histogram = ref 0

let size = ref 10

let verbose = ref false

let toggle () =
  uniform := true ;
  balanced := false

let parse_args () =
  let usage_msg = "Usage: program [options]" in
  let speclist =
    [ ("-time", Arg.Set show_time, "Show timing information")
    ; ("-verbose", Arg.Set verbose, "Prints some statistics)")
    ; ("-svg", Arg.Set svg, "Prints some statistics)")
    ; ( "-keep"
      , Arg.Set keep_intermediate
      , "Keep intermediate outputs (png/svg)" )
    ; ("-uniform", Arg.Unit toggle, "Enable uniform tree generation")
    ; ("-balanced", Arg.Unit ignore, "Enable balanced tree generation")
    ; ("-histogram", Arg.Set_int histogram, "Enable the histogram measure")
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

let base suffix = if suffix = "" then Fun.id else fun s -> s ^ "_" ^ suffix

let generate_all ~(pp_trans : char -> string) ~suffix ~alphabet tree =
  if !verbose then
    Format.printf
      "Generation using %s method\n------------------------------\n" suffix ;
  if !keep_intermediate then Ubtree.to_png_bust (base suffix "tree") tree ;
  let regexp = Ubtree.to_regexp alphabet tree in
  if !keep_intermediate then Regexp.to_png (base suffix "regexp") regexp ;
  if !verbose then
    Format.printf "Regexp: %s\n%!" (Regexp.to_string (String.make 1) regexp) ;
  let automata = wrap "Automata.of_regexp" Automata.of_regexp regexp in
  if !keep_intermediate then
    Automata.to_svg
      ~pp_trans:(function
        | None -> Format.asprintf "ε" | Some c -> Format.asprintf "%c" c )
      (base suffix "automata") automata ;
  let determinized = wrap "determinize" Automata.determinize automata in
  if !keep_intermediate then
    Automata.to_svg ~pp_trans (base suffix "determinized") determinized ;
  let minimized = wrap "minimize" Automata.minimize determinized in
  if !verbose then
    Format.printf "Imbalance: %f%%\nAutomata: %i nodes + %i transitions\n"
      (Ubtree.imbalance_percentage tree)
      (Automata.nb_states minimized)
      (Automata.nb_transitions minimized) ;
  minimized

let () =
  Random.self_init () ;
  parse_args () ;
  let p_bin = 0.85 in
  let alphabet = ['a'; 'b'] in
  let pp_trans = Format.sprintf "%c" in
  let suffix = if !uniform then "uniform" else "balanced" in
  let generator () =
    if !uniform then
      let tree = Ubtree.random_uniform_tree !size in
      generate_all ~pp_trans:(Format.sprintf "%c") ~suffix ~alphabet tree
    else
      let tree = Ubtree.random_bust_of_size p_bin !size in
      generate_all ~pp_trans:(Format.sprintf "%c") ~suffix ~alphabet tree
  in
  let gen () = generator () |> Automata.normalize in
  if !histogram > 0 then (
    let hist = Distrib.histogram gen !histogram in
    let most_present, count = Distrib.most_common hist |> Option.get in
    let diversity = Distrib.richness hist in
    let most_present_regexp = Automata.to_regexp most_present in
    Format.printf "Most common language : %s\n"
      (Regexp.to_string (Format.sprintf "%c") most_present_regexp) ;
    Format.printf "Appearing %.2f%% of the time\n"
      (float (100 * count) /. float !histogram) ;
    Format.printf "Diversity: %i\n" diversity ;
    Format.printf "Variability: %.2f\n" (float diversity /. float !histogram) ;
    Format.printf "Simpson index: %f\n" (Distrib.simpson hist) ;
    if !svg then
      Distrib.to_svg_histogram "histogram"
        (fun fmt a ->
          Format.fprintf fmt "%s"
            (Automata.to_regexp a |> Regexp.to_string (Format.sprintf "%c")) )
        hist )
  else
    let automaton = generator () in
    if !svg then
      Automata.to_svg ~pp_trans (base suffix "minimized") automaton
