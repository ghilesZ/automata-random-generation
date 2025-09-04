(* Transition : (state, symbol) -> successor, no epsilon transition allowed *)
type ('a, 'b) transition = ('a * 'b, 'a) Hashtbl.t

(* DFA *)
type ('a, 'b) t =
  { states: 'a list
  ; alphabet: 'b list
  ; transitions: ('a, 'b) transition
  ; initial: 'a
  ; finals: 'a list }

let normalize ({states; alphabet; finals; _} as a) =
  { a with
    states= List.sort compare states
  ; alphabet= List.sort compare alphabet
  ; finals= List.sort compare finals }

let equivalent a1 a2 =
  let transitions =
    let a1_trans = Hashtbl.to_seq a1.transitions |> List.of_seq in
    let a2_trans = Hashtbl.to_seq a2.transitions |> List.of_seq in
    List.sort compare a1_trans = List.sort compare a2_trans
  in
  List.sort compare a1.states = List.sort compare a2.states
  && List.sort compare a1.alphabet = List.sort compare a2.alphabet
  && List.sort compare a1.finals = List.sort compare a2.finals
  && a1.initial = a2.initial && transitions

let nb_states a = a.states |> List.length

let nb_transitions a = a.transitions |> Hashtbl.length

let to_dot ?pp_trans ?pp_state name dfa =
  let name =
    if Filename.extension name = ".dot" then name else name ^ ".dot"
  in
  let pp_trans = match pp_trans with None -> fun _ -> "" | Some s -> s in
  let new_id =
    let cpt = ref (-1) in
    fun () -> incr cpt ; !cpt
  in
  let ids = Hashtbl.create (List.length dfa.states) in
  let pp_state =
    match pp_state with
    | None -> (
        fun c ->
          match Hashtbl.find_opt ids c with
          | None ->
              let i = new_id () in
              Hashtbl.add ids c i ; string_of_int i
          | Some i -> string_of_int i )
    | Some s -> s
  in
  let oc = open_out name in
  let fmtoc = Format.formatter_of_out_channel oc in
  Format.fprintf fmtoc "digraph G {@\n" ;
  Format.fprintf fmtoc "  init [shape=point];@\n  init -> \"%s\";@\n"
    (pp_state dfa.initial) ;
  (* states *)
  List.iter
    (fun state ->
      let shape =
        if List.mem state dfa.finals then "doublecircle" else "circle"
      in
      Format.fprintf fmtoc "  \"%s\" [shape=%s; label=\"%s\"];@\n"
        (pp_state state) shape (pp_state state) )
    dfa.states ;
  (* transitions *)
  Hashtbl.iter
    (fun (start, symbol) dest ->
      Format.fprintf fmtoc "  \"%s\" -> \"%s\" [label=\"%s\"];@\n"
        (pp_state start) (pp_state dest) (pp_trans symbol) )
    dfa.transitions ;
  Format.fprintf fmtoc "}@." ;
  close_out oc

let to_svg ?(verbose = false) ?(timeout = 2) ?(keep_dot = false) ?pp_trans
    ?pp_state name pta =
  let name =
    if Filename.extension name = ".svg" then Filename.chop_extension name
    else name
  in
  to_dot ?pp_state ?pp_trans name pta ;
  let cmd =
    Format.asprintf "timeout %i dot -Tsvg %s.dot -o %s.svg" timeout name name
  in
  ( match Sys.command cmd with
  | 0 -> if verbose then Format.printf "%s image saved in %s.svg@." name name
  | 124 -> Format.printf "%s image creation failed (timeout)\n" name
  | code ->
      Format.printf "%s image creation failed with exit code %d\n" name code
  ) ;
  if not keep_dot then
    Format.asprintf "rm -f %s.dot" name |> Sys.command |> ignore

let transit dfa state c = Hashtbl.find_opt dfa.transitions (state, c)

let accept dfa word =
  let rec loop state = function
    | [] -> List.mem state dfa.finals
    | c :: rest -> (
      match Hashtbl.find_opt dfa.transitions (state, c) with
      | Some next_state -> loop next_state rest
      | None -> false )
  in
  loop dfa.initial word

let successors (dfa : ('a, 'b) t) (state : 'a) : 'a list =
  Hashtbl.fold
    (fun (start, _sym) dest acc -> if start = state then dest :: acc else acc)
    dfa.transitions []

let predecessors (dfa : ('a, 'b) t) (state : 'a) : 'a list =
  Hashtbl.fold
    (fun (start, _sym) dest acc -> if dest = state then start :: acc else acc)
    dfa.transitions []

let bfs (dfa : ('a, 'b) t) (initial_states : 'a list)
    (next_states : 'a -> 'a list) : 'a list =
  let visited = Hashtbl.create (List.length dfa.states) in
  let queue = Queue.create () in
  List.iter
    (fun state ->
      Hashtbl.add visited state true ;
      Queue.add state queue )
    initial_states ;
  while not (Queue.is_empty queue) do
    let state = Queue.pop queue in
    List.iter
      (fun next_state ->
        if not (Hashtbl.mem visited next_state) then (
          Hashtbl.add visited next_state true ;
          Queue.add next_state queue ) )
      (next_states state)
  done ;
  Hashtbl.fold (fun state _ acc -> state :: acc) visited []

let of_regexp (re : 'a Regexp.t) : (int, 'a option) t =
  let open Regexp in
  let state_id = ref 0 in
  let fresh () =
    let x = !state_id in
    incr state_id ; x
  in
  let transitions = Hashtbl.create 16 in
  (* Build an NFA fragment for the given regexp.
     Returns: start_state, end_state *)
  let rec build = function
    | Empty ->
        let s = fresh () in
        let e = fresh () in
        (* no transition *)
        (s, e)
    | Epsilon ->
        let s = fresh () in
        let e = fresh () in
        Hashtbl.add transitions (s, None) e ;
        (s, e)
    | Letter c ->
        let s = fresh () in
        let e = fresh () in
        Hashtbl.add transitions (s, Some c) e ;
        (s, e)
    | Union (r1, r2) ->
        let s = fresh () in
        let e = fresh () in
        let s1, e1 = build r1 in
        let s2, e2 = build r2 in
        Hashtbl.add transitions (s, None) s1 ;
        Hashtbl.add transitions (s, None) s2 ;
        Hashtbl.add transitions (e1, None) e ;
        Hashtbl.add transitions (e2, None) e ;
        (s, e)
    | Concat (r1, r2) ->
        let s1, e1 = build r1 in
        let s2, e2 = build r2 in
        Hashtbl.add transitions (e1, None) s2 ;
        (s1, e2)
    | Star r ->
        let s = fresh () in
        let e = fresh () in
        let s1, e1 = build r in
        Hashtbl.add transitions (s, None) s1 ;
        Hashtbl.add transitions (s, None) e ;
        Hashtbl.add transitions (e1, None) s1 ;
        Hashtbl.add transitions (e1, None) e ;
        (s, e)
  in
  let initial, final = build re in
  (* Collect all reachable states *)
  let states_set = Hashtbl.create 32 in
  Hashtbl.iter
    (fun (s, _) d ->
      Hashtbl.replace states_set s () ;
      Hashtbl.replace states_set d () )
    transitions ;
  let states = Hashtbl.fold (fun k () acc -> k :: acc) states_set [] in
  let alphabet =
    Hashtbl.fold
      (fun (_s, sym_opt) _ acc ->
        match sym_opt with
        | Some c when not (List.mem (Some c) acc) -> Some c :: acc
        | _ -> acc )
      transitions []
  in
  {states; alphabet; transitions; initial; finals= [final]}

let epsilon_closure (automaton : ('a, 'b) t) (states : 'a list) : 'a list =
  let epsilon_successors s =
    Hashtbl.find_all automaton.transitions (s, None)
  in
  bfs automaton states epsilon_successors

let find_index f lst =
  let rec aux i = function
    | [] -> raise Not_found
    | x :: xs -> if f x then i else aux (i + 1) xs
  in
  aux 0 lst

let determinize (nfa : (int, 'a option) t) : (int list, 'a) t =
  let initial_closure = epsilon_closure nfa [nfa.initial] in
  let states_tbl = Hashtbl.create 32 in
  let transitions = Hashtbl.create 64 in
  let queue = Queue.create () in
  let add_state s =
    if not (Hashtbl.mem states_tbl s) then (
      Hashtbl.add states_tbl s true ;
      Queue.push s queue )
  in
  add_state initial_closure ;
  let alphabet =
    nfa.alphabet |> List.filter_map (fun x -> x)
    (* removes None, unwraps Some *)
  in
  while not (Queue.is_empty queue) do
    let current = Queue.pop queue in
    List.iter
      (fun sym ->
        let move =
          List.fold_left
            (fun acc state ->
              match transit nfa state (Some sym) with
              | Some dst -> dst :: acc
              | None -> acc )
            [] current
        in
        let closure = epsilon_closure nfa move in
        if closure <> [] then (
          add_state closure ;
          Hashtbl.add transitions (current, sym) closure ) )
      alphabet
  done ;
  let dfa_states = Hashtbl.fold (fun s _ acc -> s :: acc) states_tbl [] in
  let finals =
    List.filter
      (fun s -> List.exists (fun x -> List.mem x nfa.finals) s)
      dfa_states
  in
  { states= dfa_states
  ; alphabet
  ; transitions
  ; initial= initial_closure
  ; finals }

let minimize dfa : (int, 'a) t =
  let partition =
    let finals, non_finals =
      List.partition (fun s -> List.mem s dfa.finals) dfa.states
    in
    [finals; non_finals]
  in
  let rec refine partition =
    let split_block block =
      let by_trans =
        List.fold_left
          (fun acc s ->
            let sig_ =
              List.map
                (fun a ->
                  match transit dfa s a with
                  | Some dest -> find_index (List.mem dest) partition
                  | None -> -1 )
                dfa.alphabet
            in
            let existing = List.assoc_opt sig_ acc in
            match existing with
            | Some group -> (sig_, s :: group) :: List.remove_assoc sig_ acc
            | None -> (sig_, [s]) :: acc )
          [] block
      in
      List.map snd by_trans
    in
    let new_partition = List.flatten (List.map split_block partition) in
    let normalize p =
      List.rev_map (List.sort compare) p |> List.sort compare
    in
    let normalized = normalize partition in
    if normalize new_partition = normalized then normalized
    else refine new_partition
  in
  let minimized_partition = refine partition in
  let state_ids =
    List.mapi (fun i group -> (group, i)) minimized_partition
  in
  let find_class s =
    List.assoc_opt true
      (List.map (fun (group, id) -> (List.mem s group, id)) state_ids)
    |> Option.get
  in
  let states = List.map snd state_ids in
  let initial = find_class dfa.initial in
  let finals =
    List.filter
      (fun id -> List.exists (fun s -> find_class s = id) dfa.finals)
      states
  in
  let transitions = Hashtbl.create 64 in
  List.iter
    (fun (group, id) ->
      match group with
      | [] -> ()
      | s :: _ ->
          List.iter
            (fun a ->
              match transit dfa s a with
              | Some dst ->
                  let dst_id = find_class dst in
                  Hashtbl.add transitions (id, a) dst_id
              | None -> () )
            dfa.alphabet )
    state_ids ;
  {states; alphabet= dfa.alphabet; transitions; initial; finals}

type 'a state = Real of 'a | Fake of int

let statify dfa =
  let mk_real x = Real x in
  let transitions = Hashtbl.create (Hashtbl.length dfa.transitions) in
  Hashtbl.iter
    (fun (s1, c) s2 -> Hashtbl.add transitions (mk_real s1, c) (mk_real s2))
    dfa.transitions ;
  { alphabet= dfa.alphabet
  ; states= List.map mk_real dfa.states
  ; initial= mk_real dfa.initial
  ; finals= List.map mk_real dfa.finals
  ; transitions }

let to_regexp (dfa : ('a, 'b) t) : 'b Regexp.t =
  let open Regexp in
  (* encapsulate state type within a polymorphic type to be able to spawn fake states *)
  let dfa = statify dfa in
  (* Create a copy of the DFA, adding a fake initial and final state *)
  let fake_initial = Fake 1 in
  let fake_final = Fake 2 in
  let new_states = fake_initial :: fake_final :: dfa.states in
  let new_transitions = Hashtbl.create (Hashtbl.length dfa.transitions) in
  (* copy transitions *)
  Hashtbl.iter
    (fun (start, symbol) dest ->
      Hashtbl.add new_transitions (start, Some symbol) dest )
    dfa.transitions ;
  (* add ε-transition from fake_initial to original initial state *)
  Hashtbl.add new_transitions (fake_initial, None) dfa.initial ;
  (* add ε-transition from original final states to fake_final *)
  List.iter
    (fun final_state ->
      Hashtbl.add new_transitions (final_state, None) fake_final )
    dfa.finals ;
  (* Initialize regexps with transitions (including epsilon transitions) *)
  let regexps = Hashtbl.create 10 in
  Hashtbl.iter
    (fun (start, symbol) dest ->
      let re = match symbol with None -> Epsilon | Some s -> Letter s in
      let key = (start, dest) in
      let existing =
        match Hashtbl.find_opt regexps key with
        | Some r -> union r re
        | None -> re
      in
      Hashtbl.replace regexps key existing )
    new_transitions ;
  (* Ensure all state pairs are initialized *)
  List.iter
    (fun i ->
      List.iter
        (fun j ->
          if not (Hashtbl.mem regexps (i, j)) then
            Hashtbl.add regexps (i, j) Empty )
        new_states )
    new_states ;
  (* Eliminate all intermediate states *)
  let eliminate_state state =
    List.iter
      (fun i ->
        List.iter
          (fun j ->
            if i <> state && j <> state then
              let rij =
                Hashtbl.find_opt regexps (i, j)
                |> Option.value ~default:Empty
              in
              let rik =
                Hashtbl.find_opt regexps (i, state)
                |> Option.value ~default:Empty
              in
              let rkk =
                Hashtbl.find_opt regexps (state, state)
                |> Option.value ~default:Empty
              in
              let rkj =
                Hashtbl.find_opt regexps (state, j)
                |> Option.value ~default:Empty
              in
              let star_rkk = if rkk = Empty then Epsilon else Star rkk in
              let new_re = union rij (concat rik (concat star_rkk rkj)) in
              Hashtbl.replace regexps (i, j) new_re )
          new_states )
      new_states
  in
  List.iter eliminate_state
    (List.filter (fun s -> s <> fake_initial && s <> fake_final) new_states) ;
  (* The final regular expression is the one from fake_initial to fake_final *)
  let final_re =
    Hashtbl.find_opt regexps (fake_initial, fake_final)
    |> Option.value ~default:Empty
  in
  final_re
