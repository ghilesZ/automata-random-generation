type 'a t =
  | Empty
  | Epsilon
  | Letter of 'a
  | Union of 'a t * 'a t
  | Concat of 'a t * 'a t
  | Star of 'a t

let to_string ?alphabet letter r =
  let alphabet = Option.map (List.sort compare) alphabet in
  (* Collects all letters from a union operation *)
  let rec collect = function
    | Union (r1, r2) -> collect r1 @ collect r2
    | Letter c -> [c]
    | _ -> raise Exit
  in
  (* Checks if a union of letters matches the provided alphabet *)
  let is_sigma union =
    try
      match alphabet with
      | Some a -> List.sort compare (collect union) = a
      | None -> false
    with Exit -> false
  in
  let rec loop = function
    | Empty -> "empty"
    | Epsilon -> "ε"
    | Letter c -> letter c
    | Union (r1, r2) as u ->
        (* If it's the entire alphabet, represent as "_" *)
        if is_sigma u then "_" else "(" ^ loop r1 ^ "|" ^ loop r2 ^ ")"
    | Concat (r1, r2) -> loop r1 ^ "; " ^ loop r2
    | Star r -> "(" ^ loop r ^ ")*"
  in
  "[" ^ loop r ^ "]"

let union r1 r2 =
  match (r1, r2) with
  | Empty, r | r, Empty -> r
  | _ -> if compare r1 r2 < 0 then Union (r1, r2) else Union (r2, r1)

let concat r1 r2 =
  match (r1, r2) with
  | Empty, _ | _, Empty -> Empty
  | Epsilon, r | r, Epsilon -> r
  | _ -> Concat (r1, r2)

let rec split = function
  | [] -> Seq.return ([], [])
  | x :: xs ->
      Seq.cons
        ([], x :: xs)
        (Seq.flat_map (fun (l1, l2) -> Seq.return (x :: l1, l2)) (split xs))

let rec accept l = function
  | Epsilon -> l = []
  | Letter c -> l = [c]
  | Empty -> false
  | Union (r1, r2) -> accept l r1 || accept l r2
  | Concat (r1, r2) ->
      Seq.exists (fun (a, b) -> accept a r1 && accept b r2) (split l)
  | Star r -> accept_star l r

and accept_star l r =
  l = []
  || Seq.exists
       (fun (prefix, suffix) ->
         prefix <> [] && accept prefix r && accept_star suffix r )
       (split l)

let match_ s = s |> String.to_seq |> List.of_seq |> accept

let to_dot ?(pp_letter = Format.sprintf "%c") name regexp =
  let name =
    if Filename.extension name = ".dot" then name else name ^ ".dot"
  in
  let get_id =
    let cpt = ref 0 in
    fun () ->
      let i = !cpt in
      incr cpt ; i
  in
  let oc = open_out name in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "digraph regexp {@\n" ;
  Format.fprintf fmt "  node [shape=circle];@\n" ;
  let rec visit id = function
    | Empty ->
        Format.fprintf fmt "  node%d [label=\"∅\", shape=plaintext];@\n" id
    | Epsilon ->
        Format.fprintf fmt "  node%d [label=\"ε\", shape=plaintext];@\n" id
    | Letter c ->
        Format.fprintf fmt "  node%d [label=\"%s\"];\n" id (pp_letter c)
    | Union (r1, r2) ->
        Format.fprintf fmt "  node%d [label=\"∪\", shape=diamond];@\n" id ;
        let id1 = get_id () in
        let id2 = get_id () in
        visit id1 r1 ;
        visit id2 r2 ;
        Format.fprintf fmt "  node%d -> node%d;@\n" id id1 ;
        Format.fprintf fmt "  node%d -> node%d;@\n" id id2
    | Concat (r1, r2) ->
        Format.fprintf fmt "  node%d [label=\"·\", shape=diamond];@\n" id ;
        let id1 = get_id () in
        let id2 = get_id () in
        visit id1 r1 ;
        visit id2 r2 ;
        Format.fprintf fmt "  node%d -> node%d;@\n" id id1 ;
        Format.fprintf fmt "  node%d -> node%d;@\n" id id2
    | Star r ->
        Format.fprintf fmt "  node%d [label=\"*\", shape=diamond];@\n" id ;
        let id1 = get_id () in
        visit id1 r ;
        Format.fprintf fmt "  node%d -> node%d;@\n" id id1
  in
  visit (get_id ()) regexp ;
  Format.fprintf fmt "}@." ;
  close_out oc

let to_png ?(verbose = false) ?(keep_dot = false) ?pp_letter name regexp =
  let name =
    if Filename.extension name = ".png" then Filename.chop_extension name
    else name
  in
  to_dot ?pp_letter name regexp ;
  Format.asprintf "dot -Tpng %s.dot -o %s.png" name name
  |> Sys.command |> ignore ;
  if not keep_dot then
    Format.asprintf "rm %s.dot" name |> Sys.command |> ignore ;
  if verbose then Format.printf "Regexp tree saved in %s.png@." name
