type 'a t = Empty | Unary of 'a * 'a t | Binary of 'a * 'a t * 'a t

let imbalance_score tree =
  let rec dfs node =
    match node with
    | Empty -> (0, 0)
    | Unary (_, child) ->
        let lh, ls = dfs child in
        let height = lh + 1 in
        let imbalance = lh in
        (* rhs is 0 for unary *)
        let total_score = ls + imbalance in
        (height, total_score)
    | Binary (_, left, right) ->
        let lh, ls = dfs left in
        let rh, rs = dfs right in
        let height = 1 + max lh rh in
        let imbalance = abs (lh - rh) in
        let total_score = ls + rs + imbalance in
        (height, total_score)
  in
  let _, score = dfs tree in
  score

let imbalance_percentage tree =
  let rec count_nodes t =
    match t with
    | Empty -> 0
    | Unary (_, c) -> 1 + count_nodes c
    | Binary (_, l, r) -> 1 + count_nodes l + count_nodes r
  in
  let n = count_nodes tree in
  let score = imbalance_score tree in
  let max_possible = n * (n - 1) / 2 in
  if max_possible = 0 then 0.0
  else float_of_int score /. float_of_int max_possible *. 100.0

let make_node probability_binary payload =
  if Random.float 1. < probability_binary then Binary (payload, Empty, Empty)
  else Unary (payload, Empty)

let rec insert (p_bin : float) bu x =
  match bu with
  | Empty -> make_node p_bin x
  | Unary (x', bu) -> Unary (x', insert p_bin bu x)
  | Binary (x', bu1, bu2) ->
      if x < x' then Binary (x', insert p_bin bu1 x, bu2)
      else Binary (x', bu1, insert p_bin bu2 x)

let bust_of_list (p_bin : float) (ints : int list) : int t =
  List.fold_left (insert p_bin) Empty ints

let shuffle_array (arr : 'a array) : unit =
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j) ;
    arr.(j) <- temp
  done

let random_permut n =
  let a = Array.init n Fun.id in
  shuffle_array a ; Array.to_list a

let rec print_tree fmt = function
  | Empty -> Format.fprintf fmt "@[<v>Empty@]"
  | Unary (x, t) -> Format.fprintf fmt "@[<v 2>Unary %d@,%a@]" x print_tree t
  | Binary (x, t1, t2) ->
      Format.fprintf fmt "@[<v 2>Binary %d@,%a@,%a@]" x print_tree t1
        print_tree t2

let to_dot_bust ?(pp_label = string_of_int) name tree =
  let name =
    if Filename.extension name = ".dot" then name else name ^ ".dot"
  in
  let new_id =
    let cpt = ref 0 in
    fun () ->
      let i = !cpt in
      incr cpt ; i
  in
  let ids = Hashtbl.create 64 in
  let get_id node =
    match Hashtbl.find_opt ids node with
    | Some id -> id
    | None ->
        let id = new_id () in
        Hashtbl.add ids node id ; id
  in
  let oc = open_out name in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "digraph bust {@\n" ;
  Format.fprintf fmt "  node [shape=circle];@\n" ;
  let rec visit = function
    | Empty -> ()
    | Unary (x, t1) as n -> (
        let id = get_id n in
        Format.fprintf fmt "  node%d [label=\"%s\", shape=ellipse];@\n" id
          (pp_label x) ;
        match t1 with
        | Empty -> ()
        | _ ->
            let id1 = get_id t1 in
            Format.fprintf fmt "  node%d -> node%d;@\n" id id1 ;
            visit t1 )
    | Binary (x, t1, t2) as n -> (
        let id = get_id n in
        Format.fprintf fmt "  node%d [label=\"%s\", shape=box];@\n" id
          (pp_label x) ;
        ( match t1 with
        | Empty -> ()
        | _ ->
            let id1 = get_id t1 in
            Format.fprintf fmt "  node%d -> node%d ;\n" id id1 ;
            visit t1 ) ;
        match t2 with
        | Empty -> ()
        | _ ->
            let id2 = get_id t2 in
            Format.fprintf fmt "  node%d -> node%d ;\n" id id2 ;
            visit t2 )
  in
  visit tree ; Format.fprintf fmt "}@." ; close_out oc

let to_png_bust ?(verbose = false) ?(keep_dot = false) ?pp_label name tree =
  let name =
    if Filename.extension name = ".png" then Filename.chop_extension name
    else name
  in
  to_dot_bust ?pp_label name tree ;
  Format.asprintf "dot -Tpng %s.dot -o %s.png" name name
  |> Sys.command |> ignore ;
  if not keep_dot then
    Format.asprintf "rm %s.dot" name |> Sys.command |> ignore ;
  if verbose then Format.printf "Tree saved in %s.png@." name

let regexp_of_bust alphabet bust =
  let rand_letter () =
    let len = List.length alphabet in
    if len = 0 then invalid_arg "alphabet must not be empty" ;
    List.nth alphabet (Random.int len)
  in
  let rand_bin_op a b =
    let open Regexp in
    if Random.bool () then Union (a, b) else Concat (a, b)
  in
  let rec aux = function
    | Unary (_, t) -> Regexp.Star (aux t)
    | Binary (_, l, r) -> rand_bin_op (aux l) (aux r)
    | _ ->
        (* leaf case: not Unary or Binary, and not Empty *)
        (* This handles odd structures, treat as leaf if no children *)
        Letter (rand_letter ())
  in
  aux bust

(* Random generation of bust tree with a given size (number of nodes) *)
let random_but_of_size p_bin size : 'a t =
  let gen =
    let id = ref 0 in
    fun () -> incr id ; !id
  in
  let rec loop size =
    if size <= 0 then Empty
    else if size = 1 then Unary (gen (), Empty)
    else if size = 2 then Binary (gen (), Empty, Empty)
    else if
      (* size > 1: decide unary or binary *)
      Random.float 1. < p_bin
    then
      (* Binary node: size = 1 (root) + left subtree + right subtree *)
      let left_size = Random.int (size - 1) in
      let right_size = size - 1 - left_size in
      Binary (gen (), loop left_size, loop right_size)
    else
      (* Unary node: size = 1 (root) + subtree *)
      Unary (gen (), loop (size - 1))
  in
  loop size

let random_bust_of_size p_bin size =
  let perm = random_permut size in
  bust_of_list p_bin perm

(* Global hashtable for memoizing counts *)
let count_trees =
  let memo : (int, Z.t) Hashtbl.t = Hashtbl.create 64 in
  let rec aux n =
    match Hashtbl.find_opt memo n with
    | Some c -> c
    | None ->
        let result =
          if n = 0 then Z.one
          else
            let count_unary = aux (n - 1) in
            let count_binary =
              let acc = ref Z.zero in
              for k = 0 to n - 1 do
                let cl = aux k in
                let cr = aux (n - 1 - k) in
                acc := Z.add !acc (Z.mul cl cr)
              done ;
              !acc
            in
            Z.add count_unary count_binary
        in
        Hashtbl.add memo n result ; result
  in
  aux

(* Uniformly sample one tree of size n, passing next_id for unique labels *)
let rec sample_tree n next_id =
  if n = 0 then (Empty, next_id)
  else
    let count_unary = count_trees (n - 1) in
    let count_binary =
      let acc = ref Z.zero in
      for k = 0 to n - 1 do
        let cl = count_trees k in
        let cr = count_trees (n - 1 - k) in
        acc := Z.add !acc (Z.mul cl cr)
      done ;
      !acc
    in
    let total = Z.add count_unary count_binary in
    let r =
      Q.mul (Q.of_bigint total) (Q.of_float (Random.float 1.)) |> Q.to_bigint
    in
    if Z.leq r count_unary then
      let subtree, next_id' = sample_tree (n - 1) (next_id + 1) in
      (Unary (next_id, subtree), next_id')
    else
      (* Binary case: pick (k, n - 1 - k) proportionally *)
      let rec find_k k acc =
        let cl = count_trees k in
        let cr = count_trees (n - 1 - k) in
        let w = Z.mul cl cr in
        let acc' = Z.add acc w in
        if Z.leq r (Z.add count_unary acc') then k else find_k (k + 1) acc'
      in
      let k = find_k 0 Z.zero in
      let lsize = k in
      let rsize = n - 1 - k in
      let left, next_id1 = sample_tree lsize (next_id + 1) in
      let right, next_id2 = sample_tree rsize next_id1 in
      (Binary (next_id, left, right), next_id2)

(* Public API: generate random tree of given size *)
let random_uniform_tree size = fst (sample_tree size 1)
