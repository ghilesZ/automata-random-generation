type 'a t = ('a, int) Hashtbl.t

let create () = Hashtbl.create 17

let add tbl x =
  let n = match Hashtbl.find_opt tbl x with None -> 0 | Some c -> c in
  Hashtbl.replace tbl x (n + 1)

let count tbl x = match Hashtbl.find_opt tbl x with None -> 0 | Some c -> c

let to_list tbl = Hashtbl.to_seq tbl |> List.of_seq

let richness tbl = Hashtbl.length tbl

let simpson tbl =
  let hist = to_list tbl in
  let total = List.fold_left (fun acc (_, c) -> acc + c) 0 hist in
  if total = 0 then 0.0
  else
    List.fold_left
      (fun acc (_, c) ->
        let p = float c /. float total in
        acc +. (p *. p) )
      0.0 hist

let is_uniform ?(rel_margin = 0.05) tbl =
  let hist = to_list tbl in
  match hist with
  | [] | [_] -> true
  | _ ->
      let counts = List.map snd hist in
      let avg =
        float (List.fold_left ( + ) 0 counts) /. float (List.length counts)
      in
      List.for_all
        (fun c ->
          let diff = abs_float (float c -. avg) in
          diff /. avg <= rel_margin )
        counts

let most_common tbl =
  to_list tbl
  |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1)
  |> function [] -> None | hd :: _ -> Some hd

(* you’d need a small plotting backend; placeholder *)
let to_svg_histogram filename pp_elt tbl =
  let hist = to_list tbl in
  if hist = [] then invalid_arg "to_svg_histogram: empty histogram"
  else
    let max_count = List.fold_left (fun acc (_, c) -> max acc c) 0 hist in
    let bar_width = 40 in
    let bar_spacing = 20 in
    let width = (bar_width + bar_spacing) * List.length hist in
    let height = 300 in
    let scale =
      if max_count = 0 then 1.0 else float (height - 50) /. float max_count
    in
    let oc = open_out (filename ^ ".svg") in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt
      "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>@."
      width height ;
    List.sort (fun (_e1, c1) (_e2, c2) -> compare c2 c1) hist
    |> List.iteri (fun i (elt, count) ->
           let x = i * (bar_width + bar_spacing) in
           let bar_h = int_of_float (float count *. scale) in
           let y = height - bar_h - 20 in
           (* draw bar *)
           Format.fprintf fmt
             "<rect x='%d' y='%d' width='%d' height='%d' \
              fill='steelblue'/>@."
             x y bar_width bar_h ;
           (* draw count above bar *)
           Format.fprintf fmt
             "<text x='%d' y='%d' font-size='12' \
              text-anchor='middle'>%d</text>@."
             (x + (bar_width / 2))
             (y - 5) count ;
           (* draw label below bar *)
           Format.fprintf fmt
             "<text x='%d' y='%d' font-size='12' \
              text-anchor='middle'>%a</text>@."
             (x + (bar_width / 2))
             (height - 5) pp_elt elt ) ;
    Format.fprintf fmt "</svg>@." ;
    close_out oc

let pp_histogram pp_elt fmt tbl =
  let hist = to_list tbl in
  List.iter (fun (x, c) -> Format.fprintf fmt "%a -> %d@." pp_elt x c) hist

let histogram generator nb =
  let hist = create () in
  if nb > 0 then
    for _ = 1 to nb do
      let sample = generator () in
      add hist sample
    done ;
  hist
