open Core

let test str = printf "%s\n%!" str

let strip_str (s: string) : string =
  let sstr = String.strip s in
  sstr

let cvt_to_int (s : string) : int =
  let is_mpty = String.is_empty s in
  if is_mpty then -1 else Int.of_string s

let rec split_while_unq (l : int list) (acc : int list) ~(f : int -> bool) :
  int list * int list =
  match l with
  | [] -> acc, l
  | hd :: rest ->
      begin
        if f hd then
          let n_acc = List.append acc [hd] in
          split_while_unq rest n_acc ~f
        else
          acc, rest
      end

let rec chunk_lst (l : int list) (acc : int list list) : int list list =
  match l with
  | [] -> List.rev acc
  | _ ->
      begin
        let chunk, rest = split_while_unq l [] ~f:Int.is_positive in
        let newer_acc = chunk :: acc in
        chunk_lst rest newer_acc
      end

let rec ttl_sum (l : int list) : int =
  match l with
  | [] -> 0
  | hd :: rest -> hd + ttl_sum rest

let rec mx_val (l : int list) (acc : int) : int =
  match l with
  | [] -> acc
  | hd :: rest ->
      begin
        let n_acc = if hd > acc then hd else acc in mx_val rest n_acc
      end

let rd_input (fname : string) : string =
  let doc = In_channel.read_all fname in
  let lst_line = String.split_lines doc in
  let num_val = List.map lst_line ~f:cvt_to_int in
  let acc = [] in
  let rez = chunk_lst num_val acc in
  let ttl = List.map rez ~f:(fun sslst -> ttl_sum sslst) in
  let mx_v = mx_val ttl 0 in
  let _ = printf "part 1 solution: %d\n%!" mx_v in
  ""

let () =
  test "2022 Advent of Code, Day 1";
  let _ = rd_input "input.txt" in
  ()

