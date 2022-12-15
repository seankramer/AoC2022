open Printf
open Core

let test str = printf "%s\n%!" str

let strip_str (s: string) : string =
  let sstr = String.strip s in
  sstr

let cvt_to_int (s : string) : int =
  let is_mpty = String.is_empty s in
  if is_mpty then -1 else Int.of_string s

let rec split_while_pos (l : int list) (acc : int list) : int list * int list =
  match l with
  | [] ->
      begin
        let () = printf "done\n%!" in
        acc, l
      end
  | hd :: rest ->
      begin
        if Int.is_positive hd then
          let () = printf "keep: %d\n%!" hd in
          let n_acc = List.append acc [hd] in
          split_while_pos rest n_acc
        else
          let () = printf "discard\n%!" in
          acc, rest
      end

let rec chunk_lst (l : int list) (acc : int list list) : int list list =
  match l with
  | [] -> List.rev acc
  | _ ->
      begin
        let chunk, rest = split_while_pos l [] in
        let newer_acc = chunk :: acc in
        chunk_lst rest newer_acc
      end

let rd_input (fname : string) : string =
  let doc = In_channel.read_all fname in
  let lst_line = String.split_lines doc in
(*
  let _ = List.iter lst_line ~f:(fun str -> printf "%s|\n%!" str) in
  let stripped_lines = List.map lst_line ~f:strip_str in
  let _ = List.iter stripped_lines ~f:(fun str -> printf "%s|\n%!" str) in
*)
  let num_val = List.map lst_line ~f:cvt_to_int in
  let acc = [] in
  let rez = chunk_lst num_val acc in
  let _ = List.iter rez ~f:(
      fun slst ->
        let () = List.iter slst ~f:(
            fun n -> printf "%d, %!" n)
        in
        print_endline ""
    )
  in
(*
  let _ = List.iter num_val ~f:(fun it -> printf "%d\n%!" it) in
*)
  ""

let () =
  test "testing, testing";
  let _ = rd_input "num.txt" in
  ()

