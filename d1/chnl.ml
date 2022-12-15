open Printf
open Core

let test str = printf "%s\n%!" str

let strip_str (s: string) : string =
  let sstr = String.strip s in
  sstr

let empty_str (s : string) : int  =
  let elst = List.take_while ls ~f:(fun sb -> )

let rd_input (fname : string) : string =
  let doc = In_channel.read_all fname in
  let lst_line = String.split_lines doc in
  let _ = List.iter lst_line ~f:(fun str -> printf "%s|\n%!" str) in
(*
  let stripped_lines = List.map lst_line ~f:strip_str in
  let _ = List.iter stripped_lines ~f:(fun str -> printf "%s|\n%!" str) in
*)
  ""

let () =
  test "testing, testing";
  let _ = rd_input "num.txt" in
  ()

