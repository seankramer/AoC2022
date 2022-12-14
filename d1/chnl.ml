open Printf
open Core

let test str = printf "%s\n%!" str

let rd_input (fname : string) : string =
  let doc = In_channel.read_all fname in
  let lst_line = String.split_lines doc in
  let _ = List.iter lst_line ~f:(fun str -> printf "%s\n%!" str) in
  ""

let () =
  test "testing, testing";
  let input_str = rd_input "input.txt" in
  ()

