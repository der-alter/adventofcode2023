open Core

let rec keep = function
  | [] -> []
  | hd :: tl -> (
    match hd with
    | Some x -> Char.to_string x :: keep tl
    | None -> keep tl)

let find_calibration s =
  let sa = String.to_array s in
  let sa =
    Array.map sa ~f:(fun x ->
        match x with
        | '1' .. '9' -> Some x
        | _ -> None)
  in
  let patterns =
    [ "one", '1';
      "two", '2';
      "three", '3';
      "four", '4';
      "five", '5';
      "six", '6';
      "seven", '7';
      "eight", '8';
      "nine", '9' ]
  in
  let () =
    List.iter patterns ~f:(fun (pattern, digit) ->
        let found = String.substr_index_all s ~pattern ~may_overlap:true in
        List.iter found ~f:(fun x -> sa.(x) <- Some digit))
  in
  let digits = Array.to_list sa in
  let all = keep digits in
  match all with
  | [] -> 0
  (* ^ TODO: the list may be empty, why? *)
  | xs ->
    let fst = List.hd_exn xs in
    let lst = List.hd_exn (List.rev xs) in
    Int.of_string (fst ^ lst)

let sum lst = List.fold lst ~init:0 ~f:( + )

let () =
  let input = In_channel.read_all "day01_input.txt" in
  let xs = String.split input ~on:'\n' in
  let calibrations = List.map xs ~f:find_calibration in
  printf "\n%d\n" (sum calibrations)
