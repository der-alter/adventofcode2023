open Core

let rec fst_digit_exn xs = 
    match xs with
    | '1'..'9'::_ -> List.hd xs
    | _::tl -> fst_digit_exn tl
    | [] -> Some('0')

let find_calibration s = 
  let xs = String.to_list s in
  let fst = Option.value_exn (fst_digit_exn xs) in
  let lst = Option.value_exn (fst_digit_exn (List.rev xs)) in
  let num = (String.of_char fst) ^ (String.of_char lst) in
  Int.of_string num

let sum lst = List.fold lst ~init:0 ~f:(+)

let () =
  let input = In_channel.read_all "day01_input.txt" in
  let xs = String.split input ~on:'\n' in
  let calibrations = (List.map xs ~f:find_calibration) in
  printf "\n%d\n" (sum calibrations)
