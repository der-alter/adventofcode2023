open Core

(* let max_red = 12 *)
(* let max_green = 13 *)
(* let max_blue = 14 *)

let concat_grabs s =
  String.lstrip s
  |> String.split ~on:';'
  |> List.map ~f:(fun x -> String.split x ~on:',' |> List.map ~f:String.lstrip)
  |> List.map ~f:(fun xs -> List.map xs ~f:(fun x -> String.split x ~on:' '))
  |> List.map ~f:(fun xs ->
         List.map xs ~f:(fun xs ->
             let nb = List.hd_exn xs in
             let nb = if String.is_empty nb then 0 else Int.of_string nb in
             let color =
               match xs with
               | [] -> ""
               | _ :: tl -> (
                 match tl with
                 | [] -> ""
                 | hd :: _ -> hd)
             in
             nb, color))
  |> List.concat

let format_games input =
  String.split input ~on:'\n'
  |> List.map ~f:(fun x -> String.split x ~on:':')
  |> List.map ~f:(fun xs ->
         let game = String.lstrip (List.hd_exn xs) |> String.split ~on:' ' in
         let game =
           match game with
           | [] -> "0"
           | _ :: tl -> (
             match tl with
             | [] -> "0"
             | hd :: _ -> hd)
         in
         let game = Int.of_string game in
         let grabs =
           match xs with
           | [] -> ""
           | _ :: tl -> (
             match tl with
             | [] -> ""
             | hd :: _ -> String.lstrip hd)
         in
         game, concat_grabs grabs)

let rec check_game xs =
  match xs with
  | [] -> true
  | hd :: tl -> (
    let nb, color = hd in
    match color with
    | "red" -> if nb > 12 then false else check_game tl
    | "green" -> if nb > 13 then false else check_game tl
    | "blue" -> if nb > 14 then false else check_game tl
    | _ -> false)

let filter_possible_games xs =
  List.map xs ~f:(fun (game, cubes) -> if check_game cubes then game else 0)

let sum lst = List.fold lst ~init:0 ~f:( + )

let () =
  let input = In_channel.read_all "day02_input.txt" in
  let games = format_games input |> filter_possible_games in
  printf "\n%d\n" (sum games)
