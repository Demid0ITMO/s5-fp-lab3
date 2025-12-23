(** Copyright 2025-2025, Lab3 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

open Interpolation

let rec gen (module I : InterpolationSig) current_x biggest_x step points =
  if current_x >= biggest_x then (current_x, "")
  else
    let next_x = current_x +. step in
    if next_x > biggest_x then (current_x, "")
    else
      let y = I.eval next_x points in
      let last_x, rest = gen (module I) next_x biggest_x step points in
      (last_x, Printf.sprintf "%s > %g %g\n%s" I.name next_x y rest)

let rec process last_x points =
  try
    let input = read_line () in
    let config = Parser.get_config () in

    match Parser.parse_point input config with
    | Error msg ->
        Printf.eprintf "Error: %s\n" msg;
        process last_x points
    | Ok (x, y) ->
        let new_points =
          match points with
          | [] -> [ (x, y) ]
          | _ ->
              let n = Parser.n config in
              if List.length points >= n then List.tl points @ [ (x, y) ]
              else points @ [ (x, y) ]
        in

        let start_x = match last_x with Some v -> v | None -> x in

        let linear =
          if Parser.linear config && List.length new_points >= 2 then
            gen (module Linear) start_x x (Parser.step config) new_points
          else (start_x, "")
        in

        let newton =
          if Parser.newton config && List.length new_points >= Parser.n config
          then gen (module Newton) start_x x (Parser.step config) new_points
          else (start_x, "")
        in

        let xl, ol = linear in
        let xn, on = newton in
        let new_last = if xl > xn then xl else xn in

        Printf.printf "%s" (ol ^ on);

        process (Some new_last) new_points
  with End_of_file -> ()

let () =
  Arg.parse Parser.speclist (fun _ -> ()) Parser.usage_msg;

  let config = Parser.get_config () in

  if Parser.step config <= 0. then failwith "step must be > 0";
  if Parser.n config <= 0 then failwith "n must be > 0";

  process None []
