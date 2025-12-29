(** Copyright 2025-2025, Lab3 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

module type InterpolationSig = sig
  val name : string
  val eval : float -> (float * float) list -> float
end

module Linear : InterpolationSig = struct
  let name = "linear"

  let eval x = function
    | [] -> 0.0
    | [ (_, y1) ] -> y1
    | (x1, y1) :: (x2, y2) :: _ ->
        if x <= x1 then y1
        else if x >= x2 then y2
        else
          let slope = (y2 -. y1) /. (x2 -. x1) in
          y1 +. (slope *. (x -. x1))
end

module Newton : InterpolationSig = struct
  let name = "newton"

  let rec divided_diff = function
    | [] | [ _ ] -> []
    | (x1, y1) :: (x2, y2) :: rest ->
        let diff = (y2 -. y1) /. (x2 -. x1) in
        diff :: divided_diff ((x2, y2) :: rest)

  let build_diff_table points =
    let rec build_table current_points table =
      match current_points with
      | [] | [ _ ] -> table
      | _ -> (
          match divided_diff current_points with
          | [] -> table
          | hd :: _ ->
              build_table (List.tl current_points)
                ((hd, current_points) :: table))
    in
    build_table points []

  let eval x = function
    | [] -> 0.0
    | points -> (
        let diff_table = build_diff_table points in
        let rec evaluate term_idx acc product =
          if term_idx >= List.length diff_table then acc
          else
            let diff, corresponding_points = List.nth diff_table term_idx in
            let new_product =
              match corresponding_points with
              | [] -> product
              | (xi, _) :: _ -> product *. (x -. xi)
            in
            evaluate (term_idx + 1) (acc +. (diff *. new_product)) new_product
        in

        match points with [] -> 0.0 | (_, y0) :: _ -> y0 +. evaluate 0 0.0 1.0)
end
