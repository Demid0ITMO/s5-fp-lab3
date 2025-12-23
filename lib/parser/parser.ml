(** Copyright 2025-2025, Lab3 Ryazanov Demid 367522 *)

(** SPDX-License-Identifier: NONE *)

type config = { interpol_alg : string; step : float; n : int; delim : string }

let default_config = { interpol_alg = "both"; step = 0.5; n = 4; delim = " " }
let config_ref = ref default_config

let both config =
  config.interpol_alg = "both"
  || (config.interpol_alg <> "linear" && config.interpol_alg <> "newton")

let linear config = config.interpol_alg = "linear" || both config
let newton config = config.interpol_alg = "newton" || both config
let step config = config.step
let n config = if newton config || both config then config.n else 2
let delim config = config.delim
let update_config f = config_ref := f !config_ref

let speclist =
  [
    ( "--alg",
      Arg.String (fun s -> update_config (fun c -> { c with interpol_alg = s })),
      "Algorithm of interpolation newton/linear/both, another - ignore, \
       (default is both)" );
    ( "--step",
      Arg.Float (fun f -> update_config (fun c -> { c with step = f })),
      Printf.sprintf "Interpolation step, float > 0, (default is %f)"
        default_config.step );
    ( "-n",
      Arg.Int (fun i -> update_config (fun c -> { c with n = i })),
      Printf.sprintf
        "Count of points in Newton interpolation, int > 0, (default is %d)"
        default_config.n );
    ( "--delimeter",
      Arg.String (fun s -> update_config (fun c -> { c with delim = s })),
      Printf.sprintf "Delimeter between x and y in input (default is '%s')"
        default_config.delim );
  ]

let usage_msg =
  "\n\
   Lab3 usage smaples:\n\
   ./lab3 --linear -n 3 --step 0.7\n\
   ./lab3 --both -n 6 --step 0.3\n\
   ./lab3 --newton\n\
   ./lab3\n"

type parse_result = (float * float, string) result

let parse_point str config =
  try
    let p = "%f" ^ delim config in
    let pattern = p ^ "%f" in
    Ok
      (Scanf.sscanf str (Scanf.format_from_string pattern "%f%f") (fun x y ->
           (x, y)))
  with
  | Scanf.Scan_failure _ -> Error ("Cannot parse point: " ^ str)
  | End_of_file -> Error ("Unexpected end of input: " ^ str)

let get_config () = !config_ref
