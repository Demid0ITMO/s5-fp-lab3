(* ========== Вспомогательные функции ========== *)
let rec take n = function
  | [] -> []
  | x :: xs -> if n > 0 then x :: take (n - 1) xs else []

let split_by_delimiters line =
  let delimiters = [ ';'; '\t'; ' '; '\n'; '\r' ] in
  let is_delimiter c = List.mem c delimiters in
  let rec split acc current = function
    | [] -> if current = "" then List.rev acc else List.rev (current :: acc)
    | c :: rest when is_delimiter c ->
        if current = "" then split acc "" rest
        else split (current :: acc) "" rest
    | c :: rest -> split acc (current ^ String.make 1 c) rest
  in
  split [] "" (List.init (String.length line) (String.get line))

let try_float_of_string s = try Some (float_of_string s) with _ -> None

(* ========== Парсинг и валидация ========== *)

let parse_point line =
  let line = String.trim line in
  if line = "" then None
  else
    let parts = split_by_delimiters line |> List.filter (fun s -> s <> "") in
    match parts with
    | [ xs; ys ] -> (
        match (try_float_of_string xs, try_float_of_string ys) with
        | Some x, Some y -> Some (x, y)
        | _ ->
            prerr_endline ("Warning: cannot parse line: \"" ^ line ^ "\"");
            None)
    | _ ->
        prerr_endline ("Warning: cannot parse line: \"" ^ line ^ "\"");
        None

let validate_order prev_point new_point =
  match (prev_point, new_point) with
  | Some (prev_x, _), Some (new_x, _) when prev_x >= new_x ->
      Printf.eprintf "Warning: input not sorted by x: %.6g >= %.6g\n" prev_x
        new_x
  | _ -> ()

(* ========== Форматирование вывода ========== *)

let fmt_num x =
  let s = Printf.sprintf "%.6f" x in
  let len = String.length s in
  let rec remove_zeros i =
    if i >= len then s
    else
      match s.[i] with
      | '.' ->
          let rec remove_trailing j =
            if j <= i + 1 then s
            else if s.[j - 1] = '0' then remove_trailing (j - 1)
            else if s.[j - 1] = '.' then String.sub s 0 (j - 1)
            else String.sub s 0 j
          in
          remove_trailing len
      | _ -> remove_zeros (i + 1)
  in
  remove_zeros 0

let fmt_point algo x y = Printf.sprintf "%s: %s %s" algo (fmt_num x) (fmt_num y)

let print_points points =
  let sorted_points =
    List.sort (fun (_, x1, _) (_, x2, _) -> compare x1 x2) points
  in
  List.iter
    (fun (algo, x, y) -> print_endline (fmt_point algo x y))
    sorted_points;
  flush stdout

(* ========== Генерация сетки ========== *)

let grid ?(inclusive = true) x_start x_end step =
  let eps = 1e-9 *. max 1.0 (abs_float x_end) in
  let check =
    if inclusive then fun x -> x <= x_end +. eps else fun x -> x < x_end -. eps
  in
  let rec gen acc current =
    if check current then gen (current :: acc) (current +. step)
    else List.rev acc
  in
  gen [] x_start

(* ========== Линейная интерполяция ========== *)

type linear_state = {
  step : float;
  verbose : bool;
  prev_point : (float * float) option;
  prev_prev_point : (float * float) option;
  last_x : float option;
}

let linear_fn (x1, y1) (x2, y2) x =
  if x2 = x1 then y1 else y1 +. ((y2 -. y1) /. (x2 -. x1) *. (x -. x1))

let make_linear_state step _window_size verbose =
  { step; verbose; prev_point = None; prev_prev_point = None; last_x = None }

let process_linear state (px, py) =
  let open Printf in
  if state.verbose then eprintf "[linear] received point: (%.4g, %.4g)\n" px py;

  match state.prev_point with
  | None -> ({ state with prev_point = Some (px, py) }, [])
  | Some (prev_x, prev_y) ->
      let f = linear_fn (prev_x, prev_y) (px, py) in
      let start_x =
        match state.last_x with Some x -> x +. state.step | None -> prev_x
      in
      let xs = grid start_x px state.step ~inclusive:false in
      let output = List.map (fun x -> ("linear", x, f x)) xs in
      let last_x =
        match List.rev output with
        | [] -> state.last_x
        | (_, x, _) :: _ -> Some x
      in
      ( {
          state with
          prev_point = Some (px, py);
          prev_prev_point = state.prev_point;
          last_x;
        },
        output )

let finalize_linear state =
  if state.verbose then prerr_endline "[linear] finalizing...";

  match (state.prev_point, state.prev_prev_point) with
  | Some (px, py), Some (prev_x, prev_y) ->
      let f = linear_fn (prev_x, prev_y) (px, py) in
      let start_x =
        match state.last_x with Some x -> x +. state.step | None -> prev_x
      in
      let xs = grid start_x px state.step ~inclusive:true in
      let output = List.map (fun x -> ("linear", x, f x)) xs in
      if
        output = []
        ||
        match List.rev output with
        | (_, last_x, _) :: _ -> last_x < px -. 1e-9
        | _ -> true
      then output @ [ ("linear", px, py) ]
      else output
  | Some (px, py), None -> [ ("linear", px, py) ]
  | None, _ -> []

(* ========== Интерполяция Лагранжа ========== *)

let lagrange_basis points i x =
  let n = List.length points in
  let xi, _ = List.nth points i in
  let rec product acc j =
    if j >= n then acc
    else if i = j then product acc (j + 1)
    else
      let xj, _ = List.nth points j in
      product (acc *. (x -. xj) /. (xi -. xj)) (j + 1)
  in
  product 1.0 0

let lagrange_poly points =
  let n = List.length points in
  fun x ->
    let rec sum acc i =
      if i >= n then acc
      else
        let _, yi = List.nth points i in
        let li = lagrange_basis points i x in
        sum (acc +. (yi *. li)) (i + 1)
    in
    sum 0.0 0

(* ========== Интерполяция Ньютона ========== *)

let divided_differences points =
  let n = List.length points in
  let xs = Array.of_list (List.map fst points) in
  let ys = Array.of_list (List.map snd points) in

  let table = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    table.(0).(i) <- ys.(i)
  done;

  for k = 1 to n - 1 do
    for i = 0 to n - k - 1 do
      table.(k).(i) <-
        (table.(k - 1).(i + 1) -. table.(k - 1).(i)) /. (xs.(i + k) -. xs.(i))
    done
  done;

  Array.init n (fun i -> table.(i).(0))

let newton_poly points =
  let n = List.length points in
  let xs = Array.of_list (List.map fst points) in
  let coefs = divided_differences points in

  fun x ->
    let rec evaluate i acc term =
      if i >= n then acc
      else evaluate (i + 1) (acc +. (coefs.(i) *. term)) (term *. (x -. xs.(i)))
    in
    evaluate 0 0.0 1.0

(* ========== Окно-ориентированные алгоритмы ========== *)

type window_state = {
  algo : string;
  step : float;
  window_size : int;
  verbose : bool;
  poly_fn : (float * float) list -> float -> float;
  points : (float * float) list;
  last_x : float option;
  first_window : bool;
}

let make_window_state algo step window_size verbose poly_fn =
  {
    algo;
    step;
    window_size;
    verbose;
    poly_fn;
    points = [];
    last_x = None;
    first_window = true;
  }

let process_window state (px, py) =
  let open Printf in
  if state.verbose then
    eprintf "[%s] received point: (%.4g, %.4g), buffer size: %d\n" state.algo px
      py
      (List.length state.points + 1);

  let points' = (px, py) :: state.points in
  let n = List.length points' in

  if n < state.window_size then ({ state with points = points' }, [])
  else
    let window = List.rev (take state.window_size points') in
    let xs_win = List.map fst window in
    let mid_idx = state.window_size / 2 in
    let x_mid = List.nth xs_win mid_idx in
    let start_x =
      match (state.first_window, state.last_x) with
      | true, _ -> List.hd xs_win
      | false, Some last_x -> last_x +. state.step
      | false, None -> List.hd xs_win
    in
    let xs = grid start_x x_mid state.step in
    let f = state.poly_fn window in
    let output = List.map (fun x -> (state.algo, x, f x)) xs in
    let last_x =
      match List.rev output with [] -> state.last_x | (_, x, _) :: _ -> Some x
    in
    if state.verbose then
      eprintf "[%s] window: [%s], outputting %d points\n" state.algo
        (String.concat "; " (List.map (sprintf "%.4g") xs_win))
        (List.length output);

    ({ state with points = points'; last_x; first_window = false }, output)

let finalize_window state =
  let open Printf in
  if state.verbose then
    eprintf "[%s] finalizing with %d points...\n" state.algo
      (List.length state.points);

  if List.length state.points >= state.window_size then
    let window = List.rev (take state.window_size state.points) in
    let xs_win = List.map fst window in
    let x_max = List.nth xs_win (state.window_size - 1) in
    let start_x =
      match state.last_x with
      | Some last_x -> last_x +. state.step
      | None -> List.hd xs_win
    in
    let xs = grid start_x x_max state.step in
    let f = state.poly_fn window in
    let output = List.map (fun x -> (state.algo, x, f x)) xs in
    if
      output = []
      ||
      match List.rev output with
      | (_, last_x, _) :: _ -> last_x < x_max -. 1e-9
      | _ -> true
    then output @ [ (state.algo, x_max, f x_max) ]
    else output
  else []

let make_lagrange_state step window_size verbose =
  make_window_state "lagrange" step window_size verbose lagrange_poly

let make_newton_state step window_size verbose =
  make_window_state "newton" step window_size verbose newton_poly

(* ========== Обработка потока ========== *)

type algorithm = {
  state : [ `Linear of linear_state | `Window of window_state ];
  process :
    [ `Linear of linear_state | `Window of window_state ] ->
    float * float ->
    [ `Linear of linear_state | `Window of window_state ]
    * (string * float * float) list;
  finalize :
    [ `Linear of linear_state | `Window of window_state ] ->
    (string * float * float) list;
}

let process_stream algorithms lines verbose =
  let rec loop lines (states, outputs) last_point =
    match lines with
    | [] ->
        if verbose then
          prerr_endline "[main] EOF reached, finalizing all algorithms...";
        let final_outputs =
          List.flatten
            (List.map2
               (fun algo state -> algo.finalize state)
               algorithms states)
        in
        List.rev_append outputs final_outputs
    | line :: rest -> (
        match parse_point line with
        | None -> loop rest (states, outputs) last_point
        | Some point ->
            validate_order last_point (Some point);
            let new_states_and_outputs =
              List.map2
                (fun algo state ->
                  let new_state, out = algo.process state point in
                  (new_state, out))
                algorithms states
            in
            let new_states = List.map fst new_states_and_outputs in
            let new_outputs =
              List.flatten (List.map snd new_states_and_outputs)
            in
            let all_outputs = new_outputs @ outputs in
            print_points all_outputs;
            loop rest (new_states, []) (Some point))
  in
  loop lines (List.map (fun a -> a.state) algorithms, []) None

(* ========== CLI и основная программа ========== *)

let usage =
  "Stream interpolation - Lab 3\n\n\
   Usage: ./interpolation [options]\n\n\
   Options:\n\
  \  -l, --linear       Use linear interpolation\n\
  \  -L, --lagrange     Use Lagrange interpolation\n\
  \  -N, --newton       Use Newton interpolation\n\
  \  -n, --points N     Number of points for interpolation window (default: 4)\n\
  \  -s, --step STEP    Sampling step for result points (required)\n\
  \  -v, --verbose      Enable verbose output for debugging\n\
  \  -h, --help         Show help\n\n\
   Examples:\n\
  \  echo -e '0 0\\n1 1\\n2 4' | ./interpolation --linear --step 0.5\n\
  \  ./interpolation --newton -n 4 --step 0.5 < data.txt\n\
  \  ./interpolation --linear --lagrange --step 1 < data.txt\n\n\
   Input format: lines with \"x y\", \"x;y\" or \"x\\ty\", sorted by x \
   ascending.\n\
   Output format: <algorithm>: <x> <y>\n"

let rec parse_args argv linear lagrange newton points step verbose =
  match argv with
  | [] -> (linear, lagrange, newton, points, step, verbose)
  | "-l" :: rest | "--linear" :: rest ->
      parse_args rest true lagrange newton points step verbose
  | "-L" :: rest | "--lagrange" :: rest ->
      parse_args rest linear true newton points step verbose
  | "-N" :: rest | "--newton" :: rest ->
      parse_args rest linear lagrange true points step verbose
  | "-n" :: rest -> (
      match rest with
      | n :: rest2 ->
          parse_args rest2 linear lagrange newton (int_of_string n) step verbose
      | _ -> failwith "Missing argument for -n")
  | "--points" :: rest -> (
      match rest with
      | n :: rest2 ->
          parse_args rest2 linear lagrange newton (int_of_string n) step verbose
      | _ -> failwith "Missing argument for --points")
  | "-s" :: rest -> (
      match rest with
      | s :: rest2 ->
          parse_args rest2 linear lagrange newton points (float_of_string s)
            verbose
      | _ -> failwith "Missing argument for -s")
  | "--step" :: rest -> (
      match rest with
      | s :: rest2 ->
          parse_args rest2 linear lagrange newton points (float_of_string s)
            verbose
      | _ -> failwith "Missing argument for --step")
  | "-v" :: rest | "--verbose" :: rest ->
      parse_args rest linear lagrange newton points step true
  | "-h" :: _ | "--help" :: _ ->
      print_endline usage;
      exit 0
  | x :: _ -> failwith ("Unknown option: " ^ x)

let () =
  try
    let linear, lagrange, newton, points, step, verbose =
      parse_args
        (List.tl (Array.to_list Sys.argv))
        false false false 4 0.0 false
    in

    if not (linear || lagrange || newton) then (
      prerr_endline
        "Error: Specify at least one algorithm: --linear, --lagrange, or \
         --newton";
      prerr_endline "";
      prerr_endline usage;
      exit 1);

    if step = 0.0 then (
      prerr_endline "Error: Sampling step must be specified with --step";
      prerr_endline "";
      prerr_endline usage;
      exit 1);

    if verbose then (
      Printf.eprintf "[config] algorithms: linear=%b lagrange=%b newton=%b\n"
        linear lagrange newton;
      Printf.eprintf "[config] step=%.6g, window-size=%d\n" step points);

    let algorithms = ref [] in
    if linear then
      algorithms :=
        {
          state = `Linear (make_linear_state step points verbose);
          process =
            (fun state point ->
              match state with
              | `Linear state ->
                  let new_state, out = process_linear state point in
                  (`Linear new_state, out)
              | _ -> assert false);
          finalize =
            (fun state ->
              match state with
              | `Linear state -> finalize_linear state
              | _ -> assert false);
        }
        :: !algorithms;

    if lagrange then
      algorithms :=
        {
          state = `Window (make_lagrange_state step points verbose);
          process =
            (fun state point ->
              match state with
              | `Window state ->
                  let new_state, out = process_window state point in
                  (`Window new_state, out)
              | _ -> assert false);
          finalize =
            (fun state ->
              match state with
              | `Window state -> finalize_window state
              | _ -> assert false);
        }
        :: !algorithms;

    if newton then
      algorithms :=
        {
          state = `Window (make_newton_state step points verbose);
          process =
            (fun state point ->
              match state with
              | `Window state ->
                  let new_state, out = process_window state point in
                  (`Window new_state, out)
              | _ -> assert false);
          finalize =
            (fun state ->
              match state with
              | `Window state -> finalize_window state
              | _ -> assert false);
        }
        :: !algorithms;

    let lines = ref [] in
    (try
       while true do
         lines := input_line stdin :: !lines
       done
     with End_of_file -> ());

    let _ = process_stream (List.rev !algorithms) (List.rev !lines) verbose in
    ()
  with
  | Failure msg ->
      prerr_endline ("Error: " ^ msg);
      prerr_endline "";
      prerr_endline usage;
      exit 1
  | e ->
      prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
      exit 1
