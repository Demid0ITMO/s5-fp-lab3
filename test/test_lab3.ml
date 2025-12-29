open OUnit2
open Interpolation

let epsilon = 1e-10

let assert_float_equal expected actual =
  assert_bool
    (Printf.sprintf "Expected: %f, got: %f" expected actual)
    (abs_float (expected -. actual) < epsilon)

let linear_empty_list_returns_zero =
  "linear empty list returns 0.0" >:: fun _ ->
  let x = 5.0 in
  let points = [] in
  let expected = 0.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_single_point_returns_its_y_value =
  "linear single point returns its y-value" >:: fun _ ->
  let x = 5.0 in
  let points = [ (2.0, 3.0) ] in
  let expected = 3.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_x_before_first_point_returns_y1 =
  "linear x before first point returns y1" >:: fun _ ->
  let x = 1.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 2.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_x_after_last_point_returns_y2 =
  "linear x after last point returns y2" >:: fun _ ->
  let x = 5.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 4.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_x_equals_first_point_returns_y1 =
  "linear x equals first point returns y1" >:: fun _ ->
  let x = 2.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 2.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_x_equals_second_point_returns_y2 =
  "linear x equals second point returns y2" >:: fun _ ->
  let x = 4.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 4.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_interpolation_between_two_points_midpoint =
  "linear interpolation between two points midpoint" >:: fun _ ->
  let x = 3.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 3.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_interpolation_with_negative_slope =
  "linear interpolation with negative slope" >:: fun _ ->
  let x = 2.0 in
  let points = [ (1.0, 4.0); (3.0, 2.0) ] in
  let expected = 3.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_interpolation_with_positive_slope =
  "linear interpolation with positive slope" >:: fun _ ->
  let x = 1.5 in
  let points = [ (1.0, 2.0); (2.0, 3.0) ] in
  let expected = 2.5 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_with_multiple_points_uses_first_two =
  "linear with multiple points uses first two" >:: fun _ ->
  let x = 1.5 in
  let points = [ (1.0, 2.0); (2.0, 3.0); (3.0, 4.0) ] in
  let expected = 2.5 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_non_linear_points_still_interpolates_linearly =
  "linear non-linear points still interpolates linearly" >:: fun _ ->
  let x = 2.0 in
  let points = [ (1.0, 1.0); (3.0, 5.0) ] in
  let expected = 3.0 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let linear_floating_point_precision_test =
  "linear floating point precision test" >:: fun _ ->
  let x = 2.5 in
  let points = [ (2.0, 2.0); (3.0, 3.0) ] in
  let expected = 2.5 in
  let result = Linear.eval x points in
  assert_float_equal expected result

let newton_empty_list_returns_zero =
  "newton empty list returns 0.0" >:: fun _ ->
  let x = 5.0 in
  let points = [] in
  let expected = 0.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let newton_single_point_returns_its_y_value =
  "newton single point returns its y-value" >:: fun _ ->
  let x = 5.0 in
  let points = [ (2.0, 3.0) ] in
  let expected = 3.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let newton_two_points_interpolation =
  "newton two points interpolation" >:: fun _ ->
  let x = 3.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 3.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let newton_two_points_extrapolation_before =
  "newton two points extrapolation before" >:: fun _ ->
  let x = 1.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 1.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let newton_two_points_extrapolation_after =
  "newton two points extrapolation after" >:: fun _ ->
  let x = 5.0 in
  let points = [ (2.0, 2.0); (4.0, 4.0) ] in
  let expected = 5.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let newton_three_points_extrapolation =
  "newton three points extrapolation" >:: fun _ ->
  let x = 3.0 in
  let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0) ] in
  let result = Newton.eval x points in
  assert_bool "Extrapolation should give value > 4" (result > 4.0)

let newton_repeated_x_values_handles =
  "newton repeated x values handles" >:: fun _ ->
  let x = 1.0 in
  let points = [ (0.0, 0.0); (0.0, 1.0); (2.0, 4.0) ] in
  try
    let _ = Newton.eval x points in
    assert true
  with
  | Division_by_zero -> ()
  | _ -> assert_failure "Unexpected exception"

let newton_single_point_with_x_before =
  "newton single point with x before" >:: fun _ ->
  let x = 0.0 in
  let points = [ (5.0, 10.0) ] in
  let expected = 10.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let newton_single_point_with_x_at_point =
  "newton single point with x at point" >:: fun _ ->
  let x = 5.0 in
  let points = [ (5.0, 10.0) ] in
  let expected = 10.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let newton_single_point_with_x_after =
  "newton single point with x after" >:: fun _ ->
  let x = 100.0 in
  let points = [ (5.0, 10.0) ] in
  let expected = 10.0 in
  let result = Newton.eval x points in
  assert_float_equal expected result

let linear_tests =
  "Linear Interpolation"
  >::: [
         linear_empty_list_returns_zero;
         linear_single_point_returns_its_y_value;
         linear_x_before_first_point_returns_y1;
         linear_x_after_last_point_returns_y2;
         linear_x_equals_first_point_returns_y1;
         linear_x_equals_second_point_returns_y2;
         linear_interpolation_between_two_points_midpoint;
         linear_interpolation_with_negative_slope;
         linear_interpolation_with_positive_slope;
         linear_with_multiple_points_uses_first_two;
         linear_non_linear_points_still_interpolates_linearly;
         linear_floating_point_precision_test;
       ]

let newton_tests =
  "Newton Interpolation"
  >::: [
         newton_empty_list_returns_zero;
         newton_single_point_returns_its_y_value;
         newton_two_points_interpolation;
         newton_two_points_extrapolation_before;
         newton_two_points_extrapolation_after;
         newton_three_points_extrapolation;
         newton_repeated_x_values_handles;
         newton_single_point_with_x_before;
         newton_single_point_with_x_at_point;
         newton_single_point_with_x_after;
       ]

let suite = "Interpolation Tests" >::: [ linear_tests; newton_tests ]
let () = run_test_tt_main suite
