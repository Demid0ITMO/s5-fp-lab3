open OUnit2
open Interpolation
open Parser

let test_parse_point_valid =
  "parse valid points" >:: fun _ ->
  let config = { default_config with delim = " " } in
  assert_equal (Ok (1.0, 2.0)) (parse_point "1.0 2.0" config);
  assert_equal (Ok (-3.5, 4.2)) (parse_point "-3.5 4.2" config);
  assert_equal (Ok (0.0, 0.0)) (parse_point "0 0" config)

let test_parse_point_with_different_delimiters =
  "parse points with different delimiters" >:: fun _ ->
  let config_comma = { default_config with delim = "," } in
  assert_equal (Ok (1.0, 2.0)) (parse_point "1.0,2.0" config_comma);

  let config_tab = { default_config with delim = "\t" } in
  assert_equal (Ok (1.0, 2.0)) (parse_point "1.0\t2.0" config_tab)

let test_linear_single_point =
  "linear interpolation with single point" >:: fun _ ->
  let points = [ (1.0, 2.0) ] in
  assert_equal 2.0 (Linear.eval 1.0 points);
  assert_equal 2.0 (Linear.eval 0.0 points);
  assert_equal 2.0 (Linear.eval 2.0 points)

let test_linear_empty_points =
  "linear interpolation with empty list" >:: fun _ ->
  assert_equal 0.0 (Linear.eval 1.0 [])

let test_newton_single_point =
  "newton interpolation with single point" >:: fun _ ->
  let points = [ (1.0, 2.0) ] in
  assert_equal 2.0 (Newton.eval 1.0 points)

let test_newton_empty_points =
  "newton interpolation with empty list" >:: fun _ ->
  assert_equal 0.0 (Newton.eval 1.0 [])

let test_config_functions =
  "config functions tests" >:: fun _ ->
  let config = { interpol_alg = "both"; step = 0.5; n = 4; delim = " " } in

  assert_bool "both should return true for 'both'" (both config);
  assert_bool "linear should return true when alg is 'both'" (linear config);
  assert_bool "newton should return true when alg is 'both'" (newton config);
  assert_equal 0.5 (step config);
  assert_equal 4 (n config);
  assert_equal " " (delim config);

  let config_linear = { config with interpol_alg = "linear" } in
  assert_bool "linear should return true for 'linear'" (linear config_linear);
  assert_bool "both should return false for 'linear'" (not (both config_linear));

  let config_newton = { config with interpol_alg = "newton" } in
  assert_bool "newton should return true for 'newton'" (newton config_newton);
  assert_bool "linear should return false for 'newton'"
    (not (linear config_newton));

  let config_invalid = { config with interpol_alg = "invalid" } in
  assert_bool "both should return true for invalid alg" (both config_invalid);
  assert_bool "linear should return true for invalid alg"
    (linear config_invalid);
  assert_bool "newton should return true for invalid alg"
    (newton config_invalid)

let suite =
  "Interpolation Test Suite"
  >::: [
         test_parse_point_valid;
         test_parse_point_with_different_delimiters;
         test_linear_single_point;
         test_linear_empty_points;
         test_newton_single_point;
         test_newton_empty_points;
         test_config_functions;
       ]

let () = run_test_tt_main suite
