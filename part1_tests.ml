(* 
                              CS51 Lab D
                      Improving Debugging Skills
                        Sample Testing File 1

This file contains a partial set of tests for the Lab D functions in
Part 1, using the `unit_test` function provided in the `Absbook`
module. (See <https://github.com/cs51/utils/blob/main/lib/absbook.ml>.)
 
 *)
(*
                               SOLUTION
 *)

open Printf
open CS51Utils.Absbook
open LabD
open LabD_examples
               
let part1tests () =
  unit_test (last_element [] = None) "last_element: empty";
  unit_test (last_element [42] = Some 42) "last_element: singleton int";
  unit_test (last_element [true] = Some true) "last_element: singleton bool";
  unit_test (last_element [42; 43] = Some 43) "last_element: double";
  unit_test (last_element [42; 41; 40] = Some 40) "last_element: triple";

  unit_test (sum_to_n 0 = 0) "sum_to_n 0";
  unit_test (sum_to_n 1 = 1) "sum_to_n 1";
  unit_test (sum_to_n 2 = 3) "sum_to_n 2";
  unit_test (sum_to_n 100 = 5050) "sum_to_n 100";

  unit_test (describe_list [] = "Empty list") "describe_list: empty";
  unit_test (describe_list [1] = "Singleton list") "describe_list: singleton int";
  unit_test (describe_list [42; 43] = "Multiple list") "describe_list: other int";
  unit_test (describe_list [42; 43; 1; 2; 3] = "Multiple list") "describe_list: other int long";
  unit_test (describe_list [true] = "Singleton list") "describe_list: singleton bool";
  unit_test (describe_list [false; true] = "Multiple list") "describe_list: other bool";

  printf "\nTests completed\n" ;;
            
let _ =
  part1tests () ;;
