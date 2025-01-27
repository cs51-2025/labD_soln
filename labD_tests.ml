(* 
                              CS51 Lab D
                      Improving Debugging Skills
                         Sample Testing File

This file contains a partial set of tests for the Lab D functions,
using the `unit_test` function provided in the `Absbook` module. (See
<https://github.com/cs51/utils/blob/main/lib/absbook.ml>.)
 
 *)
(*
                               SOLUTION
 *)
   
open Printf
open CS51Utils.Absbook
open LabD
open LabD_examples
               
let tests () =

  (* testing `is_sorted` *)
  printf "\nTesting is_sorted\n" ;
  unit_test (is_sorted []) "is_sorted: empty";
  unit_test (is_sorted [1]) "is_sorted: singleton";
  unit_test (is_sorted [1; 2]) "is_sorted: short";
  unit_test (is_sorted [1; 1]) "is_sorted: solo dup";
  unit_test (is_sorted [1; 3; 4; 4; 6; 12; 13; 13; 15])
            "is_sorted: double dup";
  unit_test (is_sorted [1; 1; 3; 4; 6; 10; 12; 13; 15])
            "is_sorted: dup in front";
  unit_test (is_sorted [1; 3; 4; 4; 6; 10; 12; 13; 15])
            "is_sorted: dup in middle";
  unit_test (is_sorted [1; 3; 4; 6; 10; 12; 13; 15; 15])
            "is_sorted: dup in end";
  unit_test (not (is_sorted [2; 1; 3; 4; 6; 10; 12; 13; 15]))
            "is_sorted: unsort front";
  unit_test (not (is_sorted [1; 3; 4; 6; 10; 9; 12; 13; 15]))
            "is_sorted: unsort middle";
  unit_test (not (is_sorted [1; 3; 4; 6; 10; 12; 13; 15; 8]))
            "is_sorted: unsort end";

  (* testing `dups_sorted` *)
  printf "\nTesting dups_sorted\n" ;
  unit_test (dups_sorted [] = 0) "dups_sorted: empty";
  unit_test (dups_sorted [1] = 0) "dups_sorted: singleton";
  unit_test (dups_sorted [1; 2] = 0) "dups_sorted: solo no dup";
  unit_test (dups_sorted [1; 1] = 1) "dups_sorted: solo dup";
  unit_test (dups_sorted [1; 3; 4; 4; 6; 12; 13; 13; 15] = 2)
            "dups_sorted: double dups";
  unit_test (dups_sorted [1; 1; 3; 4; 6; 10; 12; 13; 15] = 1)
            "dups_sorted: dup in front";
  unit_test (dups_sorted [1; 3; 4; 4; 6; 10; 12; 13; 15] = 1)
            "dups_sorted: dup in middle";
  unit_test (dups_sorted [1; 3; 4; 6; 10; 12; 13; 15; 15] = 1)
            "dups_sorted: dup in end";
  unit_test (dups_sorted [1; 1; 2; 2; 3; 4] = 2)
            "dups_sorted: adjacent doubles";
  unit_test (dups_sorted [1; 1; 2; 3; 3; 4] = 2)
            "dups_sorted: nonadjacent doubles";
  unit_test (dups_sorted [1; 1; 2; 3; 3; 3; 4] = 3)
            "dups_sorted: triples";

  (* testing `is_set` *)
  printf "\nTesting is_set\n" ;
  unit_test (is_set []) "is_set: empty";
  unit_test (is_set [1]) "is_set: singleton";
  unit_test (is_set [1; 2]) "is_set: short";
  unit_test (not (is_set [1; 1])) "is_set: solo dup";
  unit_test (not (is_set [1; 3; 4; 4; 6; 12; 13; 13; 15]))
            "is_set: double dup";
  unit_test (not (is_set [1; 1; 3; 4; 6; 10; 12; 13; 15]))
            "is_set: dup in front";
  unit_test (not (is_set [1; 3; 4; 4; 6; 10; 12; 13; 15]))
            "is_set: dup in middle";
  unit_test (not (is_set [1; 3; 4; 6; 10; 12; 13; 15; 15]))
            "is_set: dup in end";
  unit_test (not (is_set [2; 1; 3; 4; 6; 10; 12; 13; 15]))
            "is_set: unsort front";
  unit_test (not (is_set [1; 3; 4; 6; 10; 9; 12; 13; 15]))
            "is_set: unsort middle";
  unit_test (not (is_set [1; 3; 4; 6; 10; 12; 13; 15; 8]))
            "is_set: unsort end";

  (* testing `member` *)
  printf "\nTesting member\n" ;
  unit_test (not (member 1 [])) "member: empty";
  unit_test (member 1 [1]) "member: singleton";
  unit_test (not (member 2 [1])) "member: singleton smaller";
  unit_test (not (member 0 [1])) "member: singleton greater";
  unit_test (member 1 [1;2;3;5;6]) "member: longer first";
  unit_test (member 3 [1;2;3;5;6]) "member: longer middle";
  unit_test (not (member 4 [1;2;3;5;6])) "member: longer not middle";
  unit_test (member 6 [1;2;3;5;6]) "member: longer end";
  
  (* testing `union` *)
  printf "\nTesting union\n" ;
  unit_test ((union [] []) = []) "union: empty";
  unit_test ((union [1] []) = [1]) "union: right empty";
  unit_test ((union [] [1]) = [1]) "union: left empty";
  unit_test ((union [1] [1]) = [1]) "union: same";
  unit_test ((union [1] [2]) = [1;2]) "union: different";
  unit_test ((union [1;2] [1;3]) = [1;2;3]) "union: first same";
  unit_test ((union [1;3] [2;3]) = [1;2;3]) "union: last same";
  unit_test ((union [1;2] [2;3]) = [1;2;3]) "union: last first same";

  (* testing `intersection` *)
  printf "\nTesting intersection\n" ;
  unit_test ((intersection [] []) = []) "intersection: empty";
  unit_test ((intersection [1] [1]) = [1]) "intersection: singleton";
  unit_test ((intersection [1] []) = []) "intersection: right empty";
  unit_test ((intersection [] [1]) = []) "intersection: left empty";
  unit_test ((intersection [1;2] [1;3]) = [1]) "intersection: first elt";
  unit_test ((intersection [1;3] [2;3]) = [3]) "intersection: second elt";
  unit_test ((intersection [1;2] [2;3]) = [2]) "intersection: skip left";
  unit_test ((intersection [2;3] [1;2]) = [2]) "intersection: skip right";

  (* testing larger examples *)
  printf "\nTesting larger examples\n" ;
  unit_test (member 284 example1) "member: 284 in e1";
  unit_test (member 284 example2) "member: 284 in e2";
  unit_test (member 284 (intersection example1 example2)) "member: 284 in e1 ^ e2";
  unit_test (member 284 (union example1 example2)) "member: 284 in e1 u e2";
  
  unit_test (member 664 example1) "member: 664 in e1";
  unit_test (not (member 664 example2)) "member: 664 not in e2";
  unit_test (not (member 664 (intersection example1 example2))) "member: 664 not in e1 ^ e2";
  unit_test (member 664 (union example1 example2)) "member: 664 in e1 u e2";
  
  unit_test (member 1322 example1) "member: 1322 in e1";
  unit_test (member 1322 example2) "member: 1322 in e2";
  unit_test (member 1322 (intersection example1 example2)) "member: 1322 in e1 ^ e2";
  unit_test (member 1322 (union example1 example2)) "member: 1322 in e1 u e2";

  unit_test (is_set example1) "example 1";
  unit_test (is_set example2) "example 2";
  
  printf "\nTests completed\n" ;;
            
let _ = tests ()
