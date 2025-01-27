(* 
                              CS51 Lab D
                      Improving Debugging Skills
 *)
(*
                               SOLUTION
 *)

(* Objective: In this lab, you'll improve your debugging skills by
applying fundamental debugging ideas to debugging an implementation of
set operations (union, intersection, etc.).

        ****************************************************************
        In this lab, sets of integers will be represented as `int
        list`s whose elements are in sorted order with no
        duplicates. All functions can assume this invariant and should
        deliver results satisfying it as well.
        ****************************************************************

In the code that follows, some functions may have bugs, so that their
behavior may not match the intended behavior described in the
comments. Your job is to find and fix all of the bugs.

========================================================================
Part 0: Important aspects of debugging

You may not have thought explicitly about the debugging process, but
doing so can provide you with valuable skills in the process. Here are
some of the major aspects of the debugging process.

    Identification

        Read error messages in detail. They often provide not just the
        nature of the error, but an approximate location.

        Set up unit tests for individual functions. Unit tests can
        identify bugs in your code by finding cases that don't match
        the behavior you intended. Try to specify unit test cases that
        cover all of the important paths through the code. A good
        technique is to put the unit tests in a separate file that
        references the file with the functions to be tested. Then,
        whenever you make changes to the functions, you can rerun the
        test file to make sure that you haven't introduced bugs in
        previously working code.

    Localization

        When you first identify a bug, you may not know where in the
        code base the bug actually lives. You'll need to localize the
        bug -- finding its location in the code base.

        In tracking down problems in larger codebases, eliminate
        portions of the code to generate the minimal codebase that
        demonstrates the problem. Breaking the code into smaller parts
        can allow localization to one of the parts, as they can be
        unit-tested separately.

    Simplification

        When confronted with an error exhibited on a large instance,
        try to simplify it to find the minimal example that exhibits
        the problem. 

    Reproduction 

        Try alternate examples to see which ones exhibit the problem.
        The commonalities among the examples that exhibit the problem
        can give clues as to the problem. 

    Diagnosis

        Verify that invariants that should hold in the code actually
        do, with assertions or other constructs. (The `Absbook.verify`
        function can be especially useful in verifying invariants of
        the arguments and return value.) Conduct experiments
        to test your theory of what has gone wrong.

    Correction

        Generate git commits to save a version of the code so that you
        can confidently make changes to the code while you are
        experimenting, knowing that you'll be able to return to
        earlier versions.

    Maintenance

        Code that was once working can become buggy as changes are
        mode either to the code itself or to code that is uses. It's
        thus helpful to retest code when changes are made to it or its
        environment. Fortunately, unit test files are ideal for this
        process. Rerunning the unit tests liberally allows us to
        verify that working code hasn't regressed to a buggy state.
        (The process is referred to in the literature as "regrsssion
        testing" for this reason. See
        <https://en.wikipedia.org/wiki/Regression_testing>.)

To get you started on debugging, we've placed a few unit tests for
some of the functions in the file `labD_tests.ml`. Compile and run
these tests to see how the functions are working so far.

    % ocamlbuild -use-ocamlfind labD_tests.byte
    % ./labD_tests.byte

What do you notice? Does this give you an idea on where to start
debugging?

========================================================================
Part 1: Some utilities for checking the sorting and no-duplicates
conditions.
 *)
       
(* is_sorted lst -- Returns `true` if and only if `lst` is a sorted
   list *)
let is_sorted (lst : 'a list) : bool =
  lst = List.sort Stdlib.compare lst ;;

(* dups_sorted lst -- Returns the number of duplicate elements in
   `lst`, a sorted list of integers. For example

      # dups_sorted [1;2;5;5;5;5;5;5;6;7;7;9] ;;
      - : int = 6
 *)
let rec dups_sorted (lst : 'a list) : int =
  match lst with
  | [] -> 0
  | [_] -> 0
  | first :: (second :: _rest as second_rest) ->
      dups_sorted second_rest 
      + if first = second then 1 else 0 ;;

(* SOLUTION: The first of the two failing unit tests is 

      dups_sorted [1; 3; 4; 4; 6; 12; 13; 13; 15]

   which should be 2 (the duplicate 4 and 13). Running it directly, we get 

      # dups_sorted [1; 3; 4; 4; 6; 12; 13; 13; 15] ;;
      - : int = 1

   We can start by dropping all of the unduplicated elements to try to find a
   shorter failing example. Sure enough,

      # dups_sorted [4; 4; 13; 13] ;;
      - : int = 1

   In fact, it looks like any matches after the first one is ignored.

      # dups_sorted [4; 4; 4; 13; 13; 15; 15; 15; 72; 72] ;;
      - : int = 1

   First off, we'll definitely want to add some tests like these to
   `labD_tests.ml` to aid our testing down the road.

   Returning to our simplest case,

      dups_sorted [4; 4; 13; 13] ;;

   the first match will happen based on the final match case,

     | first :: second :: rest ->
         if first = second then 1 else 0
         + dups_sorted rest

   which is *attempting* to add 1 or 0 to the recursive count of
   duplicates. Unfortunately, and despite the intention expressed by
   the indentation, what is actually getting calculated is better
   indicated by this layout:

     | first :: second :: rest ->
         if first = second 
         then 1 
         else 0 + dups_sorted rest

   that is, in the case of a duplicate, the recursive call is never
   even made! This bug would therefore show up in any example that has
   more than one duplicate. This is easily repairable, either using
   parentheses to override the precedence or by swapping the two terms
   being added, like this:

     | first :: second :: rest ->
         dups_sorted rest 
         + if first = second then 1 else 0

   Making this change repairs the bug:

      # dups_sorted [4; 4; 13; 13] ;;
      - : int = 2
      # dups_sorted [1; 3; 4; 4; 6; 12; 13; 13; 15];;
      - : int = 2

   But that's not sufficient. The test case `dup in end` is still
   failing. We can check it at the command line.

      # dups_sorted [1; 3; 4; 6; 10; 12; 13; 15; 15] ;;
      - : int = 0

   Can we find a simpler case that fails. We'll start by dropping
   elements from the list one by one:

      # dups_sorted [1; 3; 4; 6; 10; 12; 13; 15; 15] ;;
      - : int = 0
      # dups_sorted [3; 4; 6; 10; 12; 13; 15; 15] ;;
      - : int = 1
      # dups_sorted [4; 6; 10; 12; 13; 15; 15] ;;
      - : int = 0
      # dups_sorted [6; 10; 12; 13; 15; 15] ;;
      - : int = 1

   It looks like the duplicate is found only when it's at an even
   numbered index. The simplest instance, then should be (sure enough)

      # dups_sorted [13; 15; 15] ;;
      - : int = 0

   That's a short enough example that we can simply play computer and
   see what the code would do. In pattern matching this list, `first`
   would be `13`, `second` would be `15`, and `rest` would be the list
   `[15]`. Since 13 and 15 are different, we'll add 0 to the recursive
   call on the list `[15]`. But that list has no duplicates, so the
   sum would be `0`. 

   By now, you've probably seen the bug. The recursive call applies
   to the tail of `lst` after the first two elements (`first` and
   `second`). What if the second and third elements are duplicates?
   The tail won't find that pair! We need to hang on to the tail after
   just the first element, so as to recur on it. This can be done in a
   variety of ways. One is to embed a second match:

     | first :: rest ->
         match rest with 
         | [] -> failwith "dups_sorted: can't happen"
         | second :: _ -> 
             dups_sorted rest 
             + if first = second then 1 else 0

   A much cleaner solution uses the `as` construct in patterns to
   allow the naming of subparts of a pattern match:

     | first :: (second :: _rest as second_rest) ->
         dups_sorted second_rest 
         + if first = second then 1 else 0 ;;
 *)

(* is_set lst -- Returns `true` if and only if lst represents a set,
   with no duplicates and elements in sorted order. *)
let is_set (lst : 'a list) : bool =
  is_sorted lst && dups_sorted lst = 0 ;;

         
(*======================================================================
Part 2: Set operations -- member, union, and intersection

Below we provide code for computing membership, intersections, and
unions of sets represented by lists with the stated invariant. 

Check out the unit tests for these in `labD_tests.ml`. Augment the
tests until you're satisfied that you've fully tested these functions,
making any needed changes as you go.

We'll test them further on larger examples in the next part, Part
3.
 *)

(* member elt set -- Returns `true` if and only if `elt` is an element
   of `set` (represented as above). Search can stop early based on
   sortedness of `set`. *)
let rec member elt set =
  match set with
  | [] -> false
  | hd :: tl ->
     if elt = hd then true
     else if elt < hd then false
     else member elt tl ;;

(* SOLUTION: We've added a good set of unit tests for the `member`
   function. These show that the function seems to be working
   correctly. *)

    
(* SOLUTION: Check out the tests we added to the testing file. With no changes
   in the definition of `union`, some of these tests, even simple
   ones, fail. For instance, 

      # union [1] [1] = [1] ;;
      - : bool = false

   What happens in the code when the two head elements are the
   same? We take the head element of the first set, together with
   remaining elements *including* the redundant head element of the
   second set. This leads to two copies of the head element, violating
   the no-duplicates invariant. Instead, we need only keep one of the
   identical head elements. (While we're at it, we clean up the base
   case pattern matches as well.)  *)

(* union set1 set2 -- Returns a list representing the union of the
   sets `set1` and `set2` *)
let rec union s1 s2 =
  match s1, s2 with
  | [], _ -> s2
  | _, [] -> s1
  | hd1 :: tl1, hd2 :: tl2 ->
     if hd1 = hd2 then
       hd1 :: union tl1 tl2
     else if hd1 < hd2 then
       hd1 :: union tl1 s2
     else (* hd1 > hd2 *)
       hd2 :: union tl2 s1 ;;

(* intersection set1 set2 -- Returns a list representing the
   intersection of the sets `set1` and `set2` *)
let rec intersection s1 s2 =
  match s1, s2 with
  | [], _ -> []
  | _, [] -> []
  | hd1 :: tl1, hd2 :: tl2 -> 
     if hd1 = hd2 then hd1 :: intersection tl1 tl2
     else if hd1 < hd2 then intersection tl1 s2
     else intersection s1 tl2 ;;

(* SOLUTION: Even a small set of tests, as we've added in the testing file,
   finds major problems with this implementation of `intersection`. Once 
   the failures are noticed, the bug -- incorrect parity of when to end 
   the search -- is clear and easily repaired. 
 *)

(*======================================================================
Part 3: Scaling up the testing

The file `labD_examples` contains a couple of larger examples of sets
represented as lists (`example1` and `example2`). The `labD_tests.ml`
file contains a few tests based on these larger examples, which are
commented out at the moment. Uncomment them now and rerun the unit
tests. What do you notice?

More bugs to debug. Where do you think the problems lie? Remaining
bugs in the functions above? In the examples? In the tests themselves?

You're on your own to figure out what's going on and correct the
problems, wherever they might be.
 *)

(* SOLUTION: Adding in the tests on the larger examples reveals some
   failures. This is perhaps surprising if you've heavily tested and
   debugged the functions involved. What could be going on? There are
   three possibilities.

   1. There are remaining bugs in the functions above.
   2. The unit tests are incorrect. (They check for the wrong values
      of the test expression.)
   3. The values used in the unit tests are problematic.

   Let's treat them serially.

   1. We've added a bunch of unit tests that seem to cover all of the
      cases. Let's assume the functions are okay for now, but may need
      to return to that assumption.

   2. We can manually double check the unit tests. 

         Is 284 actually a member of `example1`? Yes, it is, as
         verified by a simple search in the file.

         Is 284 actually a member of `example2`? Ditto.

         Is 284 thus a member of the intersection of the two examples?
         It should be.

         Similarly for the union. 

      A similar process reveals that the other tests are correctly
      stated. Hmm.

   3. Eyeballing the values used in the tests -- `example1` and
      `example2` -- they seem to look fine. No duplicates or out of
      order items at first glance. But eyeballing such a large data
      structure isn't ideal. Computers can do the checking better than
      humans. In fact, we have a function to check for us. Let's add
      unit tests to `labD_tests.ml` that verify that the two examples 
      are in fact invariant-obeying sets:

         # is_set example1 ;;
         - : bool = true
         # is_set example2 ;;
         - : bool = false

      The latter unit test fails! 
      
      The lesson here is that sometimes what needs debugging is *the
      unit tests themselves*. Let's debug the problem with `example2`.

      Does it fail the test because it has duplicates or because it
      isn't sorted or both?

         # is_sorted example2 ;;  
         - : bool = false
         # dups_sorted example2 ;;
         - : int = 0

      Apparently, the problem is in the sorting.

      How can we find the exact location of the bug in `example2`?
      More careful eyeballing is too painful. Again, it's better to
      let the computer do the work. 

      One good and very general approach to localizing a problem in a
      large program is to *use binary search*. We can separately test
      the first half and the second half of the code to see which half
      the problem occurs in. We do the same in the offending half to
      find the bad quarter. Eventually, we'll have narrowed the search
      down to a short enough piece of code that eyeballing may be
      sufficient. (The process shouldn't require many divisions
      because of the tremendous abbreviating power of logarithms.)

      Another approach is to *use computer power* to aid in the
      search. Rather than our finding the location of the problem, we
      write code to do it for us.  For instance, we can compare
      `example2` with its sorted version, and find the first place
      where they differ. That should give us a clue as to what's going
      on.

         # List.find (fun (x, y) -> x <> y)
                   (List.combine example2 
                                 (List.sort Stdlib.compare example2)) ;;     
         - : int * int = (1872, 1076)

      The problem seems to be that either 1872 or 1076 is out of
      order. Now, reexamining the definition of `example2` verifies that
      1872 is way early.

      Now we turn to fixing the problem. Here are some options to consider.

      1. Drop the unit tests that involve `example2`. This makes
         sense, because `example2` does not conform to the required
         invariants. 

         This is a *lost opportunity* since testing on some large
         cases is a useful thing to do.

      2. Change the parity of the offending unit tests, that is,
         change the `member` unit test to

            unit_test (member 1322 example2) "member: 1322 in e2";

         This is a **terrible idea**. It casts in stone the particular
         way in which the function behaves on invariant-failing cases,
         even though, the function's specification doesn't require
         this behavior.

      3. Change the definitions of the `member`, `intersection`, and
         `union` functions so that they verify the invariants on their
         arguments and results. But what should they do if the
         verification fails? They might raise an appropriate
         exception, which the unit tests could then test for. But the
         raising of the exception in that case was not part of the
         specifications of those functions.

         a. In addition, add a specification to the functions as to
            what exceptions may be raised and under what
            conditions. Then add unit tests that verify that behavior.

            This is a *reasonable choice* though it requires a fair
            amount of effort. In later labs we'll see methods for
            making sure that nonconforming data structures can be
            prevented from even being created.

      4. Change the definition of `example2` to obey the
         invariants. There are many ways to do so. For instance, we
         might drop the offending 1872 from the example, or move it to
         the end of the list where it would be in sorted order. In the
         solution version of `labD_examples.ml`, we replaced the 1872
         with 1072, which falls properly in sorted order.
 *)
