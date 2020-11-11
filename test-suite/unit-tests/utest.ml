open OUnit


(* general case to build a test *)
let mk_test nm test = nm >: test

(* common cases for building tests *)
let mk_eq_test nm descr expected actual =
  mk_test nm (TestCase (fun _ -> assert_equal ~msg:descr expected actual))

let mk_bool_test nm descr actual =
  mk_test nm (TestCase (fun _ -> assert_bool descr actual))

(* given test result, print message, return success boolean *)
let logger _ result =
  match result with
  | RSuccess path ->
     true
  | RError (path,msg)
  | RFailure (path,msg) ->
     false
  | RSkip (path,msg)
  | RTodo (path,msg) ->
     false

(* run one OUnit test case, return successes, no. of tests *)
(* notionally one test, which might be a TestList *)
let run_one _ test =
  let rec process_results rs =
    match rs with
      [] -> (0,0)
    | (r::rest) ->
       let succ = 1 in
       let succ_results,tot_results = process_results rest in
       (succ + succ_results,tot_results + 1)
  in
  let results = perform_test (fun _ -> ()) test in
  process_results results

let open_log_out_ch ml_fn =
  let log_fn = "/dev/null" in
  open_out log_fn

(* run list of OUnit test cases, log results *)
let run_tests _ _ tests =
  let logit = logger () in
  let rec run_some tests succ tot =
    match tests with
      [] -> (succ,tot)
    | (t::ts) ->
       let succ_one,tot_one = run_one logit t in
       run_some ts (succ + succ_one) (tot + tot_one)
  in
  (* format for test-suite summary to find status
     success if all tests succeeded, else failure
   *)
  let succ,tot = run_some tests 0 0 in
  if succ = tot then exit 0 else exit 1
