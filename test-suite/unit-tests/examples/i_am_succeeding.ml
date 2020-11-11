open OUnit

let ( ) = Utest.run_tests __FILE__ (Utest.open_log_out_ch __FILE__)
      ["failure" >:: (fun ( ) -> assert_equal 1 1)]
