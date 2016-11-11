;; unit tests for the ooziefuncs package
;;

(load-file "ooziefuncs.el")

;; tests

(ert-deftest valid-wf-var-test ()
  "Make sure we know what is a valid var name and what is a function"
  (should (oozie-valid-wf-var "a_perfectly_good_name"))
  (should (not (oozie-valid-wf-var "wf:thisIsAFunctionNotaName()"))))

(ert-deftest find-delimited-test ()
  "Tests that we can find delimited values appropriately"
  (with-temp-buffer
    (insert "This is line {1}\n")
    (insert "This is the {2}nd line.\n")
    (insert "{3}rd line ends it all.")
    (let ( (result (oozie--find-all-delimited "{" "}") ))
      (should (not (cl-set-difference result '("1" "2" "3") :test 'string=))))))

;; helper functions

(ert 't)
