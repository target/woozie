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
    (should (string-set= (oozie--find-all-delimited "{" "}") '("1" "2" "3") ))))


;; helper functions
(defun string-set= (l1 l2)
  "returns l1 (or l2) if sets are equal, 'nil otherwise"
  (not (cl-set-difference l1 l2 :test 'string=)))

(ert 't)
