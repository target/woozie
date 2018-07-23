;; unit tests for the ooziefuncs package
;;

(load-file "ooziefuncs.el")

;; tests
(ert-deftest list-hive-vars-test ()
  "Make sure that the hive vars are being properly extracted"
  (with-temp-buffer
    (insert "one ${hivevar:FOO} defined here.\n")
    (insert "${hivevar:BAR} is defined here too!\n")
    (insert "And a third hivevar: ${hivevar:THIS_ONE}\n")
    (insert "And repeating ${hivevar:FOO} here.")
    (let ( (hive-vars (oozie--hive-vars-list)))
      (should (string-set= hive-vars '("FOO" "BAR" "THIS_ONE"))))))

(ert-deftest list-wf-vars-test ()
  "Makes sure that the workflow variables are being properly extracted"
  (with-temp-buffer
    (insert "
<workflow-app name=\"promo_dataprep_${FOO}_${BAR}\" xmlns=\"uri:oozie:workflow:0.4\">

    <global>
        <job-tracker>${FOO}</job-tracker>
        <name-node>${BAZ}</name-node>
        <configuration>
            <property>
                <name>${FOO}</name>
                <value>${BAR}</value>
            </property>
        </configuration>
    </global>
</workflow>${BAZ} // not proper wf definition, but the code does not care.   
")
    (let ( (wf-vars (oozie--wf-vars-list)) )
      (should (string-set= wf-vars '("FOO" "BAR" "BAZ"))))))

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

(ert-deftest node-names-test ()
  (with-temp-buffer
    (insert-file "./testdata/sampleworkflow.xml")
    (should (equal (oozie--wf-flow-node-names) 	'("Fork1" "Action1" "Decison1" "Action2" "Action3" "KillAction" "End")))))

(ert-deftest transition-names-test ()
  (with-temp-buffer
    
    (insert-file "./testdata/sampleworkflow.xml")
    (should (equal (oozie--wf-transition-names) '("Config" "Parallel1" "Parallel2" "DataprepSubflow" "ErrorEmail" "ModelerSubflow" "ModelStateUpdaterSubflow" "End" "ErrorEmail" "KillAction" "KillAction") ))))

;; helper functions
(defun string-set= (l1 l2)
  "returns l1 (or l2) if sets are equal, 'nil otherwise"
  (and (= (length l1) (length l2))
       (not (cl-set-difference l1 l2 :test 'string=))
       (not (cl-set-difference l2 l1 :test 'string=))))

(ert-run-tests-batch)

