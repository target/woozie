;; unit tests for the ooziefuncs package
;;

(load-file "ooziefuncs.el")

;; helper functions

;;===================================================================================
;; HELPER FUNCTIONS
;;===================================================================================
(defun string-set= (l1 l2)
  "returns l1 (or l2) if sets are equal, 'nil otherwise"
  (and (= (length l1) (length l2))
       (not (cl-set-difference l1 l2 :test 'string=))
       (not (cl-set-difference l2 l1 :test 'string=))))

(defun dom-from-file (filename)
  "creates dom from xml file defined by FILENAME"
  (with-temp-buffer
    (insert-file-contents filename)
    (libxml-parse-xml-region (point-min) (point-max))))

;;===================================================================================
;; FIXTURES
;;===================================================================================

(setq test-dom (dom-from-file "testdata/sampleworkflow.xml"))
(setq start-node (car (dom-by-tag test-dom 'start)))
(setq action-node (car (dom-by-tag test-dom 'action)))
(setq end-node (car (dom-by-tag test-dom 'end)))

;;===================================================================================
;; TESTS
;;===================================================================================

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
  (should (equal (oozie--wf-flow-node-names test-dom)
		 '("start" "Fork1" "Parallel1" "Parallel2" "Join1" "Decision1" "ActionIfTrue" "ActionIfFalse" "KillAction" "End"))))

(ert-deftest transition-names-test ()
  (should (equal (oozie--wf-transition-names test-dom)
		 '("Fork1" "Parallel1" "Parallel2" "Join1" "ErrorEmail" "Join1" "ErrorEmail" "Decision1" "ActionIfTrue" "ActionIfFalse" "End" "ErrorEmail" "End" "KillAction") )))

;;; tests for transition functions (dot-file creation related)

(ert-deftest node-transition-test ()
  "Tests that transitions for different types of flow nodes works properly"
  (let ( (fork-node (car (dom-by-tag test-dom 'fork)))
	 (join-node (car (dom-by-tag test-dom 'join)))
	 (decision-node (car (dom-by-tag test-dom 'decision)))
	 (action-node (car (dom-by-tag test-dom 'action))))
    (should (equal (list (cons "Fork1" "Parallel1") (cons "Fork1" "Parallel2")) (oozie--wf-node-transitions fork-node)))
    (should (equal (list (cons "Join1" "Decision1")) (oozie--wf-node-transitions join-node)))
    (should (equal (list (cons "Decision1" "ActionIfTrue") (cons "Decision1" "ActionIfFalse")) (oozie--wf-node-transitions decision-node)))
    (should (equal (list (cons "Parallel1" "Join1")) (oozie--wf-node-transitions action-node)))))

(ert-deftest happy-path-transition-test ()
  "Checks that the happy path is created correctly from incomplete paths"
  (let ( (all-good-path (list (cons "A" "B") (cons "B" "C") (cons "A" "C") (cons "C" "D") (cons "start" "A")))
	 (path-with-unreachable (list (cons "A" "B") (cons "B" "C") (cons "start" "A") (cons "D" "E") (cons "E" "F"))))
    (should (equal all-good-path (oozie--wf-transitions-hp all-good-path)))
    (should (equal (list (cons "A" "B") (cons "B" "C") (cons "start" "A")) (oozie--wf-transitions-hp path-with-unreachable)))))

;;; tests for visualization function

(ert-deftest str-pad-test ()
  "Tests that string padding works as expected."
  (should (equal " abc " (oozie--str-pad "abc" 5)))
  (should (equal "abc"   (oozie--str-pad "abc" 3)))
  (should (equal "  ab  " (oozie--str-pad "ab" 6)))
  (should (equal "  abc " (oozie--str-pad "abc" 6))))

(ert-deftest graph-box-test ()
  "Tests that oozie--graph-box creates box around text"
  (let ( (box (concat " +-----+\n" " | box |\n" " +-----+\n")))
    (should (equal box (oozie--graph-box "box" 8)))))


(ert-deftest graph-end-to-end-test ()
  "Tests the visualize function end-to-end"
  (let ( (expected
	  (with-temp-buffer
	    (insert-file "testdata/wfgraph.result")
	    (buffer-string)))
	 (output
	  (with-temp-buffer
	    (insert-file "testdata/simplegraphworkflow.xml")
	    (oozie-wf-mk-ascii)
	    (buffer-string)))
	 )
    (should (equal expected output))))


(ert-deftest node-name-test ()
  "Tests that the node name function returns the value of the name attribute or type of element if name not present."
  (let ( (no-name-node (list 'noname '() (list 'subelement)))
	 (named-node   (list 'named-node (list (cons 'name "valid-name")))) )
    (should (equal "noname" (oozie--wf-node-name no-name-node)))
    (should (equal "valid-name" (oozie--wf-node-name named-node)))))

(ert-deftest dot-node-desc-test ()
  "Tests that the function that returns the dot-node info returns appropriate values"
  (should (equal "start [shape=doublecircle]" (oozie--dot-node start-node))))

(ert-deftest flow-nodes-test ()
  "Tests that the flow-nodes function returns all flow nodes, including start and end"
  (let* ( (flow-nodes  (oozie--wf-flow-nodes test-dom))
	  (node-names  (mapcar 'oozie--wf-node-name flow-nodes)))
    (should (equal node-names
		  '("start" "Fork1" "Parallel1" "Parallel2" "Join1" "Decision1" "ActionIfTrue" "ActionIfFalse" "KillAction" "End")))))


(ert-deftest nodes-from-transitions-test ()
  "Tests that we extract a proper list of nodes from a list of transitions"
  (let ( (transitions (list (cons 'A 'B) (cons 'A 'C) (cons 'B 'D))))
    (should (equal (oozie--wf-transition-nodes transitions)
		   '(A B C D)))))

;; test driver
(ert-run-tests-batch)


