;;;  -*- lexical-binding: t; -*-
;; unit tests for the ooziefuncs package
;;

(load-file "woozie.el")

;; helper functions

;;===================================================================================
;; HELPER FUNCTIONS
;;===================================================================================
(defun string-set= (l1 l2)
  "Return L1 (or L2) if sets are equal, 'nil otherwise."
  (and (= (length l1) (length l2))
       (not (cl-set-difference l1 l2 :test 'string=))
       (not (cl-set-difference l2 l1 :test 'string=))))

(defun dom-from-file (filename)
  "Create dom from xml file defined by FILENAME."
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
(setq comment-dom (dom-from-file "testdata/commentworkflow.xml"))

;;===================================================================================
;; TESTS
;;===================================================================================

(ert-deftest get-parameter-names-test ()
  "Checks that parameter name extracting function works as expected."
  (should (equal '("param1" "param2" "param1") (woozie--wf-param-names test-dom))))


(ert-deftest list-hive-vars-test ()
  "Make sure that the hive vars are being properly extracted"
  (with-temp-buffer
    (insert "one ${hivevar:FOO} defined here.\n")
    (insert "${hivevar:BAR} is defined here too!\n")
    (insert "And a third hivevar: ${hivevar:THIS_ONE}\n")
    (insert "And repeating ${hivevar:FOO} here.")
    (let ( (hive-vars (woozie--hive-vars-list)))
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
    (let ( (wf-vars (woozie--wf-vars-list)) )
      (should (string-set= wf-vars '("FOO" "BAR" "BAZ"))))))

(ert-deftest valid-wf-var-test ()
  "Make sure we know what is a valid var name and what is a function"
  (should (woozie--valid-wf-var "a_perfectly_good_name"))
  (should (not (woozie--valid-wf-var "wf:thisIsAFunctionNotaName()"))))

(ert-deftest find-delimited-test ()
  "Tests that we can find delimited values appropriately"
  (with-temp-buffer
    (insert "This is line {1}\n")
    (insert "This is the {2}nd line.\n")
    (insert "{3}rd line ends it all.")
    (goto-char (point-min))
    (should (string-set= (woozie--find-delimited-from-point "{" "}") '("1" "2" "3") ))))

(ert-deftest node-names-test ()
  (should (equal (woozie--wf-flow-node-names test-dom)
		 '("start" "Fork1" "Parallel1" "Parallel2" "Join1" "Decision1" "ActionIfTrue" "ActionIfFalse" "KillAction" "End"))))

(ert-deftest node-transition-test ()
  "Tests that transitions for different types of flow nodes works properly"
  (let ( (fork-node (car (dom-by-tag test-dom 'fork)))
	 (join-node (car (dom-by-tag test-dom 'join)))
	 (decision-node (car (dom-by-tag test-dom 'decision)))
	 (action-node (car (dom-by-tag test-dom 'action))))
    (should (equal '(("Fork1" "Parallel1" ok) ("Fork1" "Parallel2" ok)) (woozie--wf-node-transitions fork-node)))
    (should (equal '(("Join1" "Decision1" ok)) (woozie--wf-node-transitions join-node)))    
    (should (equal '(("Decision1" "ActionIfTrue" ok) ("Decision1" "ActionIfFalse" ok)) (woozie--wf-node-transitions decision-node)))
    (should (equal '(("Parallel1" "Join1" ok) ("Parallel1" "ErrorEmail" error)) (woozie--wf-node-transitions action-node)))))

(ert-deftest happy-path-transition-test ()
  "Checks that the happy path is created correctly from incomplete paths"
  (let ( (all-good-path '(("A" "B" ok) ("B" "C" ok) ("A" "C" ok) ("C" "D" ok) ("start" "A" ok)))
	 (path-with-unreachable (list (list "A" "B" 'ok) (list "B" "C" 'ok) (list "start" "A" 'ok) (list "D" "E" 'ok) (list "E" "F" 'ok))))
    (should (equal all-good-path (woozie--wf-transitions-hp all-good-path)))
    (should (equal (list (list "A" "B" 'ok) (list "B" "C" 'ok) (list "start" "A" 'ok)) (woozie--wf-transitions-hp path-with-unreachable)))))

;;; tests for visualization function

(ert-deftest str-pad-test ()
  "Tests that string padding works as expected."
  (should (equal " abc " (woozie--str-pad "abc" 5)))
  (should (equal "abc"   (woozie--str-pad "abc" 3)))
  (should (equal "  ab  " (woozie--str-pad "ab" 6)))
  (should (equal "  abc " (woozie--str-pad "abc" 6))))

(ert-deftest graph-box-test ()
  "Tests that woozie--graph-box creates box around text"
  (let ( (box (concat " +-----+\n" " | box |\n" " +-----+\n")))
    (should (equal box (woozie--graph-box "box" 8)))))


(ert-deftest graph-end-to-end-test ()
  "Tests the visualize function end-to-end"
  (let ( (expected
	  (with-temp-buffer
	    (insert-file "testdata/wfgraph.result")
	    (buffer-string)))
	 (output
	  (with-temp-buffer
	    (insert-file "testdata/simplegraphworkflow.xml")
	    (woozie-wf-mk-ascii)
	    (buffer-string)))
	 )
    (should (equal expected output))))


(ert-deftest node-name-test ()
  "Tests that the node name function returns the value of the name attribute or type of element if name not present."
  (let ( (no-name-node (list 'noname '() (list 'subelement)))
	 (named-node   (list 'named-node (list (cons 'name "valid-name")))) )
    (should (equal "noname" (woozie--wf-node-name no-name-node)))
    (should (equal "valid-name" (woozie--wf-node-name named-node)))))

(ert-deftest dot-node-desc-test ()
  "Tests that the function that returns the dot-node info returns appropriate values"
  (should (equal "start [shape=doublecircle]" (woozie--dot-node start-node))))

(ert-deftest flow-nodes-test ()
  "Tests that the flow-nodes function returns all flow nodes, including start and end"
  (let* ( (flow-nodes  (woozie--wf-flow-nodes test-dom))
	  (node-names  (mapcar 'woozie--wf-node-name flow-nodes)))
    (should (equal node-names
		  '("start" "Fork1" "Parallel1" "Parallel2" "Join1" "Decision1" "ActionIfTrue" "ActionIfFalse" "KillAction" "End")))))

(ert-deftest loading-wf-test ()
  "Tests that the workflow-app element is retrieved properly whether there are comments before or not."
  (should (equal 'workflow-app (dom-tag (woozie--get-wf-root test-dom))))
  (should (equal 'workflow-app (dom-tag (woozie--get-wf-root comment-dom)))))

  
(ert-deftest nodes-from-transitions-test ()
  "Tests that we extract a proper list of nodes from a list of transitions"
  (let ( (transitions (list (list 'A 'B 'ok) (list 'A 'C 'ok) (list 'B 'D 'ok))))
    (should (equal (woozie--wf-transition-nodes transitions)
		   '(A B C D)))))

(ert-deftest duplicates-test ()
  "Tests that the function that returns duplicates indeed returns duplicates"
  (should (equal '() (woozie--list-duplicates '())))
  (should (equal '() (woozie--list-duplicates '("a"))))
  (should (equal '() (woozie--list-duplicates '("a" "b"))))
  (should (equal '("a") (woozie--list-duplicates '("a" "b" "c" "a"))))
  (should (equal '("b" "a") (woozie--list-duplicates '("a" "a" "b" "c" "d" "b" "e"))))
  (should (equal '("b" "a") (woozie--list-duplicates '("a" "a" "b" "c" "d" "a" "a" "b")))))

(ert-deftest properties-from-file-test ()
  "Tests that we can successfully extract the list of property names defined in a file"
  (should (equal (woozie--properties-from-file "testdata/sampleconfig.properties")
		 '("property1" "property2" "property3"))))

;; test driver
(ert-run-tests-batch)


