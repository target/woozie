;;; woozie -- Summary
;; -*- lexical-binding: t; -*-
;; Copyright (c) 2016-2021, Target Corp.
;;
;; Authors: alexandre.santoro@target.com

;; Version: 0.2.0

;;; Commentary:
;;
;; ooziefuncs contains a collection of functions that simplify creating oozie workflows.
;; These include functions for:
;;    * adding actions tot the current workflow.xml buffer
;;    * extracting variable information from one (or more) workflow.xml files and adding
;;      them to a config buffer, which can then serve as the basis for creating a confi-
;;      guration file.
;;    * generating graph representations of the workflow
;;
;;; Code:

(require 'cl-lib)
(require 'dom)

;;----------------------------------------------------------------------------------------
;; user variables
;;----------------------------------------------------------------------------------------

(defvar woozie-dot-node-attribs '((start    . "[shape=doublecircle]")
				  (end      . "[shape=doublecircle]")
				  (action   . "")
				  (fork     . "[shape=box]")
				  (join     . "[shape=box]")
				  (decision . "[shape=diamond]"))
    "An association list mapping workflow element types to their respective DOT node attributes.")

;; these variables the user should not really have to change
(defvar woozie--msg-buff "*Woozie*") ;;  The temp buffer to which woozie writes its reports"

;;----------------------------------------------------------------------------------------
;; user (interactive) functions
;;----------------------------------------------------------------------------------------

(defun woozie-wf-mk (jobname)
  "Add a skeleton workflow definition named JOBNAME to the current buffer."
  (interactive "sjob name: ")
  (insert
   "<?xml version=\"1.0\" ?>
<workflow-app name=\"" jobname "\" xmlns=\"uri:oozie:workflow:0.4\">

    <global>
        <job-tracker>${jobTracker}</job-tracker>
        <name-node>${nameNode}</name-node>
        <configuration>
            <property>
                <name>mapreduce.job.queuename</name>
                <value>${queueName}</value>
            </property>
        </configuration>
    </global>
   
    <credentials>
        <credential name='hcat_creds' type='hcat'>
            <property>
                <name>hcat.metastore.uri</name>
                <value>${metastore_uri}</value>
            </property>
            <property>
                <name>hcat.metastore.principal</name>
                <value>${metastore_principal}</value>
            </property>
         </credential>
    </credentials>

    <start to=\"INSERT_ACTION_NAME_HERE\"/>

    <kill name=\"KillAction\">
        <message>Workflow failed, error message[${wf:errorMessage(wf:lastErrorNode())}]</message>
    </kill>

    <end name=\"end\"/>

</workflow-app>
"
   ))

(defun woozie-wf-action-hive (action-name hive-script)
  "Insert a oozie workflow hive action called ACTION-NAME with the script HIVE-SCRIPT."
  (interactive "saction name: \nfhive script: ")
  (let ( (hivevars (woozie--hive-vars hive-script)))
    (insert
     "
    <action name=\"" action-name "\" cred=\"hcat_creds\">
        <hive xmlns=\"uri:oozie:hive-action:0.5\">
            <script>" (file-name-nondirectory hive-script) "</script>\n")
    (dolist (hivevar hivevars)
      (insert "            <param>" hivevar "=${" hivevar "}</param>\n"))
    (insert "        </hive>
        <ok to=\"\"/>
        <error to=\"\"/>
    </action>
"
   )))

(defun woozie-wf-action-email (action-name email)
  "Insert an oozie workflow email action named ACTION-NAME sending email to EMAIL."
  (interactive "saction name: \nstargetEmail: ")
  (insert
   "
    <action name=\"" action-name "\">
        <email xmlns=\"uri:oozie:email-action:0.1\">
            <to>" email "</to>
            <subject></subject>
            <body>
            </body>
        </email>
        <ok to=\"\"/>
        <error to=\"\"/>
    </action>
"
   ))

(defun woozie-wf-action-shell (action-name cmd-str)
  "Insert a shell action named ACTION-NAME with the command defined by CMD-STR."
  (interactive "saction name: \nscommand (with arguments): ")
  (let ( (script (car (split-string cmd-str)))
	 (args   (cdr (split-string cmd-str))))
    (insert "
    <action name=\"" action-name "\" cred=\"hcat_creds\">
        <shell xmlns=\"uri:oozie:shell-action:0.3\">
            <exec>" script "</exec>
")
    (dolist (argument args)
      (woozie-wf-action-argument argument)
      (insert "\n"))
    (insert "        </shell>
        <ok to=\"end\"/>
        <error to=\"fail\" />
    </action>
"
    )))

(defun woozie-wf-action-property (name value)
  "Insert a property tag with NAME and VALUE."
  (interactive "sproperty name: \nsproperty value: ")
  (insert
   "
        <property>
            <name>" name "</name>
            <value>" value "</value>
        </property>
"
	    ))

(defun woozie-wf-action-argument (arg-value)
  "Insert an argument tag with text ARG-VALUE."
  (interactive "sarg value: ")
  (insert "            <argument>" arg-value "</argument>"))

(defun woozie-wf-action-file (filename)
  "Insert a file tag for FILENAME."
  (interactive "sfilename: ")
  (insert "            <file>" filename "</file>"))

(defun woozie-wf-validate ()
  "Perform validations on the oozie workflow xml defined in the current buffer."
  (interactive)
  (let ( (b (current-buffer))
	 (dom (libxml-parse-xml-region (point-min) (point-max))) )
    (with-output-to-temp-buffer woozie--msg-buff
      (switch-to-buffer woozie--msg-buff)
      (woozie--msg "=======================================================")
      (woozie--msg "Validating workflow.....")
      (woozie--validate-action-names dom)
      (woozie--validate-action-transitions dom)
      (switch-to-buffer b))))

(defun woozie-wf-validate-config (config-file)
  "Validate the the current buffer workflow against the specified CONFIG-FILE.
Provides a list of variables not defined in the configuration file."
  (interactive "fConfig file: ")
  (let* ( (b (current-buffer))
	  (wf-vars (woozie--wf-vars-list) )
	  (config-vars (woozie--properties-from-file config-file))
	  (missing-vars (cl-set-difference wf-vars config-vars :test 'string=)))
    (with-output-to-temp-buffer woozie--msg-buff
      (switch-to-buffer woozie--msg-buff)
      (if missing-vars
	  (progn
	    (woozie--msg "--- Missing variable definitions:")
	    (dolist (var missing-vars)
	      (woozie--msg (concat "---   * " var))))
	(woozie--msg "+++ All workflow variables are defined."))
      (switch-to-buffer b))))

(defun woozie-wf-show-vars ()
  "Show a list of all workflow variables defined in the current buffer."
  (interactive)
  (woozie--msg-list "Workflow Variables:" (woozie--wf-vars-list)))

(defun woozie-hive-show-vars ()
  "Show a list of all the hive vars defined in the current buffer.
A hive var is any field delimited by '${hivevar:' and '}"
  (interactive)
  (woozie--msg-list "Hive Variables:" (woozie--hive-vars-list)))

(defun woozie-wf-mk-ascii ()
  "Show an (ASCII) graph representation of a workflow."
  (interactive)
  (let* ((dom (libxml-parse-xml-region (point-min) (point-max)))
	 (graph (woozie--wf-transitions dom))
	 (happy-path (woozie--graph-path-from "start" graph))
	 (buf (generate-new-buffer "wfgraph")))
    (switch-to-buffer buf)
    (woozie--graph-print happy-path)))

(defun woozie-wf-mk-dot ()
  "Create a buffer with a dot format representation of the workflow in the current buffer."
  (interactive)
  (let* ( (dom (libxml-parse-xml-region (point-min) (point-max)))
	  (nodes (woozie--wf-flow-nodes dom))
	  (ok-transitions (cl-remove-if (lambda (tr) (equal 'error (caddr tr))) (woozie--wf-transitions dom)))
	  (happy-transitions (woozie--wf-transitions-hp ok-transitions))
	  (happy-node-names (woozie--wf-transition-nodes happy-transitions))
	  (happy-nodes (cl-remove-if-not (lambda (n) (member (woozie--wf-node-name n) happy-node-names)) nodes)) 
	 )
    
    (switch-to-buffer (generate-new-buffer "workflow.dot"))
    (insert "strict digraph {\n")
    (insert "\n  // nodes\n")
    (dolist (node happy-nodes)
      (insert " " (woozie--dot-node node) "\n"))
    (insert "\n  // transitions\n")
    (dolist (edge happy-transitions)
      (insert (concat "  " (car edge) " -> " (cadr edge) "\n")))
    (insert "}\n")))

;;------------------------------------------------------------------------------------------------
;; DOT functions
;;
;; Note: these functions are only added if the =dot= tool is available
;;------------------------------------------------------------------------------------------------
(if (string-prefix-p "dot" (shell-command-to-string "dot -V"))
    (progn

      (message "WOOZIE: Found dot tool. Defining graph display functions.")
      
      (defun woozie-wf-view-dag (dag-type)
	"Visualize the workflow in the current buffer as a dag in the format defined by DAG-TYPE. Defaults to PNG"
	(interactive "simage type (PNG): ")
	(let* ( (img-type (if (equal "" dag-type) "png" dag-type))
		(dotfile (concat "/tmp/ooziewf." (number-to-string (emacs-pid)) ".dot"))
		(outfile (concat "/tmp/ooziewf." (number-to-string (emacs-pid)) "." (downcase img-type))))
	  (woozie-wf-mk-dot) 
	  (write-file dotfile)
	  (kill-buffer)
	  (shell-command (concat "dot -T" img-type " " dotfile " > " outfile))
	  (find-file outfile)))
      ))


;;------------------------------------------------------------------------------------------------
;; helper functions
;;------------------------------------------------------------------------------------------------

;; notes:
;;  + transitions are represented as a triple: (FROM . TO . type) where type is either 'ok or 'error

(defun woozie--wf-transitions (dom)
  "Return a list of all the transitions in the workflow specified by DOM.
Transitions are represented as a list of the form '(FROM TO type) 
where type is either 'ok or 'error."
  (let* ((nodes (dom-children dom)) )
      (mapcan 'woozie--wf-node-transitions nodes)))

(defun woozie--wf-transition-nodes (transitions)
  "Return a list of the name of all nodes in a set of TRANSITIONS."
  (let ( (froms (mapcar 'car transitions))
	 (tos   (mapcar 'cadr transitions)) )
    (delete-dups (append froms tos))))

(defun woozie--wf-transitions-hp (transitions)
  "Return the subset of TRANSITIONS that form the happy path.
Happy path is defined as all traversals reachable from node named 'start'."
  ;; note:
  ;; This is implemented by getting the list of transitions and recursively
  ;; removing all transitions whose 'FROM' nodes do not appear as TO nodes
  ;; in some other transition.
  (let* ( (froms (mapcar 'car transitions))
	  (tos   (cons "start" (mapcar 'cadr transitions)))
	  (no-in-edge (cl-set-difference froms tos :test #'equal)) )
    (if no-in-edge
	(woozie--wf-transitions-hp (cl-remove-if (lambda (x) (member (car x) no-in-edge)) transitions))
      transitions)))

(defun woozie--wf-node-transitions (node)
  "Return a list of the outbound transitions for NODE or an empty list if node has no transitions."

  (let ( (node-type (dom-tag node)))
    (cond ((equal 'start node-type) (list (list "start" (dom-attr node 'to) 'ok)))
	  ((equal 'action node-type) (woozie--wf-action-transitions node))
	  ((equal 'fork node-type) (woozie--wf-fork-transitions node))
	  ((equal 'join node-type) (list (list (dom-attr node 'name) (dom-attr node 'to) 'ok)))
	  ((equal 'decision node-type) (woozie--wf-decision-transitions node))
	  (t '()))))

(defun woozie--wf-action-transitions (action-node)
  "Extract transitions for ACTION-NODE."
  (let ( (ok-transition (list (dom-attr action-node 'name)    (dom-attr (car (dom-by-tag action-node 'ok)) 'to) 'ok))
	 (error-transition (list (dom-attr action-node 'name) (dom-attr (car (dom-by-tag action-node 'error)) 'to) 'error)) )
    (list ok-transition error-transition)))

(defun woozie--wf-fork-transitions (fork-node)
  "Extract transitions for FORK-NODE."
  (let ((from (dom-attr fork-node 'name)))
    (mapcar (lambda (path) (list from (dom-attr path 'start) 'ok)) (dom-by-tag fork-node 'path))))

(defun woozie--wf-decision-transitions (decision-node)
  "Extract transitions for DECISION-NODE."
  (let* ((from (dom-attr decision-node 'name))
	 (switch-node (car (dom-by-tag decision-node 'switch)))
	 )
    (mapcar (lambda (case) (list from (dom-attr case 'to) 'ok)) (append (dom-by-tag switch-node 'case) (dom-by-tag switch-node 'default)))))

(defun woozie--graph-print (path)
  "Print PATH as a series of ascii boxes."
  (let* ( (width (+ 8 (apply 'max (mapcar 'length path))))
	  (first-step (car path))
	  (other-path (cdr path)))
    (insert (woozie--graph-box first-step width))
    (dolist (element other-path)
      (insert (concat (woozie--str-pad "|" width) "\n" (woozie--graph-box element width))))))

(defun woozie--str-pad (str size)
  "Pad STR with blanks on both sides to SIZE."
  (let* ( (slack (- size (length str)))
	  (pad (make-string (/ slack 2) ?\s))
	  (val (concat pad str pad)))
    (if (equal size (length val))
	val
      (concat " " val))))

(defun woozie--graph-box (val width)
  "Return an ascii box of the specified WIDTH and label VAL."
  (let* ( (top (concat "+-" (make-string (length val) ?-)  "-+"))
	  (middle (concat "| " val " |"))
	  (bottom top))
    (concat (woozie--str-pad top width) "\n" (woozie--str-pad middle width) "\n" (woozie--str-pad bottom width) "\n")))

(defun woozie--graph-path-from (first-node-name transitions)
  "Create a path from FIRST-NODE-NAME to the end using TRANSITIONS."
  (if first-node-name
      (cons first-node-name (woozie--graph-path-from (woozie--graph-to first-node-name transitions) transitions))
    '()))

(defun woozie--graph-to (node-name transitions)
  "Return thhe name of the node NODE-NAME transitions to from the set of TRANSITIONS."
  (let* ( (cur-transition (car transitions)))
    (cond ( (not transitions) nil )
	  ( (and (equal node-name (car cur-transition))
		 (equal 'ok  (caddr cur-transition))) (cadr cur-transition))
	  ( t (woozie--graph-to node-name (cdr transitions))))))


(defun woozie--wf-from-to (flow-node)
  "Return the current node name, its type and the to node for FLOW-NODE."
  (let ( (node-type (dom-tag flow-node)))
    (cond ( (equal node-type 'start)
	    (list "start" "start" (dom-attr flow-node 'to)))
	  ( (equal node-type 'end)
	    (list (dom-attr flow-node 'name) "end" nil))
	  ( t
	    (list (dom-attr flow-node 'name)
		  (symbol-name (dom-tag flow-node))
		  (dom-attr (dom-by-tag flow-node 'ok) 'to))))))

(defun woozie--msg-list (header list)
  "Insert HEADER and messages in LIST into the current buffer, one per line."
  (woozie--msg header)
  (dolist (elem list)
    (woozie--msg elem)))
  
(defun woozie--wf-vars-list ()
  "Return a list of all vars defined in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-remove-if-not 'woozie--valid-wf-var (woozie--find-delimited-from-point "${" "}"))))

(defun woozie--properties-from-file (config-file)
  "Return a list of the property names defined in CONFIG-FILE."
  (let ( (lines (woozie--file-as-line-list config-file)))
    (mapcan (lambda (l) (if (string-match  "^\\([^#=]+\\)=.*" l) (list (match-string 1 l)) '())) lines)))

(defun woozie--file-as-line-list (filename)
  "Return the contents of FILENAME as a list of strings where each string is one line in the file."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))
  
(defun woozie--msg (msg)
  "Insert MSG and a newline in the current buffer."
  (insert msg)
  (insert "\n"))

(defun woozie--validate-action-transitions (dom)
  "Check if all action transitions in the workflow DOM are valid ones."
  (let* ( (destinations (woozie--wf-flow-node-names dom))
	  (transitions (woozie--wf-transitions dom))
	  (transition-destinations (append (mapcar 'cadr transitions) '("start")))
	  (bad-transitions (cl-remove-if (lambda (x) (member (cadr x) destinations)) transitions))
	  (unused-actions  (cl-set-difference destinations transition-destinations :test 'string=)))
    (if (or bad-transitions unused-actions)
	(progn
	  (woozie--msg "--- TRANSITION ERRORS!")
	  (dolist (elem bad-transitions)
	    (woozie--msg (concat "---   bad destination for transition: " (car elem) " -> " (cadr elem))))
	  (dolist (elem unused-actions)
	    (woozie--msg (concat "~~~   no transitions exist to node " elem))))
      (progn
	(woozie--msg "+++ All transitions are valid.")
	(woozie--msg "+++ All nodes have incoming transitions.")))))

(defun woozie--validate-action-names (dom)
  "Prints a report on the *Woozie* buffer on the action names defined in the workflow DOM."
  (let* ( (action-names (woozie--wf-flow-node-names dom) )
	  (repeated-names (woozie--list-duplicates action-names)))
    (if repeated-names
      (progn
	(woozie--msg "--- ACTION ERRORS!")
	(woozie--msg "--- The following action names are repeated: ")
	(dolist (elem repeated-names)
	  (woozie--msg (concat "---    " elem))))
      (woozie--msg (concat "+++ " (number-to-string (length action-names)) " action names, all unique")))))

(defun woozie--wf-is-flow-node-p (node)
  "Return non-nil if NODE participates in a flow."
  (member (dom-tag node) '(start action decision join fork end kill)))

(defun woozie--wf-node-name (node)
  "Return the value of the name attribute, if NODE has one, or the dom-tag/element name if not."
  (let ( (name (dom-attr node 'name)))
    (if name
	name
      (symbol-name (dom-tag node)))))

(defun woozie--wf-flow-nodes (dom)
  "Return all flow nodes in the workflow DOM. 
Flow nodes are all nodes that the workflow can transition through, including `start` and `end`"
  (let ( (top-level-nodes (dom-children dom)))
    (cl-remove-if-not 'woozie--wf-is-flow-node-p top-level-nodes)))
  
(defun woozie--wf-flow-node-names (dom)
  "Return the name of all flow nodes in thhe workflow DOM."
    (mapcar 'woozie--wf-node-name (woozie--wf-flow-nodes dom)))

(defun woozie--dot-node (node)
  "Return a string with the dot NODE definition."
  (let ( (type (dom-tag node))
	 (name (woozie--wf-node-name node)))
    (concat name " " (alist-get type woozie-dot-node-attribs))))

(defun woozie--wf-get-attr (attrib nodes)
  "Return a list with the value of ATTRIB for all NODES in the list."
  (mapcar (lambda (n) (dom-attr n attrib)) nodes))
    
(defun woozie--find-all-delimited (delim1 delim2 &optional include-dupes)
 "Find all strings delimited by DELIM1 and DELIM2 in the current buffer.
Non-nil INCLUDE-DUPES allows the returned strings to be duplicates."
  (save-excursion
    (goto-char (point-min))
    (woozie--find-delimited-from-point delim1 delim2 include-dupes)))

(defun woozie--find-delimited-from-point (delim1 delim2 &optional include-dupes)
  "Return a list with all values in the current buffer bounded by DELIM1 and DELIM2.
Values are unique unless INCLUDE-DUPES is non-nil."
  (let* ( (start (search-forward delim1 nil 't))
	  (end   (progn (search-forward delim2 nil 't) (backward-char) (point)))
	  (vals  '()))
    (while (and start end)
      (let ( (new-var (buffer-substring-no-properties start end)) )
	(if (or include-dupes (not (member new-var vals)))
	    (setq vals (cons (buffer-substring-no-properties start end) vals)))
	(setq start (search-forward delim1 nil 't))
	(setq end   (progn (search-forward delim2 nil 't) (backward-char) (point)))))
    vals))

(defun woozie--hive-vars-list ()
  "Return a list of all hive vars defined in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (woozie--find-delimited-from-point "${hivevar:" "}")))

(defun woozie--hive-vars (filename)
  "Get all the hive vars in FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (woozie--hive-vars-list)))

(defun woozie--valid-wf-var (var)
  "Return non-nil if VAR is a valid oozie variable name."
  (not (string-prefix-p "wf:" var)))

(defun woozie--get-line ()
  "Get the current line (the one including point) from the current buffer."
  (let ( (start (line-beginning-position))
	 (end   (line-end-position))
	 )
    (if (< start end)
	(buffer-substring-no-properties start end)
      '())))

(defun woozie--list-duplicates (l &optional cur-dups)
  "Return all elements in L that occur more than once.
CUR-DUPS is for internal use and should be ignored."
  (let ( (first (car l))
	 (rest (cdr l)) )
    (cond ( (equal l '()) cur-dups )
	  ( (member first cur-dups) (woozie--list-duplicates rest cur-dups))
	  ( (member first rest)  (woozie--list-duplicates rest (cons first cur-dups)))
	  ( t  (woozie--list-duplicates rest cur-dups)))))

(provide 'woozie)
;;; woozie.el ends here