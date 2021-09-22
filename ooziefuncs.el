;;;  -*- lexical-binding: t; -*-
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
;;; Code

(require 'cl-lib)
(require 'dom)

;;----------------------------------------------------------------------------------------
;; user-configurable variables
;;----------------------------------------------------------------------------------------

;; provides the attributes used in DOT node definitions. Users can  change these values
;; to set shape, color, font, etc.
(defvar oozie-dot-node-attribs (list (cons 'start    "[shape=doublecircle]")
				     (cons 'end      "[shape=doublecircle]")
				     (cons 'action   "")
				     (cons 'fork     "[shape=box]")
				     (cons 'join     "[shape=box]")
				     (cons 'decision "[shape=diamond]"))
  "An association list mapping workflow element types to their respective DOT node attributes.")



;;----------------------------------------------------------------------------------------
;; user (interactive) functions
;;----------------------------------------------------------------------------------------

(defun oozie-wf-mk (jobname)
  "Adds a skeleton workflow definition to the current buffer"
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

(defun oozie-wf-action-hive (action-name hive-script)
  "Inserts a oozie workflow hive action"
  (interactive "saction name: \nfhive script: ")
  (let ( (hivevars (oozie-hive-vars hive-script)))
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

(defun oozie-wf-action-email (action-name email)
  "Insert an oozie workflow email action"
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

(defun oozie-wf-action-shell (action-name cmd-str)
  "inserts an oozie workflow shell action"
  (interactive "saction name: \nscommand (with arguments): ")
  (let ( (script (car (split-string cmd-str)))
	 (args   (cdr (split-string cmd-str))))
    (insert "
    <action name=\"" action-name "\" cred=\"hcat_creds\">
        <shell xmlns=\"uri:oozie:shell-action:0.3\">
            <exec>" script "</exec>
")
    (dolist (argument args)
      (oozie-argument argument)
      (insert "\n"))
    (insert "        </shell>
        <ok to=\"end\"/>
        <error to=\"fail\" />
    </action>
"
    )))

(defun oozie-wf-action-property (name value)
  "Inserts a property tag"
  (interactive "sproperty name: \nsproperty value: ")
  (insert
   "
        <property>
            <name>" name "</name>
            <value>" value "</value>
        </property>
"
	    ))

(defun oozie-wf-action-argument (arg-value)
  "inserts an argument tag"
  (interactive "sarg value: ")
  (insert "            <argument>" arg-value "</argument>"))

(defun oozie-wf-action-file (filename)
  "Inserts a file tag"
  (interactive "sfilename: ")-
  (insert "            <file>" filename "</file>"))

(defun oozie-wf-validate ()
  "Performs validations on the oozie workflow xml defined in the current buffer. This includes:
  * Actions have unique names
  * Transitions are to existing nodes
  * All relevant nodes have incoming transitions
"
  (interactive)
  (let ( (dom (libxml-parse-xml-region (point-min) (point-max))) )
    (with-output-to-temp-buffer "*Oozie*"
      (oozie--msg "=======================================================")
      (oozie--msg "Validating workflow.....")
      (oozie--validate-action-names dom)
      (oozie--validate-action-transitions dom))))

(defun oozie-wf-validate-config (config-file)
  "
Validates the workflow definiton in the current buffer
agains the specified CONFIG-FILE. Provides a list of 
variables not defined in the configuration file."
  (interactive "fConfig file: ")
  (let* ( (wf-vars (oozie--wf-vars-list) )
	  (config-vars (oozie--properties-from-file config-file))
	  (missing-vars (cl-set-difference wf-vars config-vars :test 'string=)))
    (with-output-to-temp-buffer "*Oozie*"
      (if missing-vars
	  (progn
	    (oozie--msg "--- Missing variable definitions:")
	    (dolist (var missing-vars)
	      (oozie--msg (concat "---   * " var))))
	(oozie--msg "+++ All workflow variables are defined.")))))

(defun oozie-wf-show-vars ()
  "Shows a list of all workflow variables defined in the current buffer"
  (interactive)
  (oozie--msg-list "Workflow Variables:" (oozie--wf-vars-list)))

(defun oozie-hive-show-vars ()
  "Shows a list of all the hive vars defined in the current buffer.
   A hive var is any field delimited by '${hivevar:' and '}"
  (interactive)
  (oozie--msg-list "Hive Variables:" (oozie--hive-vars-list)))

(defun oozie-wf-mk-ascii ()
  "Shows an (ASCII) graph representation of a workflow"
  (interactive)
  (let* ((dom (libxml-parse-xml-region (point-min) (point-max)))
	 (graph (oozie--graph-build dom))
	 (happy-path (oozie--graph-path-from "start" graph))
	 (buf (generate-new-buffer "wfgraph")))
    (switch-to-buffer buf)
    (oozie--graph-print happy-path)))

(defun oozie-wf-mk-dot ()
  "Creates a buffer with a dot format representation of the workflow in the current buffer."
  (interactive)
  (let* ( (dom (libxml-parse-xml-region (point-min) (point-max)))
	  (nodes (oozie--wf-flow-nodes dom))
	  (ok-transitions (cl-remove-if (lambda (tr) (equal 'error (caddr tr))) (oozie--wf-transitions dom)))
	  (happy-transitions (oozie--wf-transitions-hp ok-transitions))
	  (happy-node-names (oozie--wf-transition-nodes happy-transitions))
	  (happy-nodes (cl-remove-if-not (lambda (n) (member (oozie--wf-node-name n) happy-node-names)) nodes)) 
	 )
    
    (switch-to-buffer (generate-new-buffer "workflow.dot"))
    (insert "strict digraph {\n")
    (insert "\n  // nodes\n")
    (dolist (node happy-nodes)
      (insert " " (oozie--dot-node node) "\n"))
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

      (message "OOZIE: Found dot tool. Defining graph display functions.")
      
      (defun oozie-wf-view-dag (dag-type)
	"Visualize the workflow in the current buffer as a dag in the format defined by DAG-TYPE. Defaults to PNG"
	(interactive "simage type (PNG): ")
	(let* ( (buffer-modified-p nil)
		(img-type (if (equal "" dag-type) "png" dag-type))
		(dotfile (concat "/tmp/ooziewf." (number-to-string (emacs-pid)) ".dot"))
		(outfile (concat "/tmp/ooziewf." (number-to-string (emacs-pid)) "." (downcase img-type))))
	  (oozie-wf-mk-dot) 
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

(defun oozie--wf-transitions (dom)
  "Returns a list of all the transitions in the workflow, where each transitions is represented as (from . to).
   If INCLUDE-ERROR is true, also include error transitions"
  (let* ((nodes (dom-children dom)) )
      (mapcan 'oozie--wf-node-transitions nodes)))

(defun oozie--wf-transition-nodes (transitions)
  "Given a list of transitions, returns all the nodes specified in it."
  (let ( (froms (mapcar 'car transitions))
	 (tos   (mapcar 'cadr transitions)) )
    (delete-dups (append froms tos))))

(defun oozie--wf-transitions-hp (transitions)
  "Returns only the happy path transitions for the list.

   Happy path is defined as all traversals reachable from node named 'start'."
  (let* ( (froms (mapcar 'car transitions))
	  (tos   (cons "start" (mapcar 'cadr transitions)))
	  (no-in-edge (cl-set-difference froms tos :test #'equal)) )
    (if no-in-edge
	(oozie--wf-transitions-hp (cl-remove-if (lambda (x) (member (car x) no-in-edge)) transitions))
      transitions)))

(defun oozie--wf-node-transitions (node)
  "Returns a list containing the transitions for this node, if it is a flow node, return an empty list otherwise.
   If INCLUDE-ERROR is true, also include error transtions"
  (let ( (node-type (dom-tag node)))
    (cond ((equal 'start node-type) (list (list "start" (dom-attr node 'to) 'ok)))
	  ((equal 'action node-type) (oozie--wf-action-transitions node))
	  ((equal 'fork node-type) (oozie--wf-fork-transitions node))
	  ((equal 'join node-type) (list (list (dom-attr node 'name) (dom-attr node 'to) 'ok)))
	  ((equal 'decision node-type) (oozie--wf-decision-transitions node))
	  (t '()))))

(defun oozie--wf-action-transitions (action-node)
  (let ( (ok-transition (list (dom-attr action-node 'name)    (dom-attr (car (dom-by-tag action-node 'ok)) 'to) 'ok))
	 (error-transition (list (dom-attr action-node 'name) (dom-attr (car (dom-by-tag action-node 'error)) 'to) 'error)) )
    (list ok-transition error-transition)))


(defun oozie--wf-fork-transitions (fork-node)
  "Extracts transitions for a fork node"
  (let ((from (dom-attr fork-node 'name)))
    (mapcar (lambda (path) (list from (dom-attr path 'start) 'ok)) (dom-by-tag fork-node 'path))))

(defun oozie--wf-decision-transitions (decision-node)
  "Extracts transitions for a decision node"
  (let* ((from (dom-attr decision-node 'name))
	 (switch-node (car (dom-by-tag decision-node 'switch)))
	 )
    (mapcar (lambda (case) (list from (dom-attr case 'to) 'ok)) (append (dom-by-tag switch-node 'case) (dom-by-tag switch-node 'default)))))

(defun oozie--graph-print (path)
  "Prints the path as specified."
  (let* ( (width (+ 8 (apply 'max (mapcar 'length path))))
	  (first-step (car path))
	  (other-path (cdr path)))
    (insert (oozie--graph-box first-step width))
    (dolist (element other-path)
      (insert (concat (oozie--str-pad "|" width) "\n" (oozie--graph-box element width))))))

(defun oozie--str-pad (str size)
  "Pads strings with blanks on both sides to be of the specified size"
  (let* ( (slack (- size (length str)))
	  (pad (make-string (/ slack 2) ?\s))
	  (val (concat pad str pad)))
    (if (equal size (length val))
	val
      (concat " " val))))

(defun oozie--graph-box (val width)
  "Returns an ASCII representation of a box with width WIDTH and text VAL."
  (let* ( (top (concat "+-" (make-string (length val) ?-)  "-+"))
	  (middle (concat "| " val " |"))
	  (bottom top))
    (concat (oozie--str-pad top width) "\n" (oozie--str-pad middle width) "\n" (oozie--str-pad bottom width) "\n")))

(defun oozie--graph-build (dom)
  "Give the xml representation of the workflow, build the corresponding execution graph"
  (let* ((flow-nodes (cl-remove-if-not 'oozie--wf-is-graph-node (dom-children dom))))
    (mapcar 'oozie--wf-from-to flow-nodes)))

(defun oozie--graph-path-from (first-node-name graph)
  (if (not first-node-name)
      '()
    (cons first-node-name (oozie--graph-path-from (oozie--graph-to first-node-name graph) graph))))

(defun oozie--graph-to (node-name graph)
  (let* ( (current-node (car graph)))
    (cond ( (not graph) graph)
	  ( (equal node-name (car current-node)) (car (cdr (cdr current-node))))
	  ( t (oozie--graph-to node-name (cdr graph))))))

(defun oozie--wf-from-to (flow-node)
  "Return the current node name, its type and the to node for the action"
  (let ( (node-type (dom-tag flow-node)))
    (cond ( (equal node-type 'start)
	    (list "start" "start" (dom-attr flow-node 'to)))
	  ( (equal node-type 'end)
	    (list (dom-attr flow-node 'name) "end" nil))
	  ( t
	    (list (dom-attr flow-node 'name)
		  (symbol-name (dom-tag flow-node))
		  (dom-attr (dom-by-tag flow-node 'ok) 'to))))))

(defun oozie--msg-list (header list)
  (oozie--msg header)
  (dolist (elem list)
    (oozie--msg elem)))
  
(defun oozie--wf-vars-list ()
  "Returns a list of all vars defined in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (cl-remove-if-not 'oozie-valid-wf-var (oozie--find-delimited-from-point "${" "}"))))

(defun oozie--properties-from-file (config-file)
  "Returns a list of properties defined in CONFIG-FILE"
  (let ( (lines (oozie--file-as-line-list config-file))
	 (props '() ))
    (dolist (line lines)
      (if (not (string-prefix-p "#" line))
	  (setq props (cons (substring line 0 (string-match "=" line)) props))))
    props))

(defun oozie--file-as-line-list (filename)
  "Returns the contents of FILENAME as a list of strings where each string is one line in the file"
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))
  
(defun oozie--msg (msg)
  (prin1 msg)
  (prin1 "\n"))

(defun oozie--validate-action-transitions (dom)
  "Checks if all action transitions are valid ones"
  (let* ( (destinations (oozie--wf-flow-node-names dom))
	  (transitions (oozie--wf-transitions dom))
	  (transition-destinations (append (mapcar 'cadr transitions) '("start")))
	  (bad-transitions (cl-remove-if (lambda (x) (member (cadr x) destinations)) transitions))
	  (unused-actions  (cl-set-difference destinations transition-destinations :test 'string=)))
    (if (or bad-transitions unused-actions)
	(progn
	  (oozie--msg "--- TRANSITION ERRORS!")
	  (dolist (elem bad-transitions)
	    (oozie--msg (concat "---   bad destination for transition: " (car elem) " -> " (cadr elem))))
	  (dolist (elem unused-actions)
	    (oozie--msg (concat "~~~   no transitions exist to node " elem))))
      (progn
	(oozie--msg "+++ All transitions are valid.")
	(oozie--msg "+++ All nodes have incoming transitions.")))))

(defun oozie--validate-action-names (dom)
  "Prints a report on the *Oozie* buffer on action names. Gives total count and flags names that are not unique."
  (let* ( (action-names (oozie--wf-flow-node-names dom) )
	  (repeated-names (oozie--list-duplicates action-names)))
    (if repeated-names
      (progn
	(oozie--msg "--- ACTION ERRORS!")
	(oozie--msg "--- The following action names are repeated: ")
	(dolist (elem repeated-names)
	  (oozie--msg (concat "---    " elem))))
      (oozie--msg (concat "+++ " (number-to-string (length action-names)) " action names, all unique")))))

(defun oozie--wf-is-flow-node-p (node)
  "Returns true if node is a node that participates in a flow"
  (member (dom-tag node) '(start action decision join fork end kill)))


(defun oozie--wf-is-graph-node (node)
  (let ( (n (dom-tag node)) )
    (or (equal n 'action)
	(equal n 'start)
	(equal n 'end))))

(defun oozie--wf-node-name (node)
  "Returns the value of the name attribute, if the node has one, or the dom-tag/element name if not."
  (let ( (name (dom-attr node 'name)))
    (if name
	name
      (symbol-name (dom-tag node)))))

(defun oozie--wf-flow-nodes (dom)
  "Returns all flow nodes in the workflow definition. Flow nodes are all nodes that the workflow can 
   transition through, including `start` and `end`"
  (let ( (top-level-nodes (dom-children dom)))
    (cl-remove-if-not 'oozie--wf-is-flow-node-p top-level-nodes)))
  
(defun oozie--wf-flow-node-names (dom)
  "Returns the names of all flow nodes (action, decision, fork, etc.) in the current buffer xml"
    (mapcar 'oozie--wf-node-name (oozie--wf-flow-nodes dom)))

(defun oozie--dot-node (node)
  "Returns a string with the DOT node definition."
  (let ( (type (dom-tag node))
	 (name (oozie--wf-node-name node)))
    (concat name " " (alist-get type oozie-dot-node-attribs))))



(defun oozie--wf-get-attr (attrib nodes)
  "Given a list of nodes, returns a list with the value of _attrib_ for all those nodes."
  (mapcar (lambda (n) (dom-attr n attrib)) nodes))
    
(defun oozie-config-vars ()
  "Gets a list of all defined (i.e., in the oozie-vars buffer) variables"
  (let ( (cbuff (current-buffer))
	 (config-vars '()))
    (pop-to-buffer "oozie-vars")
    (goto-char (point-min))
    (let ( (cur-line (oozie-get-line)) )
      (while cur-line
	(setq config-vars (cons cur-line config-vars))
	(forward-line)
	(setq cur-line (oozie-get-line))))
    (pop-to-buffer cbuff)
    config-vars))

(defun oozie--find-all-delimited (delim1 delim2 &optional include-dupes)
  "
Finds all strings delimited by DELIM1 and DELIM2 in the current
current buffer. 
"
  (save-excursion
    (goto-char (point-min))
    (oozie--find-delimited-from-point delim1 delim2 include-dupes)))


(defun oozie--find-delimited-from-point (delim1 delim2 &optional include-dupes)
  "Returns a list with all (possibly unique) values in the current buffer bounded by the two delimiters,
   starting at the current point."
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

(defun oozie--hive-vars-list ()
  "Returns a list of all hive vars defined in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (oozie--find-delimited-from-point "${hivevar:" "}")))

(defun oozie-hive-vars (filename)
  "Gets all the hive vars in FILENAME"
  (with-temp-buffer
    (insert-file-contents filename)
    (oozie--hive-vars-list)))

(defun oozie-valid-wf-var (var)
  (not (string-prefix-p "wf:" var)))

(defun oozie-get-line ()
  "Gets the current line (the one including point) from the current buffer"
  (let ( (start (line-beginning-position))
	 (end   (line-end-position))
	 )
    (if (< start end)
	(buffer-substring-no-properties start end)
      '())))

(defun oozie--list-duplicates (l &optional cur-dups)
  "Given a list, return all elements that occur more than once."
  (let ( (first (car l))
	 (rest (cdr l)) )
    (cond ( (equal l '()) cur-dups )
	  ( (member first cur-dups) (oozie--list-duplicates rest cur-dups))
	  ( (member first rest)  (oozie--list-duplicates rest (cons first cur-dups)))
	  ( t  (oozie--list-duplicates rest cur-dups)))))
   
