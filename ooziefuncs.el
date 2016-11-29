;;; ooziefuncs.el --- functions for building oozie workflows 

;; Copyright (c) 2016, Target Corp.
;;
;; Authors: alexandre.santoro@target.com
;; Keywords: oozie workflow / hive 
;; Version: 0.1.0-SNAPSHOT

;;; Commentary:
;;
;; ooziefuncs contains a collection of functions that simplify creating oozie workflows.
;; These include functions for:
;;    * adding actions tot the current workflow.xml buffer
;;    * extracting variable information from one (or more) workflow.xml files and adding
;;      them to a config buffer, which can then serve as the basis for creating a confi-
;;      guration file.
;;
;; TODO LIST:
;;    * hive action extracts and adds vars
;;    * svg workflow diagram generation

;;; Code

(require 'cl-lib)

;; commands

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
        <hive xmlns=\"uri:oozie:hive-action:0.2\">
            <job-tracker>${jobTracker}</job-tracker>
            <name-node>${nameNode}</name-node>
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
        <email>
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
        <shell xmlns=\"uri:oozie:shell-action:0.1\">
            <job-tracker>${jobTracker}</job-tracker>
            <name-node>${nameNode}</name-node>
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
  "
Validates the current workflow, checking:
  * actions have unique names
  * transitions are valid"
  (interactive)
  (oozie--msg-clr)
  (oozie--msg "\n=======================================================")
  (oozie--msg "Validating workflow.....")
  (oozie--validate-action-names)
  (oozie--validate-action-transitions)
  )

(defun oozie-wf-validate-config (config-file)
  "
Validates the workflow definiton in the current buffer
agains the specified CONFIG-FILE. Provides a list of 
variables not defined in the configuration file."
  (interactive "fConfig file: ")
  (let* ( (wf-vars (oozie--workflow-vars) )
	  (config-vars (oozie--properties-from-file config-file))
	  (missing-vars (cl-set-difference wf-vars config-vars :test 'string=)))
    (if missing-vars
	(progn
	  (oozie--msg "--- Missing variable definitions:")
	  (dolist (var missing-vars)
	    (oozie--msg (concat "---   * " var))))
      (oozie--msg "+++ All workflow variables are defined."))))
    

;; helper functions

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
  "Prints MSG in the oozie message screen"
  (let ( (cbuff (current-buffer)) )
    (pop-to-buffer "oozie-messages")
    (goto-char (point-max))
    (insert msg "\n")
    (pop-to-buffer cbuff)))

(defun oozie--msg-clr ()
  "Clears the oozie message window buffer"
  (let ( (cbuff (current-buffer)) )
    (pop-to-buffer "oozie-messages")
    (erase-buffer)
    (pop-to-buffer cbuff)))

(defun oozie-workflow-vars()
  "Returns a list of all vars defined in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (oozie-filter 'oozie-valid-wf-var (oozie--find-delimited-from-point "${" "}"))))

(defun oozie--validate-action-transitions ()
  "Checks if all action transitions are valid ones"
  (let* ( (end-names  (oozie--find-all-delimited "end name=\"" "\""))
	  (kill-names (oozie--find-all-delimited "kill name=\"" "\""))
	  (action-names (oozie--wf-action-names))
	  (destinations (append end-names kill-names action-names))
	  (transition-names (oozie--wf-transition-names))
	  (bad-transitions (cl-set-difference transition-names destinations :test 'string=))
	  (unused-actions  (cl-set-difference destinations transition-names :test 'string=)))
    (if bad-transitions
	(progn
	  (oozie--msg "--- Bad transitions exist.")
	  (dolist (elem bad-transitions)
	    (oozie--msg (concat "---   bad transition to " elem))))
      (oozie--msg "+++ All transitions are valid."))
    (if unused-actions
	(progn
	  (oozie--msg "~~~ The following actions are not used:")
	  (dolist (elem unused-actions)
	    (oozie--msg (concat "~~~   " elem " action is not used.")))))))

(defun oozie--validate-action-names ()
  (let* ( (action-names (oozie--wf-action-names) )
	  (unique-action-names (remove-duplicates  action-names :test 'string=))
	  (repeated-names (cl-set-difference action-names unique-action-names :test 'string=)))
    
    (if (= (length action-names) (length unique-action-names))
	(oozie--msg (concat "+++ " (number-to-string (length action-names)) " action names, all unique"))
      (progn
	(oozie--msg "--- Action names are NOT unique")
	(oozie--msg "--- The following action names are repeated: ")
	(dolist (elem repeated-names)
	  (oozie--msg (concat "---    " elem)))))))
    
(defun oozie--wf-action-names ()
  "Returns a list of action names"
  (save-excursion
    (goto-char (point-min))
    (oozie--find-delimited-from-point "<action name=\"" "\"" 't)))

(defun oozie--wf-transition-names ()
  "Returns a list of transition targets"
  (save-excursion
    (goto-char (point-min))
    (oozie--find-delimited-from-point "to=\"" "\"")))

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
  "Returns a list with all **unique** values in the current buffer bounded by the two delimiters,
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

(defun oozie-hive-vars (filename)
  "Gets all the hive vars in FILENAME"
  (let ( (result '()) )
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (setq result (oozie--find-delimited-from-point "${hivevar:" "}"))
      result)))


(defun oozie-valid-wf-var (var)
  (not (string-prefix-p "wf:" var)))

(defun oozie-get-line ()
  "Gets the line from the current buffer"
  (let ( (start (line-beginning-position))
	 (end   (line-end-position))
	 )
    (if (< start end)
	(buffer-substring-no-properties (line-beginning-position) (line-end-position))
      '())))

(defun oozie-filter (pred list)
  "Filters the list recursively"
  (let ( (rfl '()) )
    (dolist (elem list)
      (if (funcall pred elem)
	  (setq rfl (cons elem rfl))))
    (reverse rfl)))

