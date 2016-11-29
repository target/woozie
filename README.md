# OozieEmacsLib

Oozie functionality in the emacs environment!

This repository includes code for Oozie-related productivity while using Emacs. This includes,
but is not limited to:
* functions for adding xml snippets into a workflow.xml file
* functions for validating xml workflows
* functions for extracting variable lists from different types of Hadoop/Ooozie files

Note: This is work in progress and my first ever attempt at writing emacs lisp code, so be 
kind and patient as we learn and the code evolves.

# Running Unit Tests

To run unit tests for this code: `emacs --script ooziefuncstest.el`

# Installation

Add the following to your .emacs file:
```lisp
(load-file "path/to/ooziefuncs.el")
```

All functions start with `oozie-`, so feel free to use `ctrl-h f oozie-<TAB>` to explore
the available functionality.






