;;;; writer.lisp
;;;; Part of cl-hackvmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.


(in-package :cl-hackvmtr)

(defun vm-parse-commands (command-list &optional (file-name nil))
  "Takes a list of VM commands and generates a list of assembly commands,
corresponding to the operations, in assembly for the Hack platform."
  (loop for command in command-list
     append (vm-dispatch-command command file-name)))

(defun pretty-print-commands (translated-commands)
  "Pretty-prints all commands, line by line, on the output stream."
  (format t "~{~a~%~}" translated-commands))

(defun write-asm-file (file-path command-list)
  "Writes post-translation assembly commands in COMMAND-LIST to FILE-PATH."
  (handler-case
      (with-open-file (stream file-path
			      :direction :output
			      :if-exists :supersede)
	(format stream "~a"
		(with-output-to-string (*standard-output*)
		  (pretty-print-commands command-list)))
	t)
    (error () (format t "Cannot output to file.~%") nil)))


(defun vm-parse-all-commands (multiple-file-specs &optional (no-bootstrap nil))
  "Takes a list of file specs. Each spec is another list, comprised of a string
file name (without extension) and a list of commands contained in the file. Each
spec is then translated into assembly commands for the Hack platform, and
flattened into a single depth-one list of assembly commands. NO-BOOSTRAP
optionally informs whether the translation should contain the bootstrap code for
setting up the stack and calling 'Sys.init'."
  (flatten (list (unless no-bootstrap
		   (vm-initialization))
		 (loop for spec in multiple-file-specs
		    append (vm-parse-commands (cadr spec)
					      (car spec)))
		 ;; (vm-halt)
		 (setf *arith-tst-flag*     0
		       *funcall-ret-flag*   0
		       *current-filename* nil))))

