;;;; writer.lisp
;;;; Part of cl-hackvmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.


(in-package :cl-hackvmtr)

(defun vm-parse-commands (command-list)
  "Takes a list of VM commands and generates a list of assembly commands,
corresponding to the operations, in assembly for the Hack platform."
  (loop for command in command-list
     append (vm-dispatch-command command)))

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


(defun vm-parse-all-commands (multiple-file-commands)
  "Takes a list, where each element is also a list corresponding to the VM
commands of a single file, and translates everything into a single, flat list of
assembly commands for the Hack platform."
  (flatten (list (vm-initialization)
		 (loop for file-commands in multiple-file-commands
		    append (vm-parse-commands file-commands))
		 (vm-halt)
		 (setf *arith-tst-flag* 0))))
