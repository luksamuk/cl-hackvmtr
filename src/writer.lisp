;;;; writer.lisp
;;;; Part of cl-hackvmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.


(in-package :cl-hackvmtr)

(defun vm-parse-commands (command-list)
  (loop for command in command-list
     append (vm-dispatch-command command)))

(defun pretty-print-commands (translated-commands)
  (format t "~{~a~%~}" translated-commands))

(defun write-asm-file (file-path command-list)
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
  (flatten (list (vm-initialization)
		 (loop for file-commands in multiple-file-commands
		    append (vm-parse-commands file-commands))
		 (vm-halt)
		 (setf *arith-tst-flag* 0))))
