;;;; interface.lisp
;;;; Part of cl-hackvmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package :cl-hackwmtr)

(defun read-vm-input (file-or-dir)
  (cond ((uiop:directory-exists-p file-or-dir) ; is a directory
	 (directory (concatenate 'string
				 file-or-dir
				 "/*.vm"))) ; TODO: Get folder name
	((uiop:file-exists-p file-or-dir)
	 (list (file-namestring file-or-dir) ; TODO: cleanup filename
	       (list (probe-file file-or-dir))))
	(t (error "Cannot open file."))))

(defun read-vm-file (file-path)
  (cleanup-commands
   (handler-case
       (with-open-file (stream file-path
			       :direction :input)
	 (loop for line = (read-line stream nil)
	    while line
	    collect line))
     (error () nil))))

;; Module name?
;; (write-asm-file "/home/alchemist/testvm/testvm.asm"
;; 		(vm-parse-all-commands
;; 		 (mapcar #'read-vm-file
;; 			 (read-vm-input "/home/alchemist/testvm/"))))
