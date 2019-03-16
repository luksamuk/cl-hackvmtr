;;;; interface.lisp
;;;; Part of cl-hackvmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package :cl-hackvmtr)

(defun read-vm-input (file-or-dir)
  "Takes the path of a file or a directory given as input, and returns a list
containing two things. The first should be the name for the translated module,
deduced from file or directory name, and the second should be the list of files
which will be compiled."
  (labels ((module-from-path (path)
	     (cond ((equal (search ".vm" path :from-end t)
			   (- (length path) 3))
		    (subseq path
			    (1+ (or (search "/" path :from-end t) -1))
			    (- (length path) 3)))
		   (t (let ((path-nobar
			     (if (char= (uiop:last-char path) #\/)
				 (subseq path 0
					 (1- (length path)))
				 path)))
			(subseq path-nobar
				(1+ (or (search "/" path-nobar :from-end t) -1))
				(length path-nobar)))))))
    (cond ((uiop:directory-exists-p file-or-dir) ; is a directory
	   (list (module-from-path file-or-dir)
		 (directory (concatenate 'string
					 file-or-dir
					 "/*.vm"))))
	  ((uiop:file-exists-p file-or-dir)
	   (list (module-from-path file-or-dir) ; TODO: cleanup filename
		 (list (probe-file file-or-dir))))
	  (t (error "Cannot open file.")))))

(defun read-vm-file (file-path)
  "Returns a list containing only clean an non-comment commands in a file. Each
element corresponds to a line on the script."
  (cleanup-commands
   (handler-case
       (with-open-file (stream file-path
			       :direction :input)
	 (loop for line = (read-line stream nil)
	    while line
	    collect line))
     (error () nil))))


(defmacro if-let ((var value) conseq &optional (altern nil))
  `(let ((,var ,value))
     (if ,var ,conseq ,altern)))

(defun vm-translate (file-or-dir)
  "Global command for translating a VM file or directory."
  (if-let (path (truename file-or-dir))
    (let ((input (read-vm-input (uiop:unix-namestring path))))
      (write-asm-file (concatenate 'string
				   "./" (car input) ".asm")
		      (vm-parse-all-commands
		       (mapcar #'read-vm-file
			       (cadr input)))))))

