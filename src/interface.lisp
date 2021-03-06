;;;; interface.lisp
;;;; Part of cl-hackvmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package :cl-hackvmtr)

(defun get-module-from-path (path)
  "Takes a PATH and returns a module name. The module name may be equal to
either the directory name, or to the file name without extension. The module
name is used for naming the final ASM file, and for naming the static variables
for each file."
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
		     (length path-nobar))))))

(defun read-vm-input (file-or-dir)
  "Takes the path of a file or a directory given as input, and returns a list
containing two things. The first should be the name for the translated module,
deduced from file or directory name, and the second should be the list of files
which will be compiled."
  (cond ((uiop:directory-exists-p file-or-dir)
	 (list (get-module-from-path file-or-dir)
	       (mapcar (lambda (x)
			 (cons (get-module-from-path (uiop:unix-namestring x))
			       x))
		       (directory (concatenate 'string file-or-dir "/*.vm")))))
	((uiop:file-exists-p file-or-dir)
	 (cons (get-module-from-path file-or-dir)
	       (probe-file file-or-dir)))
	(t (error "Cannot open file."))))

(defun read-vm-file (file-spec)
  "Takes a file spec, containing the file name (without extension), consed into
the file path to the same file. Returns another spec, which is comprised of a
single list, where the first element is the same given file name, and the second
element is a list of VM commands to be translated."
  (list (car file-spec)
	(cleanup-commands
	 (handler-case
	     (with-open-file (stream (cdr file-spec) :direction :input)
	       (loop for line = (read-line stream nil)
		  while line collect line))
	   (error () nil)))))


(defmacro if-let ((var value) conseq)
  "Attempts to bind VALUE to VAR, then checks whether VAR is NIL. If not,
executes the command at CONSEQ."
  `(let ((,var ,value))
     (if (not (null ,var)) ,conseq nil)))


(defun vm-translate (file-or-dir &optional (no-bootstrap nil))
  "Global command for translating a VM file or directory. FILE-OR-DIR should
name a valid .vm file, or a directory containing .vm files. NO-BOOSTRAP
optionally informs whether the translation should contain the bootstrap code for
setting up the stack and calling 'Sys.init'."
  (if-let (path (truename file-or-dir))
    (let* ((input (read-vm-input (uiop:unix-namestring path)))
	   (output-name (car input)))
      (handler-case
	  (cond ((listp (cdr input)) ; List of files
		 (write-asm-file (concatenate 'string "./" output-name ".asm")
				 (vm-parse-all-commands
				  (mapcar #'read-vm-file (cadr input))
				  no-bootstrap)))
		(t
		 (write-asm-file (concatenate 'string "./" output-name ".asm")
				 (vm-parse-all-commands
				  (list (read-vm-file input))
				  no-bootstrap))))
	(error (err)
	  (format t "~a~%" err)
	  (format t "Error translating project. Bailing out.~%"))))))


