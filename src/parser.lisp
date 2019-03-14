;;;; parser.lisp
;;;; Part of cl-hackwmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package :cl-hackwmtr)


;;; Ram address usage:
;; 0~15        => Virtual special registers
;; 16~255      => Static variables (VM functions)
;; 256~2047    => Stack
;; 2048~16383  => Heap
;; 16384~24575 => I/O map
;; 24575~32767 => Unused
(defparameter *ram-table-base*
  '((special . ("0"       "15"))
    (static  . ("16"     "255"))
    (stack   . ("256"   "2047"))
    (heap    . ("2048"  "16383"))
    (io      . ("16384" "24575"))
    (unused  . ("24576" "32767")))
  "Base addresses for RAM segments.")


(defun ram-base-address (segment &key (position 'both))
  "Returns the base address for a specific RAM segment.
Passing key argument POSITION returns only a specific position of the segment,
which could be START, END or BOTH, the latter being the default option."
  (let ((segment-addrs (cdr (assoc segment *ram-table-base*))))
    (when segment-addrs
      (or (and (eq position 'start)
	       (car segment-addrs))
	  (and (eq position 'end)
	       (cadr segment-addrs))
	  (and (eq position 'both)
	       segment-addrs)))))

;;; TODO:
;; - function
;; - call
;; - label
(defparameter *segments-table-base*
  '((local    . "LCL")
    (argument . "ARG")
    (this     . "THIS")
    (that     . "THAT")
    (pointer  . "3") ; Valid: pointer 0 (this), pointer 1 (that)
    (temp     . "5") ; Valid: temp 0 through 7 inclusive (R5~R12)
    (constant)  ; No base. A constant supplies the following i
    (static   . "16")) ; Static variables
  "Base addresses for memory segments. These segments differ from RAM segments
wrt. the actual idioms used in the assembly code.")

(defun segment-base-address (segment)
  (cdr (assoc segment *segments-table-base*)))

(defun asm-static-ainstr (module-name static-num)
  (format nil "~a.~d" module-name static-num))


(defun cleanup-commands (command-list)
  (labels ((clean-comments (string)
	     (let ((comment-start (search "//" string)))
	       (string-trim '(#\Space #\Newline #\Tab #\Linefeed #\Return)
			    (if comment-start
				(subseq string 0 comment-start)
				string)))))
    (loop for command in command-list
       for clean-command = (clean-comments command)
       unless (string-equal clean-command "")
       collect clean-command)))



(defun flatten (list-of-lists)
  (loop for element in list-of-lists
     if (not (listp element))
     collect element
     else append (flatten element)))


(defun fetch-segment-baseaddr (segment i) ; todo: special segments
  (let* ((inum (format nil "@~d" i))
	 (baseaddr (segment-base-address segment))
	 (segm (format nil "@~a" baseaddr)))
    (when (and (not baseaddr)
	       (not (eq segment 'constant)))
      (error "Invalid segment: ~a" segment))
    (if (eq segment 'constant)
	(list inum
	      "D=A") ; data from A is automatically put in D
	(cons segm
	      (if (not (zerop i))
		  (list "D=A"
			inum
			"A=D+A"
			"D=M")
		  (list "D=M")))))) ; data from M[A] is put in D


(defmacro hack-inline (&rest commands)
  `(flatten (list ,@commands)))

(defun push-from-dreg ()
  (hack-inline "@SP"     ; put stack top pointer addr in A
	       "A=M"     ; put stack top addr in A
	       "M=D"     ; assign data in D to top of stack
	       "@SP"     ; reload stack top pointer
	       "M=M+1")) ; increment stack top pointer

(defun push-segment (segment i)
  (hack-inline (fetch-segment-baseaddr segment i) ; data is in D
	       (push-from-dreg))) ; increment stack top pointer

(defun pop-into-dreg ()
  (hack-inline "@SP"   ; put stack top pointer addr in A
	       "M=M-1" ; decrement stack top pointer addr
	       "A=M"   ; put stack top addr in A
	       "D=M")) ; put M[A] in D

(defun pop-segment (segment i)
  (when (eq segment 'constant)
    (error "Cannot pop to a constant value"))
  (hack-inline (pop-into-dreg) ; pop data into D register
	       "@R13"
	       "M=D"   ; put data stored in D in temporary location
	       (fetch-segment-baseaddr segment i) ; dest addr is in A
	       "D=A"   ; store A in D
	       "@R13"
	       "A=M"   ; put contents of R13 in A
	       ;; In A, we have the dest addr. In D, we have popped data
	       "M=D"))   ; make final assignment

(defun fetch-operands-from-stack ()
  "Fetches operands from stack so that X is in D and Y is in A"
  (hack-inline (pop-into-dreg) ; Fetch Y into D
	       "@R13"
	       "M=D" ; save data in temporary R13 location
	       (pop-into-dreg) ; Fetch X into D
	       "@R13"
	       "A=M")) ; restore saved data in R13

(defparameter *arith-tst-flag* 0
  "Internal value for arithmetical test flags.
Increases at each test performed, so that generating a new testing assembly does
not conflict with others.")

(defun hack-flag (flag-name &optional (index nil))
  (let ((name (string-upcase flag-name)))
    (if index
	(format nil "~a.~d" name index)
	name)))

(defun hack-enclose (flag)
  (format nil "(~a)" flag))

(defun hack-ref (flag)
  (format nil "@~a" flag))

(defun hack-append-falsity-test (command test)
  (format nil "~a;~a"
	  command
	  (case test
	    (eq "JNE")
	    (lt "JGE")
	    (gt "JLE"))))

(defun hack-compare-test (test-type)
  (let ((commlist
	 (hack-inline (fetch-operands-from-stack)
		      "D=D-A"
		      ;; Prepare to jump to a FALSE.x flag on test
		      (hack-ref (hack-flag "FALSE" *arith-tst-flag*))
		      ;; Take D and append a falsity test depending
		      ;; on the test type. If false, jump to FALSE.x
		      (hack-append-falsity-test "D" test-type)
		      "D=-1" ; -1 corresponds to TRUE
		      ;; Jump to ENDTEST.x
		      (hack-ref (hack-flag "ENDTEST" *arith-tst-flag*))
		      "0;JMP"
		      ;; Code segment for false comparision
		      (hack-enclose (hack-flag "FALSE" *arith-tst-flag*))
		      "D=0"  ; 0 corresponds to FALSE
		      ;; Code segment for end of test
		      (hack-enclose (hack-flag "ENDTEST" *arith-tst-flag*))
		      ;; Push result to stack
		      (push-from-dreg))))
    ;; Increment arithmetic test flag
    (incf *arith-tst-flag*)
    ;; Return list of commands
    commlist))


	       

(defun perform-operation (operation)
  (case operation
    (add   (hack-inline (fetch-operands-from-stack)
			"D=D+A"
			(push-from-dreg)))
    (sub   (hack-inline (fetch-operands-from-stack)
			"D=D-A"
			(push-from-dreg)))
    (neg   (hack-inline (pop-into-dreg)
			"D=-D"
			(push-from-dreg)))
    (eq    (hack-compare-test 'eq))
    (gt    (hack-compare-test 'gt))
    (lt    (hack-compare-test 'lt))
    (and   (hack-inline (fetch-operands-from-stack)
			"D=D&A"
			(push-from-dreg)))
    (or    (hack-inline (fetch-operands-from-stack)
			"D=D|A"
			(push-from-dreg)))
    (not   (hack-inline (pop-into-dreg)
			"D=!D"
			(push-from-dreg)))))


(defparameter *arithmetic-operations*
  '(add sub neg eq gt lt and or not))

(defparameter *stack-operations*
  '(push pop))

(defun intern-or-parse-value (x)
  (handler-bind ((error (lambda (e)
			  (declare (ignore e))
			  (invoke-restart 'parse-as-symbol))))
    (restart-case (progn (parse-integer x))
      (parse-as-symbol ()
	(intern (string-upcase x))))))

(defun vm-dispatch-command (command-string)
  (let ((commands (mapcar #'intern-or-parse-value
			  (split-sequence #\Space command-string))))
    (cond ((member (car commands) *arithmetic-operations*)
	   (unless (= (length commands) 1)
	     (error "Command ~a has too many arguments.~%In: ~a"
		    (car commands)
		    commands))
	   (perform-operation (car commands)))
	  ((member (car commands) *stack-operations*)
	   (unless (= (length commands) 3)
	     (error "Command ~a has too ~a arguments.~%In: ~a"
		    (car commands)
		    (if (< (length commands) 3) "few" "many")
		    commands))
	   (apply (if (eq (car commands) 'push)
		      #'push-segment
		      #'pop-segment)
		  (cdr commands)))
	  (t (error "Unknown command: ~a" commands)))))

(defun vm-initialization ()
  "Initialize virtual machine by setting up special pointers."
  (list "@256" ; set SP to base stack address
	"D=A"
	"@SP"
	"M=D"))

(defun vm-halt ()
  "Halt virtual machine by using an infinite loop."
  (list "(-INTERNAL.HACKVM.HALT)"
	"@-INTERNAL.HACKVM.HALT"
	"0;JMP"))
