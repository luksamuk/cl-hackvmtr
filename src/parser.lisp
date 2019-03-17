;;;; parser.lisp
;;;; Part of cl-hackvmtr.
;;;; Copyright (c) 2019 Lucas Vieira
;;;; This project is distributed under the MIT License.

(in-package :cl-hackvmtr)

#|
                             ***  TODO LIST  ***

DONE - New commands for Program Control Flow:
- label LABEL   => Names a label at point
- goto LABEL    => Unconditionally jump to a label (JMP-style)
- if-goto LABEL => Jump to label iff popped value is not zero (JNE-style)

New commands for Function Calling:
- function F N => Starts the code of a function F with N local variables
- call F M     => Calls function F, and states that M arguments were pushed to
                  the stack
- return       => Return the called function

Functions to be implemented:
- Sys.init     => Argument-less function, responsible for calling the entry
                  point (and maybe setting up memory segments?)

NOTES:
- Need to see if I need to handle control flow and function commands as separate
  types of commands; I think I probably should, to keep the program clean.
- Commands for control flow are simple enough, and easy to implement. Should be
  the first step, and may be executed quickly.
- Function calling commands are trickier, but intuition dictates that they might
  be better expressed as meta-commands. For example, a "call F M" command
  involves pushing a function's frame to the stack; pushing anything is still
  technically at VM level, so maybe we should replace it by equivalent labels
  and such.
- I think I could add tail call optimization when implementing the call command.
  For that, I'll revisit the explicit-control evaluator code on SICP's video
  lectures, to figure out a simple way to do that.

|#



;;; The following commented code can be ignored, and will potentially be removed
;;; soon.

;;; Ram address usage:
;; 0~15        => Virtual special registers
;; 16~255      => Static variables (VM functions)
;; 256~2047    => Stack
;; 2048~16383  => Heap
;; 16384~24575 => I/O map
;; 24575~32767 => Unused
;; (defparameter *ram-table-base*
;;   '((special . ("0"       "15"))
;;     (static  . ("16"     "255"))
;;     (stack   . ("256"   "2047"))
;;     (heap    . ("2048"  "16383"))
;;     (io      . ("16384" "24575"))
;;     (unused  . ("24576" "32767")))
;;   "Base addresses for RAM segments.")
;;
;;
;; (defun ram-base-address (segment &key (position 'both))
;;   "Returns the base address for a specific RAM segment.
;; Passing key argument POSITION returns only a specific position of the segment,
;; which could be START, END or BOTH, the latter being the default option."
;;   (let ((segment-addrs (cdr (assoc segment *ram-table-base*))))
;;     (when segment-addrs
;;       (or (and (eq position 'start)
;; 	       (car segment-addrs))
;; 	  (and (eq position 'end)
;; 	       (cadr segment-addrs))
;; 	  (and (eq position 'both)
;; 	       segment-addrs)))))
;;
;; (defun asm-static-ainstr (module-name static-num)
;;   (format nil "~a.~d" module-name static-num))
;;
;; (defun intern-or-parse-value (x)
;;   (handler-bind ((error (lambda (e)
;; 			  (declare (ignore e))
;; 			  (invoke-restart 'parse-as-symbol))))
;;     (restart-case (progn (parse-integer x))
;;       (parse-as-symbol ()
;; 	(intern (string-upcase x))))))



;;; =========================== Global Parameters ========================== ;;;

(defparameter *segments-table-base*
  '(("local"    . "LCL")
    ("argument" . "ARG")
    ("this"     . "THIS")
    ("that"     . "THAT")
    ("pointer"  . "3") ; 0=THIS, 1=THAT. Change this to change THIS & THAT
    ("temp"     . "5") ; Valid: temp 0~7 inclusive (R5~R12). NOT POINTERS!
    ("constant")       ; No base. A constant supplies the following i
    ("static"   . "16")) ; Static variables
  "Base addresses for memory segments. These segments differ from RAM segments
with respect to the actual idioms used in the assembly code.")


(defparameter *arith-tst-flag* 0
  "Internal value for arithmetical test flags.
Increases at each test performed, so that generating a new testing assembly does
not conflict with others.")

(defparameter *arithmetic-operations*
  (list "add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not")
  "Lists the strings corresponding to possible arithmetic, bitwise or
comparision operations for the VM.")

(defparameter *stack-operations*
  (list "push" "pop")
  "Lists the strings corresponding to possible stack operations for the VM.")

(defparameter *flow-operations*
  (list "label" "goto" "if-goto")
  "Lists the strings corresponding to possible control flow operations for the
VM.")

;;; ============================== Utilities =============================== ;;;


(defun segment-base-address (segment)
  "Fetches the address of SEGMENT, for segment base address lookup. This does
not perform the necessary translation in assembly."
  (cdr (assoc segment *segments-table-base* :test #'equal)))


(defun cleanup-commands (command-list)
  "Takes a list of unclean VM commands, then trims and removes comments from
each one of them. Empty or comment-only lines are ignored."
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
  "Takes a list of lists and flattens them into a depth-zero list."
  (loop for element in list-of-lists
     if (not (listp element))
     collect element
     else append (flatten element)))

(defmacro case-string (keyform &body cases)
  "Macro for declaring a special type of case block, in which each test
corresponds to a string comparision."
  (cons 'cond
	(loop for case in cases
	   collect `((string= ,keyform ,(car case))
		     ,@(cdr case)))))


;;; --------------------- ASM code generation tools ------------------------ ;;;

(defmacro hack-inline (&rest commands)
  "Macro for building a list of inline Hack assembly commands. By using this,
one can mix Hack assembly with other lists of Hack assembly code, usually
generated by specific operations."
  `(flatten (list ,@commands)))


(defun hack-flag (flag-name &optional (index nil))
  "Produces a multi-purpose label in the form 'FLAG-NAME.INDEX'."
  (let ((name (string-upcase flag-name)))
    (if index
	(format nil "~a.~d" name index)
	name)))

(defun hack-enclose (flag)
  "Encloses a label in parenthesis. Such a label indicates the start of a
certain region on Hack assembly, which can be jumped to."
  (format nil "(~a)" flag))

(defun hack-ref (flag)
  "Prefixes a label with @. Such a label indicates that the address of the first
command under the label should be stored in the A register."
  (format nil "@~a" flag))


;;; ====================== Stack Commands Translation ====================== ;;;


(defun fetch-segment-baseaddr (segment i)
  "Fetches a segment's base address.
Address is stored in A, and there is no guarantee that D will be intact."
  (let* ((inum (format nil "@~d" i))
	 (baseaddr (segment-base-address segment))
	 (segm (format nil "@~a" baseaddr)))
    ;; If base address is not registered and it is not a 'constant' segment,
    ;; we have an error.
    (when (and (null baseaddr)
	       (not (string= segment "constant")))
      (error "Invalid segment: ~a" segment))
    ;;; Dispatch the segment address fetch in a per-segment basis.
    (cond ((string= segment "constant") ; 'constant' just puts a number in A
	   (list inum))         ; Load i in A
	  ;; 'temp', 'static' & 'pointer' are special in the sense that we
	  ;; use them as data segments. 'pointer' is used to manipulate
	  ;; the current pointed regions of 'this' and 'that', so they're
	  ;; pretty much handled as raw information.
	  ((or (string= segment "temp")
	       (string= segment "pointer")
	       (string= segment "static"))
	   (if (string= i "0")  ;; Check if calculation is needed
	       (list segm)      ; If not, load @5 into A
	       ;; If needed, calculate absolute pointer into A
	       (list segm       ; Load @5 into A
		     "D=A"      ; Store A into D
		     inum       ; Load i into A
		     "A=D+A"))) ; Store addr + A in A
	  ;; Segment with address = 0 is the segment itself
	  ((string= i "0")
	   (list segm           ; Load base address pointer in A
		 "A=M"))        ; Load base address in A
	  ;; Segment with address > 0 needs its abs value calculated.
	  ;; Destroys value in D.
	  (t (list segm         ; Load base address pointer in A
		   "D=M"        ; Load base address in D
		   inum         ; Load i in A
		   "A=D+A"))))) ; Store baseaddr + A in A

(defun push-from-dreg ()
  "Takes the value stored in D register, then pushes it to the top of stack."
  (hack-inline "@SP"     ; put stack top pointer addr in A
	       "A=M"     ; put stack top addr in A
	       "M=D"     ; assign data in D to top of stack
	       "@SP"     ; reload stack top pointer
	       "M=M+1")) ; increment stack top pointer

(defun push-segment (segment i)
  "Pushes a SEGMENT at relative address I to the top of stack."
  (hack-inline (fetch-segment-baseaddr segment i) ; address is in A
	       (if (string= segment "constant")   ;; constant value check
		   "D=A"                          ; if constant, D gets A
		   "D=M")                         ; if not, D gets M[A]
	       (push-from-dreg))) ; increment stack top pointer

(defun pop-into-dreg ()
  "Pops the topmost value on the stack into the D register."
  (hack-inline "@SP"   ; put stack top pointer addr in A
	       "M=M-1" ; decrement stack top pointer addr
	       "A=M"   ; put stack top addr in A
	       "D=M")) ; put M[A] in D

(defun pop-segment (segment i)
  "Pops the topmost value on the stack into SEGMENT at relative address I."
  ;; Popping into constant is a very senseless thing to try
  (when (string= segment "constant")
    (error "Cannot pop to a constant value"))
  (hack-inline (pop-into-dreg) ; pop data into D register
	       "@R13"
	       "M=D"   ; put data stored in D in temporary location
	       (fetch-segment-baseaddr segment i) ; dest addr is in A
	       "D=A"   ; store A in D
	       "@R14"
	       "M=D"   ; save address where data will be stored in R14
	       "@R13"
	       "D=M"   ; recover R13 data into D
	       "@R14"
	       "A=M"   ; recover address in R14 into A
	       "M=D")) ; save data stored in D into M[A]

(defun fetch-operands-from-stack ()
  "Fetches two operands (x, y) from stack.
In the end of operation, it is ensured that the value for X is in D, and
the value for Y is in A."
  (hack-inline (pop-into-dreg) ; Fetch Y into D
	       "@R13"
	       "M=D" ; save data in temporary R13 location
	       (pop-into-dreg) ; Fetch X into D
	       "@R13"
	       "A=M")) ; restore saved data in R13


;;; ==================== Stack Operations Translation ====================== ;;;


(defun hack-append-falsity-test (command test)
  "Takes a COMMAND and appends to it a jump postfix, based on the TEST which
needs to be performed. TEST might be one of the valid comparision operations for
the VM: 'eq', 'lt' or 'gt'."
  (format nil "~a;~a"
	  command
	  (case test
	    (eq "JNE")
	    (lt "JGE")
	    (gt "JLE"))))

(defun hack-compare-test (test-type)
  "Generates Hack assembly commands for comparing two numbers on top of stack.
The operands are expected to be pushed in a way so they can only be retrieved by
reverse order (e.g. X > Y expects Y to be on top of X in the stack). TEST-TYPE
indicates the type of comparision operation which will be written."
  (let ((commlist
	 (hack-inline (fetch-operands-from-stack)
		      "D=D-A" ; Subtract operands for comparision
		      ;; Prepare to jump to a FALSE.x flag on test
		      (hack-ref (hack-flag "FALSE" *arith-tst-flag*))
		      ;; Take D and append a falsity test depending
		      ;; on the test type. If false, jump to FALSE.x
		      (hack-append-falsity-test "D" test-type)
		      "D=-1"  ; Put 'true' value on D
		      ;; Jump to ENDTEST.x
		      (hack-ref (hack-flag "ENDTEST" *arith-tst-flag*))
		      "0;JMP" ; Unconditional jump to ENDTEST.x
		      ;; Code segment label for false comparision
		      (hack-enclose (hack-flag "FALSE" *arith-tst-flag*))
		      "D=0"  ; Put 'false' value on D
		      ;; Code segment label for end of test
		      (hack-enclose (hack-flag "ENDTEST" *arith-tst-flag*))
		      ;; Push result to stack
		      (push-from-dreg))))
    ;; Increment arithmetic test flag
    (incf *arith-tst-flag*)
    ;; Return list of commands
    commlist))
		

(defun perform-operation (operation)
  "Takes a specific arithmetic or bitwise OPERATION as string argument, and
produces Hack assembly code for that operation."
  (case-string operation
    ("add"   (hack-inline (fetch-operands-from-stack)
			  "D=D+A" ; Add operands, put in D
			  (push-from-dreg)))
    ("sub"   (hack-inline (fetch-operands-from-stack)
			  "D=D-A" ; Subtract operands, put in D
			  (push-from-dreg)))
    ("neg"   (hack-inline (pop-into-dreg)
			  "D=-D"  ; Put numeric opposite of single operand in D
			  (push-from-dreg)))
    ("eq"    (hack-compare-test 'eq)) ; Dispatch = test
    ("gt"    (hack-compare-test 'gt)) ; Dispatch > test
    ("lt"    (hack-compare-test 'lt)) ; Dispatch < test
    ("and"   (hack-inline (fetch-operands-from-stack)
			  "D=D&A" ; Bitwise AND, put in D
			  (push-from-dreg)))
    ("or"    (hack-inline (fetch-operands-from-stack)
			  "D=D|A" ; Bitwise OR, put in D
			  (push-from-dreg)))
    ("not"   (hack-inline (pop-into-dreg)
			  "D=!D"  ; Put negation of single operand in D
			  (push-from-dreg)))))



;;; ================ Control Flow Operations Translation =================== ;;;

(defun perform-control-flow (operation operand)
  "Takes a control flow string OPERATION, along with its string OPERAND, and
produces Hack assembly code for that operation."
  (case-string operation
    ("label"   (hack-inline (hack-enclose operand)))
    ("goto"    (hack-inline (hack-ref operand)
			    "0;JMP"))
    ("if-goto" (hack-inline (pop-into-dreg)
			    (hack-ref operand)
			    "D;JNE"))))



;;; =================== General VM Command Translation ===================== ;;;

(defmacro command-of-p (command command-list)
  "Checks whether a certain COMMAND belongs to a COMMAND-LIST."
  `(member ,command ,command-list :test #'equal))

(defmacro check-command-length (split-command ideal-arity)
  "Checks whether a command SPLIT-COMMAND, split into a list of strings, has a
certain IDEAL-ARITY length."
  `(unless (= (length ,split-command) ,ideal-arity)
     (error "Command ~a has too ~a arguments.~%In: ~a"
	    (car ,split-command)
	    (if (< (length ,split-command) ,ideal-arity) "few" "many")
	    ,split-command)))

(defun vm-dispatch-command (command-string)
  "Takes a COMMAND-STRING and dispatches it to translation subroutines,
returning a list of Hack assembly commands."
  (let ((commands (split-sequence #\Space command-string)))
    ;;       Arithmetic operations
    (cond ((command-of-p (car commands) *arithmetic-operations*)
	   (check-command-length commands 1)
	   (perform-operation (car commands)))
	  ;; Stack operations
	  ((command-of-p (car commands) *stack-operations*)
	   (check-command-length commands 3)
	   (apply (if (string= (car commands) "push")
		      #'push-segment
		      #'pop-segment)
		  (cdr commands)))
	  ;; Control flow operations
	  ((command-of-p (car commands) *flow-operations*)
	   (check-command-length commands 2)
	   (apply #'perform-control-flow commands))
	  ;; Error or unexisting command
	  (t (error "Unknown command: ~a" (car commands))))))


(defun initialize-segment (segment initial-value)
  "Generates code which predefines the value of SEGMENT to INITIAL-VALUE."
  (hack-inline (format nil "@~d" initial-value)
	       "D=A"
	       (format nil "@~a" segment)
	       "M=D"))

(defun vm-initialization ()
  "Initializes the VM by setting up the proper values to certain segments,
so that the machine does not execute arbitrary code."
  (list (initialize-segment "SP"    256)
	;; Other base addresses. These are potential placeholders
	(initialize-segment "LCL"   300)
	(initialize-segment "ARG"   400)
	(initialize-segment "THIS" 3000)
	(initialize-segment "THAT" 3010)))

(defun vm-halt ()
  "Produces an infinite loop to be appended to the complete Hack assembly code,
after translation, so that the machine does not execute arbitrary code."
  (list "(-INTERNAL.HACKVM.HALT)"
	"@-INTERNAL.HACKVM.HALT"
	"0;JMP"))
