(in-package :cl-hackwmtr)

(defparameter *stack-test*
  (read-vm-file #P"/home/alchemist/.local/share/nand2tetris/projects/07/StackArithmetic/StackTest/StackTest.vm"))

(defparameter *simple-add*
  (read-vm-file #P"/home/alchemist/.local/share/nand2tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm"))
