;;;; cl-hackwmtr.asd

(asdf:defsystem #:cl-hackvmtr
  :description "VM Translator for the nand2tetris Hack platform"
  :author "Lucas Vieira <lucasvieira@protonmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:module "src" :components ((:file "parser")
					   (:file "writer")
					   (:file "interface")))))

