;;;; cl-hackwmtr.asd

(asdf:defsystem #:cl-hackwmtr
  :description "Describe cl-hackwmtr here"
  :author "Lucas Vieira <lucasvieira@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:module "src" :components ((:file "parser")
					   (:file "writer")
					   (:file "interface")))))

