;;;; telephone.asd

(asdf:defsystem #:telephone
  :description "Describe telephone here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:hunchentoot
               #:drakma
               #:closure-html
               #:alexandria
               #:quri)
  :serial t
  :components ((:file "package")
               (:file "telephone")))
