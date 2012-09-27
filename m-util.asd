;;;; m-util.asd

(asdf:defsystem #:m-util
  :serial t
  :description "All sorts of utility stuff. This is where Mikko Juola ~
  collects some very generic code pieces he likes to use in his code."
  :author "Mikko Juola <mikjuo@gmail.com>"
  :license "ISC License"
  :components ((:file "package")
               (:file "m-util")
               (:file "anaphora")))


