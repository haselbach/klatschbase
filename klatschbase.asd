(in-package #:asdf)

(defsystem klatschbase
  :description "Chat Web Application"
  :version "0.2"
  :author "Christian Haselbach"
  :license "MIT"
  :components ((:file "package")
               (:file "rest-my-case" :depends-on ("package"))
               (:file "chat-core"    :depends-on ("package"))
               (:file "chat"         :depends-on ("chat-core" "rest-my-case")))
  :depends-on (:hunchentoot :cl-json :cl-ppcre :parenscript))
