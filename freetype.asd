(in-package #:cl-user)

(asdf:defsystem freetype
  :version "0.0.0"
  :license "MIT"
  :author "Noel Benzinger <noelbenzinger@gmail.com>"
  :maintainer "Noel Benzinger <noelbenzinger@gmail.com>"
  :description "Common lisp wrapper around Freetype using autowrap"
  :serial t
  :depends-on
  (:alexandria
   :cl-autowrap
   :cl-plus-c)
  :components
  ((:module
    autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "freetype.h")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "freetype")))
