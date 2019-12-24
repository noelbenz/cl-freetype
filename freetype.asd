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
    ((:static-file "freetype.h")
     (:static-file "freetype.arm-pc-linux-gnu.spec")
     (:static-file "freetype.i386-unknown-freebsd.spec")
     (:static-file "freetype.i386-unknown-openbsd.spec")
     (:static-file "freetype.i686-apple-darwin9.spec")
     (:static-file "freetype.i686-pc-linux-gnu.spec")
     (:static-file "freetype.i686-pc-windows-msvc.spec")
     (:static-file "freetype.x86_64-apple-darwin9.spec")
     (:static-file "freetype.x86_64-pc-linux-gnu.spec")
     (:static-file "freetype.x86_64-pc-windows-msvc.spec")
     (:static-file "freetype.x86_64-unknown-freebsd.spec")
     (:static-file "freetype.x86_64-unknown-openbsd.spec")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "freetype")))
