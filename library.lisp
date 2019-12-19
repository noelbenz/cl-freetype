(in-package #:freetype)

(cffi:define-foreign-library libfreetype
  (:darwin (:or "libfreetype.dylib" "libfreetype"))
  (:unix (:or "libfreetype.so" "libfreetype"))
  (:windows (:or "libfreetype.dll" "libfreetype"))
  (t "libfreetype"))

(cffi:use-foreign-library libfreetype)

