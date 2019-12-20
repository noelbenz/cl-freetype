(in-package #:freetype)

(cffi:define-foreign-library libfreetype
  (:darwin (:or "libfreetype.dylib" "libfreetype"))
  (:unix (:or "libfreetype.so" "libfreetype"))
  (:windows (:or "freetype.dll" "freetype"))
  (t (:or "freetype" "libfreetype")))

(cffi:use-foreign-library libfreetype)

