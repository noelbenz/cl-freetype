(in-package #:freetype)

(defvar *library*)
(export '*library*)

(define-condition freetype-error (error)
  ((function-name :initarg :function-name :reader function-name)
   (error-code :initarg :error-code :reader error-code)
   (error-message :initarg :error-message :reader error-message)))
(export 'freetype-error)
(export 'function-name)
(export 'error-code)
(export 'error-message)

(defun string-starts-with (lhs rhs)
  (let ((position (search rhs lhs)))
    (and position (= position 0))))

(defun error-name (error-code)
  (loop for sym being the external-symbols of 'freetype-ffi do
       (when (and (string-starts-with (symbol-name sym) "+FT-ERR-")
                  (boundp sym)
                  (symbol-value sym)
                  (= (symbol-value sym) error-code))
         (return (subseq (symbol-name sym) 8 (- (length (symbol-name sym)) 1))))))
(export 'error-name)

(defmacro handle-error-c-fun (fn &rest args)
  (with-gensyms (error-code)
    `(let ((,error-code (c-fun ,fn ,@args)))
       (when (/= ,error-code 0)
         (error 'freetype-error
                :function-name ,(symbol-name fn)
                :error-code ,error-code
                :error-message (error-name ,error-code))))))

(defmacro def-record-method (wrapper-class method-name &key (no-export nil)
                             (result-wrapper-class nil) fields)
  "Generates a generic method named `method-name' that reads the record of
`wrapper-class' described by `fields'"
  (with-gensyms (wrapper result)
    `(progn
      (defgeneric ,method-name (object))
      (defmethod ,method-name ((,wrapper ,wrapper-class))
        (let ((,result (c-ref ,wrapper ,wrapper-class ,@fields)))
          ,(if result-wrapper-class
              `(autowrap:wrap-pointer (ptr ,result) (quote ,result-wrapper-class))
              result)))
      ,@(unless no-export `((export (quote ,method-name)))))))

(defmacro def-record-methods (wrapper-class record-descriptors)
  "Generates a generic method for each descriptor in `record-descriptors' which
reads a record from `wrapper-class'.

Each record descriptor has the following form:
 (method-name :no-export t :result-wrapper-class freetype-ffi:ft-face-rec :fields (:some-field))"
  `(progn
     ,@(loop for descriptor in record-descriptors
            collect `(def-record-method ,wrapper-class ,@descriptor))))

(defun from-26-6 (fixed-26-6)
  (/ fixed-26-6 64))
(export 'from-26-6)

(defun to-26-6 (decimal)
  (round (* decimal 64)))
(export 'to-26-6)

(defun make-library ()
  (cffi:with-foreign-objects ((library-ptr :pointer))
    (handle-error-c-fun freetype-ffi:ft-init-free-type library-ptr)
    (autowrap:wrap-pointer (cffi:mem-ref library-ptr :pointer) 'freetype-ffi:ft-library)))
(export 'make-library)

(defun destroy-library (library)
  (handle-error-c-fun freetype-ffi:ft-done-free-type library))
(export 'destroy-library)

(defmacro with-init (&body body)
  `(let ((*library* (make-library)))
     (unwind-protect (progn ,@body)
       (destroy-library *library*))))
(export 'with-init)

(defun get-library ()
  *library*)
(export 'get-library)

(defun make-face (path &optional (face-index 0))
  (cffi:with-foreign-objects ((face-ptr :pointer))
    (handle-error-c-fun freetype-ffi:ft-new-face *library* path face-index face-ptr)
    (autowrap:wrap-pointer (cffi:mem-ref face-ptr :pointer) 'freetype-ffi:ft-face-rec)))
(export 'make-face)

(defun destroy-face (face)
  (handle-error-c-fun freetype-ffi:ft-done-face face))
(export 'destroy-face)

(def-record-methods freetype-ffi:ft-face-rec
    ((num-faces :fields (:num-faces))
     (face-index :fields (:face-index))
     (face-flags :fields (:face-flags))
     (style-flags :fields (:style-flags))
     (num-glyphs :fields (:num-glyphs))
     (family-name :fields (:family-name))
     (style-name :fields (:style-name))
     (num-fixed-sizes :fields (:num-fixed-sizes))
     (available-sizes :fields (:available-sizes))
     (num-charmaps :fields (:num-charmaps))
     (charmaps :fields (:charmaps))
     (bbox :fields (:bbox))
     (units-per-em :fields (:units-per-em))
     (ascender :fields (:ascender))
     (descender :fields (:descender))
     (height :fields (:height))
     (max-advance-width :fields (:max-advance-width))
     (max-advance-height :fields (:max-advance-height))
     (underline-position :fields (:underline-position))
     (underline-thickness :fields (:underline-thickness))
     (glyph :fields (:glyph))
     (size-metrics :fields (:size :metrics) :result-wrapper-class freetype-ffi:ft-size-metrics)
     (charmap :fields (:charmap))))

(defgeneric set-char-size (face &key char-width char-height
                           horizontal-resolution vertical-resolution))
(export 'set-char-size)

(defmethod set-char-size ((face freetype-ffi:ft-face-rec) &key (char-width 0) (char-height 0)
                          (horizontal-resolution 0) (vertical-resolution 0))
  (handle-error-c-fun freetype-ffi:ft-set-char-size face char-width char-height
                      horizontal-resolution vertical-resolution)
  face)

(defgeneric set-pixel-size (face &key pixel-width pixel-height ))
(export 'set-pixel-size)

(defmethod set-pixel-size ((face freetype-ffi:ft-face-rec) &key (pixel-width 0) (pixel-height 0))
  (handle-error-c-fun freetype-ffi:ft-set-pixel-sizes face pixel-width pixel-height)
  face)

(def-record-methods freetype-ffi:ft-size-metrics
    ((x-ppem :fields (:x-ppem))
     (y-ppem :fields (:y-ppem))
     (y-scale :fields (:y-scale))
     (x-scale :fields (:x-scale))
     (ascender :fields (:ascender))
     (descender :fields (:descender))
     (height :fields (:height))
     (max-advance :fields (:max-advance))))

#+nil
(freetype:with-init
  (format t "Library loaded!~%")
  (let ((face (make-face "projects/freetype/BebasNeueBold.ttf")))
    (set-char-size face :char-width (to-26-6 12) :horizontal-resolution 8 :vertical-resolution 12)
    (format t "Face: ~A~%" face)
    (format t "Number of faces: ~A~%" (num-faces face))
    (format t "Ascender: ~A~%" (ascender face))
    (format t "X Scale: ~A~%" (x-scale (size-metrics face)))
    (destroy-face face)))

