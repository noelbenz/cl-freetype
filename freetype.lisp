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

(defmacro def-record-method (wrapper-class method-name &rest record-fields)
  "Generates a generic method named `method-name' that reads the record of `wrapper-class' described by `record-fields'"
  (with-gensyms (wrapper)
    `(progn
      (defgeneric ,method-name (object))
      (defmethod ,method-name ((,wrapper ,wrapper-class))
        (c-ref ,wrapper ,wrapper-class ,@record-fields)))))

(defmacro def-record-methods (wrapper-class record-descriptors)
  "Generates a generic method for each descriptor in `record-descriptors' which reads a record from `wrapper-class'.

Each record descriptor is a list whose first element is the generic method's name and the rest of the elements are the record fields"
  `(progn
     ,@(loop for descriptor in record-descriptors
            collect `(def-record-method ,wrapper-class ,@descriptor))))

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
    ((num-faces :num-faces)
     (face-index :face-index)
     (face-flags :face-flags)
     (style-flags :style-flags)
     (num-glyphs :num-glyphs)
     (family-name :family-name)
     (style-name :style-name)
     (num-fixed-sizes :num-fixed-sizes)
     (available-sizes :available-sizes)
     (num-charmaps :num-charmaps)
     (charmaps :charmaps)
     (bbox :bbox)
     (units-per-em :units-per-em)
     (ascender :ascender)
     (descender :descender)
     (height :height)
     (max-advance-width :max-advance-width)
     (max-advance-height :max-advance-height)
     (underline-position :underline-position)
     (underline-thickness :underline-thickness)
     (glyph :glyph)
     (size :size)
     (charmap :charmap)))

#+nil
(freetype:with-init
  (format t "Library loaded!~%")
  (let ((face (make-face "projects/freetype/BebasNeueBold.ttf")))
    (format t "Face: ~A~%" face)
    (format t "Number of faces: ~A~%" (num-faces face))
    (format t "Ascender: ~A~%" (ascender face))
    (destroy-face face)))

