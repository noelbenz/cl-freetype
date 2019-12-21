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
    (autowrap:wrap-pointer (cffi:mem-ref face-ptr :pointer) 'freetype-ffi:ft-face)))
(export 'make-face)

(defun destroy-face (face)
  (handle-error-c-fun freetype-ffi:ft-done-face face))
(export 'destroy-face)

#+nil
(freetype:with-init (format t "Library loaded! Face: ~A~%" (destroy-face (make-face "projects/freetype/BebasNeueBold.ttf"))))

