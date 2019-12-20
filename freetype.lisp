(in-package :freetype)

(defvar *library*)
(export '*library*)

(defun make-library ()
  (cffi:with-foreign-objects ((library-ptr-ptr :pointer) (library-ptr :pointer))
    (setf (cffi:mem-aref library-ptr-ptr :pointer) library-ptr)
    (let* ((error-code (c-fun freetype-ffi:ft-init-free-type library-ptr-ptr))
           (library-cffi-ptr (cffi:make-pointer (cffi:pointer-address library-ptr))))
      (if (/= error-code 0)
          ;; TODO: Generic error handler that throws a condition.
          (format t "There was an error making the library!~%")
          (autowrap:wrap-pointer library-cffi-ptr 'freetype-ffi:ft-library)))))
(export 'make-library)

(defun destroy-library (library)
  (c-fun freetype-ffi:ft-done-free-type library))
(export 'destroy-library)

(defmacro with-init (&body body)
  `(let ((*library* (make-library)))
     (unwind-protect (progn ,@body)
       (destroy-library *library*))))
(export 'with-init)

(defun get-library ()
  *library*)
(export 'get-library)


