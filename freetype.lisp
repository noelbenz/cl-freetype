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

(defmacro def-record-reader (wrapper-class method-name &key (no-export nil)
                                                            (result-wrapper-class nil)
                                                            (inhibit-string-conversion nil)
                                                            fields)
  "Generates a generic method named `method-name' that reads the record of
`wrapper-class' described by `fields'"
  (with-gensyms (wrapper result)
    `(progn
       (defgeneric ,method-name (object))
       (defmethod ,method-name ((,wrapper ,wrapper-class))
         (let ((,result ,(if inhibit-string-conversion
                             `(second (multiple-value-list
                                       (autowrap:inhibit-string-conversion
                                         (c-ref ,wrapper ,wrapper-class ,@fields))))
                             `(c-ref ,wrapper ,wrapper-class ,@fields)) ))
           ,(if result-wrapper-class
                `(autowrap:wrap-pointer
                  (if (cffi:pointerp ,result) ,result (ptr ,result))
                  (quote ,result-wrapper-class))
                result)))
       ,@(unless no-export `((export (quote ,method-name)))))))

(defmacro def-record-readers (wrapper-class record-descriptors)
  "Generates a generic method for each descriptor in `record-descriptors' which
reads a record from `wrapper-class'.

Each record descriptor has the following form:
 (method-name :no-export t :result-wrapper-class freetype-ffi:ft-face-rec :fields (:some-field))"
  `(progn
     ,@(loop for descriptor in record-descriptors
            collect `(def-record-reader ,wrapper-class ,@descriptor))))

(defmacro def-flag-combiner (fn-name prefix &optional (suffix "+"))
  (with-gensyms (flags flag)
    `(defun ,fn-name (&rest ,flags)
       (reduce
        (lambda (lhs rhs) (logior lhs rhs))
        (mapcar (lambda (,flag)
                  (ecase ,flag
                    ,@(let ((prefix-upper (string-upcase prefix)))
                        (loop for sym being the external-symbols of 'freetype-ffi
                           when (and (string-starts-with (symbol-name sym) prefix-upper)
                                     (boundp sym))
                           collect
                             (let* ((sym-name (symbol-name sym))
                                    (keyword-name (subseq sym-name
                                                          (length prefix-upper)
                                                          (- (length sym-name) (length suffix)))))
                               `(,(intern keyword-name "KEYWORD") ,(symbol-value sym)))))))
                ,flags)
        :initial-value 0))))

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

(def-record-readers freetype-ffi:ft-face-rec
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
     (glyph :fields (:glyph) :result-wrapper-class freetype-ffi:ft-glyph-slot-rec)
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

(def-record-readers freetype-ffi:ft-size-metrics
    ((x-ppem :fields (:x-ppem))
     (y-ppem :fields (:y-ppem))
     (y-scale :fields (:y-scale))
     (x-scale :fields (:x-scale))
     (ascender :fields (:ascender))
     (descender :fields (:descender))
     (height :fields (:height))
     (max-advance :fields (:max-advance))))

(defgeneric get-char-index (face charcode))
(export 'get-char-index)

(defmethod get-char-index ((face freetype-ffi:ft-face-rec) (charcode integer))
  "Looks up a unicode character in the face and returns its index. Character should be specified as a UTF-32 integer."
  (c-fun freetype-ffi:ft-get-char-index face charcode))

(defmethod get-char-index ((face freetype-ffi:ft-face-rec) (charcode character))
  "Looks up a character in the face and returns its index.

This may not work correctly on all implementations.
It depends on the behaviour of char-int to return a UTF-32 integer."
  (c-fun freetype-ffi:ft-get-char-index face (char-int charcode)))

(def-flag-combiner combine-load-flags "+ft-load-")

(defgeneric load-glyph (face glyph-index &rest load-flags))
(export 'load-glyph)

(defmethod load-glyph ((face freetype-ffi:ft-face-rec) glyph-index &rest load-flags)
  (handle-error-c-fun freetype-ffi:ft-load-glyph
                      face
                      glyph-index
                      (apply #'combine-load-flags load-flags)))

(def-record-readers freetype-ffi:ft-glyph-slot-rec
    ((library :fields (:library))
     (face :fields (:face))
     (next :fields (:next))
     (glyph-index :fields (:glyph-index))
     (generic :fields (:generic))
     (metrics :fields (:metrics))
     (linear-hori-advance :fields (:linear-hori-advance))
     (linear-vert-advance :fields (:linear-vert-advance))
     (advance :fields (:advance))
     (glyph-format :fields (:format))
     (bitmap :fields (:bitmap) :result-wrapper-class freetype-ffi:ft-bitmap)
     (bitmap-left :fields (:bitmap-left))
     (bitmap-top :fields (:bitmap-top))
     (outline :fields (:outline))
     (num-subglyphs :fields (:num-subglyphs))
     (subglyphs :fields (:subglyphs))
     (control-data :fields (:control-data))
     (control-len :fields (:control-len))
     (lsb-delta :fields (:lsb-delta))
     (rsb-delta :fields (:rsb-delta))))

(def-flag-combiner combine-render-mode-flags "+ft-render-mode-")

(defmethod render-glyph (glyph &rest render-mode-flags)
  (handle-error-c-fun freetype-ffi:ft-render-glyph glyph
                      (apply #'combine-render-mode-flags render-mode-flags)))

(def-record-readers freetype-ffi:ft-bitmap
    ((rows :fields (:rows))
     (width :fields (:width))
     (pitch :fields (:pitch))
     (buffer-ptr :inhibit-string-conversion t :fields (:buffer))
     (num-grays :fields (:num-grays))
     (pixel-mode :fields (:pixel-mode))))

(defgeneric buffer (bitmap))
(export 'buffer)

(defmethod buffer ((bitmap freetype-ffi:ft-bitmap))
  (let ((ptr (buffer-ptr bitmap))
        (array (make-array (list (rows bitmap) (width bitmap))
                           :element-type 'unsigned-byte)))
    (dotimes (y (rows bitmap))
      (dotimes (x (width bitmap))
        (setf (aref array y x) (cffi:mem-aref ptr :unsigned-char (+ (* y (width bitmap)) x)))))
    array))

#+nil
(freetype:with-init
  (format t "Library loaded!~%")
  (let ((face (make-face "projects/freetype/BebasNeueBold.ttf")))
    (set-char-size face :char-width (to-26-6 12) :horizontal-resolution 80 :vertical-resolution 120)
    (format t "Face: ~A~%" face)
    (format t "Number of faces: ~A~%" (num-faces face))
    (format t "Ascender: ~A~%" (ascender face))
    (format t "X Scale: ~A~%" (x-scale (size-metrics face)))
    (format t "Char index: ~A~%" (get-char-index face #\A))
    (load-glyph face (get-char-index face #\A))
    (let ((glyph (glyph face)))
      (format t "Glyph index: ~A~%" (glyph-index glyph))
      (render-glyph glyph)
      (let ((bitmap (bitmap glyph)))
        (format t "Glyph bitmap: ~A~%" bitmap)
        (format t "Bitmap width: ~A~%" (width bitmap))
        (format t "Bitmap height: ~A~%" (rows bitmap))
        (format t "Bitmap buffer: ~A~%" (buffer bitmap))))
    (destroy-face face)))

