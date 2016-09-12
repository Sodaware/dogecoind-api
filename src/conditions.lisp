;;; conditions.lisp -- Error conditions and warnings

(in-package #:dogecoind-api)


;; ----------------------------------------
;; -- Deprecation warnings

(defvar *deprecation-warnings-enabled-p* nil
  "If true, deprecated parameters will trigger a warning.")

(defun enable-deprecation-warnings ()
  "Enable deprecation warnings."
  (setf *deprecation-warnings-enabled-p* t))

(defun disable-deprecation-warnings ()
  "Disable deprecation warnings."
  (setf *deprecation-warnings-enabled-p* nil))

(defmacro is-deprecated (parameter)
  "Check if PARAMETER is present. If it is, raise a warning."
  (let ((parameter-name (format nil "~a" parameter)))
    `(when (and ,parameter *deprecation-warnings-enabled-p*) 
       (deprecated-parameter-warning ,parameter-name))))

(defmacro method-is-deprecated (name)
  "Raise a warning that the method is deprecated."
  (let ((method-name (format nil "~a" name)))
    `(when (and ,name *deprecation-warnings-enabled-p*) 
       (deprecated-method-warning ,method-name))))


;; ----------------------------------------
;; -- Deprecation warnings

(defun deprecated-parameter-warning (name)
  "Signal a deprecation warning for parameter NAME."
  (warn 'deprecated-parameter
        :text (format nil "Parameter `~a` is deprecated" name)))

(defun deprecated-method-warning (name)
  "Signal a deprecation warning for method NAME."
  (warn 'deprecated-method
        :text (format nil "Method `~a` is deprecated" name)))

(define-condition deprecated-parameter (warning)
  ()
  (:documentation "A warning signalled when a function parameter has been deprecated"))

(define-condition deprecated-method (warning)
  ()
  (:documentation "A warning signalled when a method has been deprecated"))


;; ----------------------------------------
;; -- Error conditions

(define-condition dogecoind-error (simple-error)
  ()
  (:documentation "Base error for dogecoind."))

(define-condition parameter-error (dogecoind-error)
  ()
  (:documentation "Signalled if an invalid parameter was sent."))

(define-condition server-error (dogecoind-error)
  ((message :initarg :message :reader server-error-message)
   (code :initarg :code :reader server-error-code))
  (:report (lambda (condition stream)
             (format stream "Server returned error #~a: ~a"
                     (server-error-code condition)
                     (server-error-message condition))))
  (:documentation "Thrown if an error was returned by the server."))

(define-condition configuration-error (dogecoind-error)
  ()
  (:documentation "Signalled if configuration values are not set or are incorrect."))

