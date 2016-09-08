;;; util.lisp -- Internal utility functions

(in-package #:dogecoind-api)

(defun assoc-cdr (key list)
  (cdr (assoc key list)))

(defun server-error-message (response)
  "Get the error message from a server error object."
  (assoc-cdr :message (assoc-cdr :error response)))

(defun server-error-code (response)
  "Get the error code from a server error object."
  (assoc-cdr :code (assoc-cdr :error response)))
