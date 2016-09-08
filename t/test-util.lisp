;;; t/test-util.lisp -- Utility functions for testing.

(in-package :cl-user)
(defpackage dogecoind-api-test
  (:use :cl        
        :prove
        :cl-mock))
(in-package :dogecoind-api-test)

(defun read-file-into-string (file-path)
  (with-open-file (file-stream file-path)
    (let ((file-contents (make-string (file-length file-stream))))
      (read-sequence file-contents file-stream)
      file-contents)))

(defun fixture-file-name (action override)
  (if override
      (format nil "t/fixtures/~a.json" override)
      (format nil "t/fixtures/~a.json" action)))

(defmacro with-mocked-request (action &body body)
  (let ((expected-uri (format nil "http://127.0.0.1:8334/~a/" action))
        (mocked-contents (read-file-into-string (format nil "t/fixtures/~a.json" action)))
        (payload-content (dogecoind-api::create-payload action)))
    `(with-mocks ()
       (answer (drakma:http-request uri :method :get :content payload-content)
               (progn
                 (is uri ,expected-uri "Generates correct endpoint")
                 ,mocked-contents))
       (progn ,@body))))

(defmacro with-mocked-payload (action (&key (payload nil) (fixture nil) (debug nil)) &body body)
  (when debug
    (ok (probe-file (fixture-file-name action fixture)) "Fixture file exists"))
  (let ((expected-uri (format nil "http://127.0.0.1:8334/~a/" action))
        (response (read-file-into-string (fixture-file-name action fixture)))
        (payload-content (dogecoind-api::create-payload action payload)))    
    (when debug
      (diag (format nil "Expected endpoint: ~a" expected-uri))
      (diag (format nil "Generated payload: ~a" payload-content)))
    `(with-mocks ()
       (answer (drakma:http-request uri :method :post :content ,payload-content)
               (progn
                 (is uri ,expected-uri "Generates correct endpoint")
                 ,response))
       (progn ,@body))))

(defmacro signals-deprecation (name &body body)
  `(let ((dogecoind-api:*deprecation-warnings-enabled-p* t))
     (is-error ,@body
               'dogecoind-api::deprecated-parameter
               (format nil "Signals a deprecation warning when `~a` is passed" ,name))))
