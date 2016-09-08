;;; rpc.lisp -- Functions for making RPC calls

(in-package #:dogecoind-api)


;; ----------------------------------------
;; -- Making requests

(defun request (endpoint-template action &optional parameters (method :get))
  "Make a request to the RPC server. Raises `server-error` if the server returns an error value."
  (let* ((endpoint (format nil endpoint-template action))
         (payload (create-payload action parameters))
         (response (drakma:http-request endpoint :method method :content payload))) 
    (let ((decoded-response (json:decode-json-from-string response)))
      (when (and (assoc-cdr :error decoded-response)
                 (listp (assoc-cdr :error decoded-response)))
        (error 'server-error
               :message (server-error-message decoded-response)
               :code (server-error-code decoded-response)))
      (if (listp decoded-response)
          (assoc-cdr :result decoded-response)
          decoded-response))))

(defun post-request (endpoint action &optional parameters)
  "Make a POST request to ENDPOINT with ACTION and optional PARAMETERS"
  (request endpoint action parameters :post))

(defun get-request (endpoint action &optional parameters)
  "Make a GET request to ENDPOINT with ACTION and optional PARAMETERS"
  (request endpoint action parameters :get))


;; ----------------------------------------
;; -- Creating endpoints and payloads

(defmethod client-endpoint ((client client))
  "Create the base address endpoint for an RPC call."
  (format nil "~a://~a:~a/~a/"
          (client-protocol client)
          (client-host client)
          (client-port client)
          "~(~a~)"))

(defmethod create-endpoint ((client client) method)
  "Create the address endpoint for a simple call to METHOD."
  (format nil (client-endpoint client) method))

(defun create-payload (method &optional parameters)
  "Create a JSON-formatted RPC payload for METHOD with PARAMETERS."
  (let ((request (append `((:method . ,method)) `((:params . ,parameters)))))
    (when (null parameters)
      (setq request (remove (assoc :params request) request)))
    (json:encode-json-to-string request)))

(defun create-parameter-list (&rest parameters)
  "Create a list of parameters that can be sent to an RPC endpoint.
Any nil values after `:optional` will be removed."
  (let ((result (list))
        (in-optional nil))
    (dolist (parameter parameters)
      (when (eq parameter :optional)
        (setf in-optional t))
      (unless (eq parameter :optional)
        (if in-optional
            (when parameter (push parameter result))
            (push parameter result))))
    (reverse result)))
