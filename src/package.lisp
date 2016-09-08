;;; package.lisp -- Exported things

(defpackage #:dogecoind-api
  (:use #:cl)
  (:export #:*deprecation-warnings-enabled-p*
           #:enable-deprecation-warnings
           #:disable-deprecation-warnings
           
           ;; Client object
           #:client
           #:make-client
           #:authorize
           #:authorizedp

           ;; RPC Methods
           #:add-multisig-address
           #:get-account-balance
           #:get-server-balance))
