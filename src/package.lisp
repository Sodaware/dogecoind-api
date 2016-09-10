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
           #:account-balance
           #:add-multisig-address
           #:backup-wallet
           #:create-multisig-address
           #:server-balance))
