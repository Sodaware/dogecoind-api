;;; package.lisp -- Exported things

(defpackage #:dogecoind-api
  (:use #:cl)
  (:export #:*deprecation-warnings-enabled-p*
           #:enable-deprecation-warnings
           #:disable-deprecation-warnings

           ;; Conditions
           #:deprecated-method
           #:deprecated-parameter

           ;; Client object
           #:client
           #:make-client
           #:authorize
           #:authorizedp

           ;; RPC Methods
           #:account-address
           #:account-balance
           #:add-multisig-address
           #:backup-wallet
           #:create-multisig-address
           #:server-balance

           ;; Error Constants
           #:+rpc-invalid-request+
           #:+rpc-method-not-found+
           #:+rpc-invalid-params+
           #:+rpc-internal-error+
           #:+rpc-parse-error+
           #:+rpc-misc-error+
           #:+rpc-forbidden-by-safe-mode+
           #:+rpc-type-error+
           #:+rpc-invalid-address-or-key+
           #:+rpc-out-of-memory+
           #:+rpc-invalid-parameter+
           #:+rpc-database-error+
           #:+rpc-deserialization-error+
           #:+rpc-verify-error+
           #:+rpc-verify-rejected+
           #:+rpc-verify-already-in-chain+
           #:+rpc-in-warmup+
           #:+rpc-client-not-connected+
           #:+rpc-client-in-initial-download+
           #:+rpc-client-node-already-added+
           #:+rpc-client-node-not-added+
           #:+rpc-client-node-not-connected+
           #:+rpc-client-invalid-ip-or-subnet+
           #:+rpc-wallet-error+
           #:+rpc-wallet-insufficient-funds+
           #:+rpc-wallet-invalid-account-name+
           #:+rpc-wallet-keypool-ran-out+
           #:+rpc-wallet-unlock-needed+
           #:+rpc-wallet-passphrase-incorrect+
           #:+rpc-wallet-wrong-enc-state+
           #:+rpc-wallet-encryption-failed+
           #:+rpc-wallet-already-unlocked+))
