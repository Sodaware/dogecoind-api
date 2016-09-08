;;; client.lisp -- Main client functionality. Wraps all RPC endpoints.

(in-package #:dogecoind-api)


;; ----------------------------------------
;; -- Client

;; Wraps all client connection details in a single structure. Default values
;; will connect to a standard dogecoin server without needing extra
;; configuration.

(defstruct client
  (host "127.0.0.1")
  (port 8334)
  (protocol "http")
  (username nil)
  (password nil))


;; ----------------------------------------
;; -- Authorization helpers

(defmethod authorize ((client client) username password)
  "Authorize the client with USERNAME and PASSWORD."
  (setf (client-username client) username)
  (setf (client-password client) password))

(defmethod authorizedp ((client client))
  "Check if the client has had a username and password set."
  (and (not (null (client-username client)))
       (not (null (client-password client)))))


;; ----------------------------------------
;; -- RPC request macros

(defmacro http-get (client action)
  "Makes a HTTP GET request to ACTION for CLIENT."
  `(get-request (client-endpoint ,client) ,action))

(defmacro http-post (client action &rest parameters)
  "Makes a HTTP POST request to ACTION for CLIENT with PARAMETERS as the payload."
  `(post-request (client-endpoint ,client)
                 ,action
                 (create-parameter-list ,@parameters)))


;; ----------------------------------------
;; -- RPC methods

(defmethod add-multisig-address ((client client) keys &optional account)
  "Add a multisignature address to the wallet that requires KEYS to release and return the address.

Each KEY in KEYS is a dogecoin address or hex-encoded public key.
If ACCOUNT is specified, the address will be assigned to the account."
  (is-deprecated account)
  (http-post client "addmultisigaddress"
             (length keys) keys
             :optional account))

(defmethod get-server-balance ((client client) &optional (minimum-confirmations 1) (watch-only-p nil))
  "Get the total balance for all accounts on the server."
  (get-account-balance client "*" minimum-confirmations watch-only-p))

(defmethod get-account-balance ((client client) account-name &optional (minimum-confirmations 1) (watch-only-p nil))
  "Get the balance for ACCOUNT-NAME."
  (http-post client "getbalance" account-name
             :optional minimum-confirmations watch-only-p))
