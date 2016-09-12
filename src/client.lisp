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
  (setf (client-password client) password)
  t)

(defmethod authorizedp ((client client))
  "Check if the client has had a username and password set."
  (and (not (null (client-username client)))
       (not (null (client-password client)))))

(defmethod client-authorization ((client client))
  "Get client username/password pair to use with Drakma basic-authorization."
  (when (authorizedp client)
    (list (client-username client) (client-password client))))


;; ----------------------------------------
;; -- RPC request macros

(defmacro http-post (client action &rest parameters)
  "Makes a HTTP POST request to ACTION for CLIENT with PARAMETERS as the payload."
  `(post-request (client-endpoint ,client)
                 ,action 
                 (create-parameter-list ,@parameters)
                 (client-authorization ,client)))


;; ----------------------------------------
;; -- RPC methods

(defmethod account-address ((client client) account)
  "Get the current dogecoin address for receiving payments to ACCOUNT.
If ACCOUNT doesn't exist it will be created."
  (method-is-deprecated "account-address")
  (http-post client "getaccountaddress" account))

(defmethod add-multisig-address ((client client) keys &optional account)
  "Add a multisignature address to the wallet that requires KEYS to release and return the address.

Each KEY in KEYS is a dogecoin address or hex-encoded public key.
If ACCOUNT is specified, the address will be assigned to the account."
  (is-deprecated account)
  (http-post client "addmultisigaddress"
             (length keys) keys
             :optional account))



(defmethod create-multisig-address ((client client) keys)
  "Create a multisignature address to the wallet that requires KEYS to release and return."  
  (let ((response (http-post client "createmultisig" (length keys) keys)))
    (values (cdr (assoc :address response))
            (cdr (assoc :redeem-script response)))))

(defmethod server-balance ((client client) &optional (minimum-confirmations 1) (watch-only-p nil))
  "Get the total balance for all accounts on the server."
  (account-balance client "*" minimum-confirmations watch-only-p))

(defmethod account-balance ((client client) account-name &optional (minimum-confirmations 1) (watch-only-p nil))
  "Get the balance for ACCOUNT-NAME."
  (http-post client "getbalance" account-name
             :optional minimum-confirmations watch-only-p))

;; ----------------------------------------
;; -- Wallet functions

(defmethod backup-wallet ((client client) destination)
  "Safely copy wallet.dat to DESTINATION. 

DESTINATION can either be a directory or a path with filename." 
  (http-post client "backupwallet" destination)
  destination)

(defmethod encrypt-wallet ((client client) passphrase)
  "Encrypt wallet and require PASSPHRASE to unlock it."
  (http-post client "encryptwallet" passphrase))

(defmethod lock-wallet ((client client))
  "Lock the wallet."
  (http-post client "walletlock"))

(defmethod unlock-wallet ((client client) passphrase timeout)
  "Unlock the wallet using PASSPHRASE and keep unlocked for TIMEOUT seconds."
  (http-post client "walletpassphrase" passphrase timeout))

(defmethod change-wallet-passphrase ((client client) old-passphrase new-passphrase)
  "Change the wallet passphrase from OLD-PASSPHRASE to NEW-PASSPHRASE."
  (http-post client "walletpassphrasechange" old-passphrase new-passphrase))
