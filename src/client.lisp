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
  "Makes an RPC POST request for CLIENT. Executes ACTION with PARAMETERS."
  `(post-request (client-endpoint ,client)
                 ,action 
                 (create-parameter-list ,@parameters)
                 (client-authorization ,client)))


;; ----------------------------------------
;; -- Account methods

(defmethod accounts ((client client))
  "Get a list of account names and their balances"
  (http-post client "listaccounts"))


;; ----------------------------------------
;; -- Address methods

(defmethod address-account ((client client) dogecoin-address)
  "Get the account for DOGECOIN-ADDRESS."
  (http-post client "getaccount" dogecoin-address))

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

(defmethod validate-address ((client client) dogecoin-address)
  "Fetch information about DOGECOIN-ADDRESS"
  (http-post client "validateaddress" dogecoin-address))

(defmethod valid-address-p ((client client) dogecoin-address)
  "Validate DOGECOIN-ADDRESS"
  (let ((response (validate-address client dogecoin-address)))
    (not (null (assoc-cdr :isvalid response)))))

(defmethod address-mine-p ((client client) dogecoin-address)
  "Does this address belong to wallet"
  (let ((response (validate-address dogecoin-address)))
    (not (null (assoc-cdr :ismine response)))))

(defmethod server-balance ((client client) &optional (minimum-confirmations 1) (watch-only-p nil))
  "Get the total balance for all accounts on the server."
  (account-balance client "*" minimum-confirmations watch-only-p))

(defmethod account-balance ((client client) account-name &optional (minimum-confirmations 1) (watch-only-p nil))
  "Get the balance for ACCOUNT-NAME."
  (http-post client "getbalance" account-name
             :optional minimum-confirmations watch-only-p))


;; ----------------------------------------
;; -- Block functions

(defmethod block-hash ((client client) index)
  "Get hash of block at INDEX."
  (http-post client "getblockhash"))


;; ----------------------------------------
;; -- Receive stats

(defmethod received-by-account ((client client) &optional (account "") (minimum-confirmations 1))
  "Returns the total amount received by ACCOUNT in transactions with at least MINIMUM-CONFIRMATIONS confirmations."
  (http-post client "getreceivedbyaccount" :optional account minimum-confirmations))

(defmethod received-by-address ((client client) &optional dogecoin-address (minimum-confirmations 1))
  "Returns the total amount received by DOGECOIN-ADDRESS in transactions with at least MINIMUM-CONFIRMATIONS confirmations.
This function only counts *receiving* transactions and does not check payments
that have been made.  Works only for addresses in the local wallet, external
addresses will always show 0."
  (http-post client "getreceivedbyaddress" dogecoin-address :optional minimum-confirmations))


;; ----------------------------------------
;; -- Transaction functions

(defmethod transaction ((client client) transaction-id)
  "Get information about transaction TRANSACTION-ID."
  (http-post client "gettransaction" transaction-id))

(defmethod recent-transactions ((client client) &optional (account "") (count 10) (from 0))
  (http-post client "listtransactions" account count from))


;; ----------------------------------------
;; -- Server functions

(defmethod block-count ((client client))
  "Get the number of blocks in the longest block chain."
  (http-post client "blockcount"))

(defmethod connection-count ((client client))
  "Get the number of connections to other nodes."
  (http-post client "getconnectioncount"))

(defmethod difficulty ((client client))
  "Get the proof-of-work difficulty as a multiple of the minimum difficulty."
  (http-post client "getdifficulty"))

(defmethod generatep ((client client))
  "Check if server is currently generating hashes."
  (http-post client "getgenerate"))

(defmethod hashes-per-second ((client client))
  "Get hashes-per-second performance during generation"
  (http-post client "hashespersecond"))

(defmethod info ((client client))
  "Get various bits of state information."
  (http-post client "getinfo"))

(defmethod mining-info ((client client))
  "Get mining-related information from the server."
  (http-post client "getmininginfo"))

(defmethod peer-info ((client client))
  "Get data about each connected node."
  (http-post client "getpeerinfo"))

(defmethod refill-keypool ((client client))
  "Fills the keypool. Requires a wallet passphrase to be set."
  (http-post client "keypoolrefill"))

(defmethod set-transaction-fee ((client client) transaction-fee)
  "Sets the transaction fee per kilobyte paid by transactions created by this wallet."
  (http-post client "settxfee" transaction-fee))

(defmethod stop-server ((client client))
  "Stop the dogecoind server."
  (http-post client "stop"))


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
