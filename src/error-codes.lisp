;;; error-codes.lisp -- Constants for error codes that the API can return.

(in-package #:dogecoind-api)


;; ----------------------------------------
;; -- Error constants

;; Standard JSON-RPC 2.0 errors
(defconstant +rpc-invalid-request+  -32600)
(defconstant +rpc-method-not-found+ -32601)
(defconstant +rpc-invalid-params+   -32602)
(defconstant +rpc-internal-error+   -32603)
(defconstant +rpc-parse-error+      -32700)

;; General application defined errors
(defconstant +rpc-misc-error+                  -1 "std::exception thrown in command handling")
(defconstant +rpc-forbidden-by-safe-mode+      -2   "Server is in safe mode, and command is not allowed in safe mode")
(defconstant +rpc-type-error+                  -3   "Unexpected type was passed as parameter")
(defconstant +rpc-invalid-address-or-key+      -5   "Invalid address or key")
(defconstant +rpc-out-of-memory+               -7   "Ran out of memory during operation")
(defconstant +rpc-invalid-parameter+           -8   "Invalid, missing or duplicate parameter")
(defconstant +rpc-database-error+              -20  "Database error")
(defconstant +rpc-deserialization-error+       -22  "Error parsing or validating structure in raw format")
(defconstant +rpc-verify-error+                -25  "General error during transaction or block submission")
(defconstant +rpc-verify-rejected+             -26  "Transaction or block was rejected by network rules")
(defconstant +rpc-verify-already-in-chain+     -27  "Transaction already in chain")
(defconstant +rpc-in-warmup+                   -28 "Client still warming up")

;; P2P client errors
(defconstant +rpc-client-not-connected+        -9   "Dogecoin is not connected")
(defconstant +rpc-client-in-initial-download+  -10  "Still downloading initial blocks")
(defconstant +rpc-client-node-already-added+   -23  "Node is already added")
(defconstant +rpc-client-node-not-added+       -24  "Node has not been added before")
(defconstant +rpc-client-node-not-connected+   -29  "Node to disconnect not found in connected nodes")
(defconstant +rpc-client-invalid-ip-or-subnet+ -30  "Invalid IP/Subnet")

;; Wallet errors
(defconstant +rpc-wallet-error+                -4   "Unspecified problem with wallet (key not found etc.)")
(defconstant +rpc-wallet-insufficient-funds+   -6   "Not enough funds in wallet or account")
(defconstant +rpc-wallet-invalid-account-name+ -11  "Invalid account name")
(defconstant +rpc-wallet-keypool-ran-out+      -12  "Keypool ran out, call keypoolrefill first")
(defconstant +rpc-wallet-unlock-needed+        -13  "Enter the wallet passphrase with walletpassphrase first")
(defconstant +rpc-wallet-passphrase-incorrect+ -14  "The wallet passphrase entered was incorrect")
(defconstant +rpc-wallet-wrong-enc-state+      -15  "Command given in wrong wallet encryption state (encrypting an encrypted wallet etc.)")
(defconstant +rpc-wallet-encryption-failed+    -16  "Failed to encrypt the wallet")
(defconstant +rpc-wallet-already-unlocked+     -17  "Wallet is already unlocked")
