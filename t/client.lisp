(in-package :cl-user)
(defpackage dogecoind-api-test
  (:use :cl
        :dogecoind-api
        :prove
        :cl-mock))
(in-package :dogecoind-api-test)

;; ----------------------------------------
;; -- Test setup

(defvar *basic-client* (make-client))

(plan 9)


;; ----------------------------------------
;; -- Client

(subtest "client"
  (ok (make-client) "Can create a client"))


;; ----------------------------------------
;; -- Authorization helpers

(subtest ":authorize"
  (let ((client (make-client)))
    (dogecoind-api:authorize client "username" "password")
    (is (dogecoind-api::client-username client) "username" "Sets the username")
    (is (dogecoind-api::client-password client) "password" "Sets the password")))

(subtest ":authorizedp"
  (let ((client (make-client)))
    (is (dogecoind-api:authorizedp client) nil "Returns nil before client authorized")
    (dogecoind-api:authorize client "username" "password")
    (is (dogecoind-api:authorizedp client) t "Returns true after client authorized")))


;; ----------------------------------------
;; -- RPC Methods

(subtest ":account-address"
  (with-mocked-payload "getaccountaddress"
    (:payload ("account-1"))
    (is (dogecoind-api:account-address *basic-client* "account-1")
        "abc123"
        "Returns the address for an account"))
  (signals-method-deprecation
   "account-address"
   (dogecoind-api:account-address *basic-client* "account-1")))

(subtest ":add-multisig-address"
  (with-mocked-payload "addmultisigaddress"
    (:payload (1 ("address-1")))
    (ok (dogecoind-api:add-multisig-address *basic-client* (list "address-1"))
        "Can add a multisig address when no account specified"))
  (with-mocked-payload "addmultisigaddress"
    (:payload (3 ("key-1" "key-2" "key-3")))
    (ok (dogecoind-api:add-multisig-address *basic-client* (list "key-1" "key-2" "key-3"))
        "Can add a multisig address with multiple keys"))
  (with-mocked-payload "addmultisigaddress"
    (:payload (3 ("key-1" "key-2" "key-3") "account"))
    (ok (dogecoind-api:add-multisig-address *basic-client* (list "key-1" "key-2" "key-3") "account")
        "Can add a multisig address and assign to an account"))
  (signals-deprecation "account"
                       (dogecoind-api:add-multisig-address *basic-client* (list "key-1") "account-value")))

(subtest ":backup-wallet"
  (with-mocked-payload "backupwallet"
    (:payload ("~/destination.wallet")) 
    (ok (dogecoind-api:backup-wallet *basic-client* "~/destination.wallet")
        "Copies wallet when a filename is provided")))

(subtest ":create-multisig-address"
  (with-mocked-payload "createmultisig"
    (:payload (2 ("address1" "address2")))
    (multiple-value-bind (address redeem-script)
        (dogecoind-api:create-multisig-address *basic-client* (list "address1" "address2"))
      (is address "new-address-here"
          "Returns the added address")
      (is redeem-script "hex-encoded-string-here"
          "Returns the redeem script"))))

(subtest ":server-balance"
  (with-mocked-payload "getbalance"
    (:payload ("*" 1))
    (is (dogecoind-api:server-balance *basic-client*) 123.123465
        "Gets the balance for the server")))

(subtest ":account-balance"
  (with-mocked-payload "getbalance"
    (:payload ("testaccount" 1) :fixture "getbalance-testaccount")
    (is (dogecoind-api:account-balance *basic-client* "testaccount") 321.12347
        "Gets the balance for the account")))

(finalize)
