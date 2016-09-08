(in-package :cl-user)
(defpackage dogecoind-api-test
  (:use :cl
        :dogecoind-api
        :prove
        :cl-mock))
(in-package :dogecoind-api-test)

(defvar *default-client* (make-client))

(plan 4)

(subtest "::client-endpoint"

  (is "http://127.0.0.1:8334/~(~a~)/"
      (dogecoind-api::client-endpoint *default-client*)
      "Creates correct endpoint with default client")

  (let ((client (dogecoind-api:make-client :host "localhost")))
    (is "http://localhost:8334/~(~a~)/"
        (dogecoind-api::client-endpoint client)
        "Includes customized :host in endpoint"))

  (let ((client (dogecoind-api:make-client :port 1234)))
    (is "http://127.0.0.1:1234/~(~a~)/"
        (dogecoind-api::client-endpoint client)
        "Includes customized :post in endpoint"))

  (let ((client (dogecoind-api:make-client :protocol "https")))
    (is "https://127.0.0.1:8334/~(~a~)/"
        (dogecoind-api::client-endpoint client)
        "Includes customized :protocol in endpoint")))


(subtest "::create-endpoint"

  (is "http://127.0.0.1:8334/getinfo/"
      (dogecoind-api::create-endpoint *default-client* "getinfo")
      "Can create an endpoint with default client")
  (is "http://127.0.0.1:8334/getinfo/"
      (dogecoind-api::create-endpoint *default-client* "GetInFo")
      "Converts actions to lowercase")

  (let ((client (dogecoind-api:make-client :host "localhost")))
    (is "http://localhost:8334/getinfo/"
        (dogecoind-api::create-endpoint client "getinfo")
        "Can customize host"))

  (let ((client (dogecoind-api:make-client :port 1234)))
    (is "http://127.0.0.1:1234/getinfo/"
        (dogecoind-api::create-endpoint client "getinfo")
        "Can customize port"))

  (let ((client (dogecoind-api:make-client :protocol "https")))
    (is "https://127.0.0.1:8334/getinfo/"
        (dogecoind-api::create-endpoint client "getinfo")
        "Can customize protocol")))

(subtest "::create-payload"

  (is (dogecoind-api::create-payload "getinfo")
      "{\"method\":\"getinfo\"}"
      "Creates a payload with no parameters")

  (is (dogecoind-api::create-payload "getinfo" (list 1 2 3))
      "{\"method\":\"getinfo\",\"params\":[1,2,3]}"
      "Creates a payload with a list of parameters"))

(subtest "::create-parameter-list"

  (is (dogecoind-api::create-parameter-list "a" "b" "c")
      (list "a" "b" "c")
      "Can create parameters list when no values are optional")

  (is (dogecoind-api::create-parameter-list "a" "b" "c" :optional "d" "e" "f")
      (list "a" "b" "c" "d" "e" "f")
      "Can create parameters list when values are optional and present")

  (is (dogecoind-api::create-parameter-list "a" "b" "c" :optional "d" nil "f")
      (list "a" "b" "c" "d" "f")
      "Can create parameters list when values are optional and nil"))

(finalize)
