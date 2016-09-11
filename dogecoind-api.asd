(in-package :cl-user)
(defpackage dogecoind-api-asd
  (:use :cl :asdf))
(in-package :dogecoind-api-asd)

(defsystem dogecoind-api
  :version "0.1.0"
  :author "Phil Newton"
  :license "LGPL 3.0"
  :description "A library for working with a `dogecoind` server via its RPC interface."
  :depends-on (:drakma
               :cl-json)
  :pathname "src"
  :components ((:file "package")
               (:file "error-codes")
               (:file "conditions")
               (:file "client")
               (:file "rpc")
               (:file "util"))
  :in-order-to ((test-op (test-op dogecoind-api-test))))
