(in-package :cl-user)
(defpackage dogecoind-api-test-asd
  (:use :cl :asdf))
(in-package :dogecoind-api-test-asd)

(defsystem dogecoind-api-test
  :author "Phil Newton"
  :license "LGPLv3"
  :depends-on (:dogecoind-api
               :prove
               :cl-mock)
  :components ((:module "t"
                        :components 
                        ((:file "test-util")
                         (:test-file "client")
                         (:test-file "rpc"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)))
