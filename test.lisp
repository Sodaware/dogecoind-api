(load "~/.sbclrc")
(load "dogecoind-api.asd")
(load "dogecoind-api-test.asd")
(asdf:test-system :dogecoind-api)
