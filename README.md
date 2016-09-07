# dogecoind-api

## Description

**dogecoind-api** is a Common Lisp library for working with a `dogecoind` server
via its RPC interface.


## Installation (with Quicklisp)

Clone the repository into your `~/quicklisp/local-projects/` directory:

```bash
cd ~/quicklisp/local-projects/
git clone https://github.com/sodaware/dogecoind-api.git
```

Include the code where needed:

```lisp
(ql:quickload :dogecoind-api)
```


## Quick usage example

### Getting the balance of the whole wallet

```lisp
(let* ((client (dogecoind-api:make-client))
       (balance (dogecoind-api:get-server-balance client))) 
  (format t "Server balance: Ð ~a" balance))
=> "Server balance: Ð 12345.6"
```

### Connecting to the test network

```lisp
(let* ((client (dogecoind-api:make-client :port 44555))
       (balance (dogecoind-api:get-server-balance client))) 
  (format t "Test balance: Ð ~a" balance))
=> "Server balance: Ð 12345.6"
```


## Testing

Tests use `[prove](https://github.com/fukamachi/prove)` and live in the `/t/`
directory. With SBCL installed they can be executed by running `make test`.

## Function Documentation

Documentation for each method can be found on the [Bitcoin](https://en.bitcoin.it/wiki/Original_Bitcoin_client/API_calls_list) wiki.

The API functions are mapped as follows:

API Method           | Local Method
---------------------|--------------------------------------
addmultisigaddress   | dogecoind-api:add-multisig-address
getbalance           | dogecoind-api:get-account-balance
                     | dogecoind-api:get-server-balance

Some API methods are wrapped with several helper methods where appropriate.


#### dogecoind-api:add-multisig-address *client* *keys* &optional *account*

Add a multisignature address to the wallet that requires *keys* in order to
spend and return the newly-created dogecoin address.

Each *key* in *keys* is a bitcoin address or hex-encoded public key. 

If *account* is specified, the address with be assigned to that account. Note
that this parameter is now deprecated..

```lisp
(dogecoind-api:add-multisig-address *client* (list "address1" "address2"))
=> "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e"
```

#### dogecoind-api:get-account-balance *client* *account*

Get the available balance for *account*.

```lisp
(dogecoind-api:get-account-balance *client* "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> 12345.67890
```

#### dogecoind-api:get-server-balance *client*

Get the total balance of all accounts on the server.

```lisp
(dogecoind-api:get-server-balance *client*)
=> 12345.67890
```


## Licence

Copyright (C) 2016 Phil Newton

DOGECOIND-API is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

DOGECOIND-API is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with DOGECOIND-API. If not, see http://www.gnu.org/licenses/.
