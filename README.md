[![Clojars Project](https://img.shields.io/clojars/v/de.active-group/active-data-http.svg)](https://clojars.org/de.active-group/active-data-http)
[![cljdoc badge](https://cljdoc.org/badge/de.active-group/active-data-http)](https://cljdoc.org/d/de.active-group/active-data-http/CURRENT)

The goal of this library is to facilitate using data described by
[realms](https://github.com/active-group/active-data) in web
applications contexts, i.e. when transpoting data via http.

[Latest Version](https://clojars.org/de.active-group/active-data-http)

[API Docs](https://cljdoc.org/d/de.active-group/active-data-http/CURRENT)

## Introduction

There are two main facilities included in this library. One part is
for defining translations of your data in Transit/EDN form. The second
part is to simplify defining and using server endpoints used by fat
web clients.

### Transit format

Based on
[active-data-translate](https://github.com/active-group/active-data-translate),
there is a "batteries included" transit format that can translate
between most of your data and a transit compatible form automatically:

```
active.data.http.formats.transit/transit-format
```

Use this only if the coupling introduced by that is not an issue, or
can be mitigated by other means.

The coupling can for example be
  
- between producer and consumer code of the transit values, if
  they are developed independently, or

- between past and future versions of the code, if transit values are
  written to some kind of databases, or if producer and consumer can
  have different versions of the code.

In those situations, you should define explicit translations for the
realms that are defined in your code and thus are subject to potential
change over time. You can use

```
active.data.http.formats.transit/basic-formatters
```

as a base for that, which includes formatters for things that are
unlikely to change, like formatting strings as strings, numbers as
numbers, and sequences as vectors, etc.

### Reitit coercion

A reitit coercion based on realms and a format is available in the
`active.data.http.reitit` namespace. This allows you to declare
parameters and response bodies as realms, and have them automatically
translated via the format.

```clojure
(require '[active.data.http.reitit :as http-reitit])

(def-record user [...])

(def my-format ...)

["/api/public/get-user/:id"
 {:get {:handler (fn [request]
                   {:status 200
                    :body (db/get-user-from-db (:id (:path (:parameters request))))})
        :parameters {:path {:id realm/integer}}
        :responses {200 {:body user}}
        :coercion (http-reitit/realm-coercion my-format)}}]
```

See
[active-data-translate](https://github.com/active-group/active-data-translate)
on how to define formats.

Note that to get this working you'll need a few middlewares for these
routes. Namely the coersion middlewares, parameters-middleware and
some transit middleware like muuntaja:

```clojure
{:middleware 
 [reitit.ring.coercion/coerce-exceptions-middleware
  reitit.ring.coercion/coerce-request-middleware
  reitit.ring.coercion/coerce-response-middleware
  reitit.ring.middleware.parameters/parameters-middlware]
 :muuntaja muuntaja.core/instance}
```

See Reitit documentation for more details on how this can be set up.

### RPCs

To make endpoints for a webclient served by the same server and the
usage of them even easier to set up, there is an "RPC"-like facility
included in this library.

It is intended for "internal apis" and shared code (cljc) only, where
the coupling between the server and the client code is not an issue.

To use this, you would first define a so called context and the api in
some shared code (cljc file):

```clojure
(require '[active.data.http.rpc :as rpc #?(:cljs :include-macros true)])

(def internal-api (rpc/context "/api/internal"))

(rpc/defn-rpc get-user! internal-api :- user [id :- realm/integer])
```

And define implementations for those RPCs in some server code, for
example with reitit:

```clojure
(require '[active.data.http.rpc.reitit :as rpc-reitit])

(def app
  (rpc-reitit/context-router
    internal-api
    [(rpc-reitit/impl get-user! db/get-user-from-db]))
```

And finally to "call" those RPCs from the client side, for example with
the [reacl-c](https://github.com/active-group/reacl-c) library, you
would modify the shared api code to add a "caller" to the context like
so:

```clojure
(require '[active.data.http.rpc :as rpc #?(:cljs :include-macros true)])
#?(:cljs (require '[active.data.http.rpc.reacl-c :as rpc-reacl-c]))

(def internal-api (-> (rpc/context "/api/internal")
                      #?(:cljs (rpc/set-context-caller rpc-reacl-c/caller))))
```

Which then enables you to call the names defined by `defn-rpc` as a
function to get a reacl-c request, which can then be executed in various
ways:

```clojure
(require '[reacl-c-basics.ajax :as ajax])

(ajax/fetch (get-user! 4711))
```

See [reacl-c-basics](https://github.com/active-group/reacl-c-basics)
for more details on that.

## License

Copyright © 2024 Active Group GmbH

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
