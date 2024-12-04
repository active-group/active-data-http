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

(TODO: Warning about potential coupling and when to (not) use this)

And some basic, less opinionated formatters that you can use as a
basis to define the format of your API data:

```
active.data.http.formats.transit/basic-formatters
```

This includes things like formatting strings as strings, numbers as
numbers, and sequences as vectors.
(TODO: details)

### Reitit coercion

A reitit coercion based on realms and a format.
TODO

```clojure
(require '[active.data.http.reitit :as http-reitit])

(def my-format ...)

["/api/public/get-user/:id"
 {:get {:handler (fn [request]
                   {:status 200
                    :body (db/get-user-from-db (:id (:path (:parameters request))))})
        :parameters {:path {:id realm/integer}}
        :responses {200 {:body user}}
        :coercion (http-reitit/realm-coercion my-format)}}]
```

TODO required middleware
```
[rrc/coerce-exceptions-middleware
 rrc/coerce-request-middleware
 rrc/coerce-response-middleware]
```

### RPCs

TODO Intended to be used for 'internal apis' only.

```clojure
(require '[active.data.http.rpc :as rpc #?(:cljs :include-macros true)])

(def internal-api (rpc/context "/api/internal"))

(rpc/defn-rpc get-user! internal-api :- user [id :- realm/integer])
```

```clojure
(require '[active.data.http.rpc.reitit :as rpc-reitit])

(def app
  (rpc-reitit/context-router
    internal-api
    [(rpc-reitit/impl get-user! db/get-user-from-db]))
```

```clojure
(require '[active.data.http.rpc :as rpc #?(:cljs :include-macros true)])
#?(:cljs (require '[active.data.http.rpc.reacl-c :as rpc-reacl-c]))

(def internal-api (-> (rpc/context "/api/internal")
                      #?(:cljs (rpc/set-context-caller rpc-reacl-c/caller))))
```

```clojure
(require '[reacl-c-basics.ajax :as ajax])

(ajax/fetch (get-user! 4711))
```


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
