# erf - A design-first Erlang REST Framework
[![erf ci](https://github.com/nomasystems/erf/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/erf/actions/workflows/ci.yml)
[![erf docs](https://github.com/nomasystems/erf/actions/workflows/docs.yml/badge.svg)](https://nomasystems.github.io/erf)

`erf` is a design-first Erlang REST framework. It provides an interface to spawn specification-driven HTTP servers with several automated features that aim to ease the development, operation and maintenance of design-first RESTful services. Its HTTP protocol features are provided as a wrapper of the [elli](https://github.com/elli-lib/elli) HTTP 1.1 server.

Requires Erlang/OTP 27 or later.

## What is design-first?

When following a code-first approach to develop APIs, the interface is produced as a result of the implementation and, therefore, client-side code, integration tests and other parts of the system that depend on the API behaviour, need to wait until the server-side work is done.

Design-first is an approach to API development that prioritises the design of the API before its implementation. The explicit contract produced in this design, which should be the result of an agreement between the stakeholders of the API, aims to reduce bottlenecks in the development process.

## How does `erf` help developing design-first RESTful services?

`erf` is an HTTP server framework that, taking an API design in the form of an specification file and a callback module as input, starts a server and dynamically generates code to efficiently type-check and route requests to callback functions. Its main goal is to provide a tool to REST API development in Erlang that reduces the development time by automating the implementation of boilerplate code that can be inferred from the API specification.

## Quickstart

1. Design your API using OpenAPI 3.0. For example: [users.openapi.json](examples/users/priv/users.openapi.json).

2. Add `erf` as a dependency in your `rebar3` project.
```erl
{deps, [
    {erf, {git, "git@github.com:nomasystems/erf.git", {branch, "main"}}}
]}.
```

3. Implement a callback module for your API. A hypothetical example for [users.openapi.json](examples/users/priv/users.openapi.json) would be [users_callback.erl](examples/users/src/users_callback.erl).
```erl
%% An <code>erf</code> callback for the users REST API.
-module(users_callback).

%%% EXTERNAL EXPORTS
-export([
    create_user/1,
    get_user/1,
    delete_user/1
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
create_user(#{body := Body} = _Request) ->
    Id = base64:encode(crypto:strong_rand_bytes(16)),
    ets:insert(users, {Id, Body#{<<"id">> => Id}}),
    {201, [], Body#{<<"id">> => Id}}.

get_user(#{path_parameters := PathParameters} = _Request) ->
    Id = proplists:get_value(<<"userId">>, PathParameters),
    case ets:lookup(users, Id) of
        [] ->
            {404, [], #{
                <<"message">> =>
                    <<"User ", Id/binary, " not found">>
            }};
        [{Id, User}] ->
            {200, [], User}
    end.

delete_user(#{path_parameters := PathParameters} = _Request) ->
    Id = proplists:get_value(<<"userId">>, PathParameters),
    case ets:lookup(users, Id) of
        [] ->
            {404, [], #{
                <<"message">> =>
                    <<"User ", Id/binary, " not found">>
            }};
        [_User] ->
            ets:delete(users, Id),
            {204, [], #{<<"id">> => Id}}
    end.
```

4. Start an `erf` instance using the [`erf:start_link/1`](https://nomasystems.github.io/erf/erf.html#start_link/1) function under the supervisor of your application. For example:
```erl
-module(users_sup).

%%% BEHAVIOURS
-behaviour(supervisor).

%%% START/STOP EXPORTS
-export([start_link/0]).

%%% INTERNAL EXPORTS
-export([init/1]).

%%%-------------------------------------------------------
%%% START/STOP EXPORTS
%%%-------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%-------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-------------------------------------------------------
init([]) ->
    % Users storage
    ets:new(users, [public, named_table]),
    UsersAPIConf = #{
        spec_path => <<"priv/users.openapi.json">>,
        callback => users_callback,
        preprocess_middlewares => [users_preprocess],
        postprocess_middlewares => [users_postprocess],
        port => 8080
    },
    UsersChildSpec = {
        public_api_server,
        {erf, start_link, [UsersAPIConf]},
        permanent,
        5000,
        worker,
        [erf]
    },
    {ok, {{one_for_one, 5, 10}, [UsersChildSpec]}}.
```
Notice the configured preprocess and postprocess middlewares. They implement a basic authorization mechanism, short-circuiting the request and returning a 403 HTTP error code if the `X-API-KEY: api-key` header is missing, and they print in console the time in microseconds that authorized requests take to complete.

5. Start requesting your service.
```sh
$ curl -vvv 'localhost:8080/users' -H 'Content-Type: application/json' -H 'X-API-KEY: api-key' -d '{"username": "foo", "password": "foobar"}'
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /users HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/8.0.1
> Accept: */*
> Content-Type: application/json
> Content-Length: 44
>
< HTTP/1.1 201 Created
< connection: Keep-Alive
< content-length: 73
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"id":"b7R7bJSbaTmoiWwecy2IwA==","password":"foobar","username":"foo"}
```

## `erf` configuration

`erf`'s main entry point (i.e., the `start_link/1` function) receives an API specification, a callback module and a set of optional values that enable its configuration.

The configuration is provided as map with the following type spec:
```erl
%%% erf.erl
-type conf() :: #{
    spec_path := binary() | #{version() => binary()},
    callback := module() | #{version() => module()},
    port => inet:port_number(),
    name => atom(),
    spec_parser => module(),
    default_version => version(),
    preprocess_middlewares => [module()],
    postprocess_middlewares => [module()],
    ssl => boolean(),
    certfile => binary(),
    keyfile => binary(),
    static_routes => [static_route()],
    swagger_ui => boolean(),
    min_acceptors => pos_integer(),
    accept_timeout => pos_integer(),
    request_timeout => pos_integer(),
    header_timeout => pos_integer(),
    body_timeout => pos_integer(),
    max_body_size => pos_integer(),
    log_level => logger:level()
}.
```

A detailed description of each parameter can be found in the following list:
- `spec_path` : Path to API specification file. To serve several versions of the same API from a single instance, provide a map from version to specification file path instead -- see [API versioning](#api-versioning).
- `callback`: Name of the callback module. To serve several versions of the same API, provide a map from version to callback module instead, giving each version its own controller -- see [API versioning](#api-versioning).
- `port`: Port the server will listen to. Defaults to `8080`.
- `name`: Name under which the server is registered. Defaults to `erf`.
- `spec_parser`: Name of the specification parser module. Defaults to `erf_parser_oas_3_0`.
- `default_version`: Only meaningful when `spec_path` is a map. See [API versioning](#api-versioning). Defaults to `undefined`.
- `preprocess_middlewares`: List of names of middlewares to be invoked before the request is forwarded to the callback. Defaults to `[]`.
- `postprocess_middlewares`: List of names of middlewares to be invoked after the response is returned by the callback. Defaults to `[]`.
- `ssl`: Boolean flag that enables/disables SSL. Defaults to `false`.
- `certfile`: Path to the SSL certificate file. Defaults to `undefined`.
- `keyfile`: Path to the SSL key file. Defaults to `undefined`.
- `static_routes`: List of routes that serve static files. Defaults to `[]`.
- `swagger_ui`: Boolean flag that enables/disables the Swagger UI. Defaults to `false`.
- `min_acceptors`: Minimum number of acceptor processes. Defaults to `20`.
- `accept_timeout`: Timeout in ms for accepting an incoming request. Defaults to `10000`.
- `request_timeout`: Timeout in ms for receiving more packets when waiting for the request line. Defaults to `60000`.
- `header_timeout`: Timeout in ms for receiving more packets when waiting for the headers. Defaults to `10000`.
- `body_timeout`: Timeout in ms for receiving more packets when waiting for the body. Defaults to `30000`.
- `max_body_size`: Maximum size in bytes for the body of allowed received messages. Defaults to `1024000`.
- `log_level`: Severity associated to logged messages. Defaults to `error`.

## Callback modules & middlewares

`erf` dynamically generates a router that type check the received requests against the API specification. If the request passes the validation, it is deconstructed and passed to the middleware and callback modules. But, how do those middleware and callback modules must look like?

- **Preprocess middlewares** receive a request, do something with it (such as adding an entry to an access log) and return it for the next middleware or callback module to process it. This allows each preprocess middleware to modify the content of the request, updating any of its fields such as the `context` field, specifically dedicated to store contextual information middlewares might want to provide. Preprocess middlewares can short-circuit the processing flow, returning `{stop, Response}` or `{stop, Response, Request}` instead of just `Request`. The first of those alternatives prevents the following preprocess middlewares to execute, as well as the callback module, skipping directly to the postprocess middlewares. The second alternative response format does the same but allows to modify the request information.

- **Callback module**.
The router expects your callback module to export one function per operation defined in your API specification. It also expects each operation to include an `operationId` that, after being transformed to _snake_case_, will identify the function that is going to be called. Such function receives an `erf:request()` and must return an `erf:response()`. When `spec_path` is configured with several versions (see [API versioning](#api-versioning)), `callback` can name one module per version -- each handling only the operations its own version's specification defines -- or a single module shared by every version, which then relies on `erf:request()`'s `version` field to tell versions apart.

- **Postprocess middlewares** can also update the request, like the preprocess middlewares, by returning a `{erf:response(), erf:request()}` tuple or just return a `erf:response()` and leave the received request intact. This middlewares cannot short-circuit the processing flow.

An example of an API specification and a supported callback can be seen in [Quickstart](#quickstart). Files `users_preprocess.erl` and `users_postprocess.erl` under `examples/users` exemplify how to use `erf` middlewares. Try out the example by running `rebar3 as examples shell` from the root of this project.

## API versioning

A design-first API is defined by its specification file, but that specification is not static: it evolves. `erf` lets a single running instance serve **several versions of the same API specification at once**, so that a new version can be rolled out without cutting off clients that are still using an older one.

A runnable example lives in [`examples/products`](examples/products): a `Product`'s `price` used to be a plain number (v1) and, to support multiple currencies, became a `{amount, currency}` object (v2) -- a realistic, backwards-incompatible change. v1 and v2 each have their own callback module (`products_v1_callback`, `products_v2_callback`), sharing storage through a plain, version-agnostic `products_store` module. Run it with `rebar3 as examples_versioning shell` and try it out:
```sh
# v1 clients keep sending a plain number; `products_v1_callback` translates it to/from the
# `{amount, currency}` shape `products_store` keeps internally.
$ curl -X POST localhost:8081/v1/products -d '{"name":"Widget","price":19.99}'
{"id":"...","name":"Widget","price":19.99}

# v2 clients use the new shape directly; `products_v2_callback` passes it straight through.
$ curl -X POST localhost:8081/v2/products -d '{"name":"Gizmo","price":{"amount":29.99,"currency":"EUR"}}'
{"id":"...","name":"Gizmo","price":{"amount":29.99,"currency":"EUR"}}

# Same operationId (`listProducts`), same underlying data, one function per module -- each
# renders it in its own version's shape. The unprefixed, `default_version`-aliased route
# reaches v1's module, exactly like `/v1/products` does.
$ curl localhost:8081/v1/products    # price: 29.99
$ curl localhost:8081/v2/products    # price: {"amount":29.99,"currency":"EUR"}
$ curl localhost:8081/products       # same as /v1/products

# `discontinueProduct` only exists in v2's specification, so only `products_v2_callback`
# needs to implement it.
$ curl -X DELETE localhost:8081/v2/products/<id>   # 204
$ curl -X DELETE localhost:8081/v1/products/<id>   # 404: v1 never had this operation
```
(Note: unlike `examples/users`, this example doesn't require an `X-API-KEY` header.)

### Configuring more than one version

Instead of a single path, set `spec_path` to a map from version identifier to specification file, and `callback` to a matching map from version identifier to callback module -- each version gets its own controller:
```erl
UsersAPIConf = #{
    spec_path => #{
        <<"v1">> => <<"priv/users_v1.openapi.json">>,
        <<"v2">> => <<"priv/users_v2.openapi.json">>
    },
    callback => #{
        <<"v1">> => users_v1_callback,
        <<"v2">> => users_v2_callback
    },
    port => 8080
}.
```

When `spec_path` and `callback` are a single binary/module (as in [Quickstart](#quickstart)), nothing changes: this is the exact same behaviour `erf` has always had. Versioning is an opt-in, additive feature.

Each version identifier (`<<"v1">>`, `<<"v2">>`, ... any `binary()` works, though it must be safe to use as a single URL path segment, i.e. it must not contain `/`) is prepended as the first path segment of every route defined by that version's specification. With the configuration above, `POST /users` as defined in `users_v1.openapi.json` is served at `POST /v1/users` and reaches `users_v1_callback`, while the same operation as (possibly differently) defined in `users_v2.openapi.json` is served at `POST /v2/users` and reaches `users_v2_callback`.

Each version is parsed, type-checked and validated completely independently -- two versions can define the very same schema name with an incompatible shape (e.g. `v2`'s `User` requiring a field `v1`'s didn't have) with no risk of collision, because `erf` namespaces every generated validation module by version internally.

### One controller per version, or a single shared one

`erf` still expects **one callback module per version, with one function per `operationId`** -- but how much code two versions of the same operation actually share is entirely up to you, not something `erf` decides on your behalf:

- **Different modules per version** (the shape shown above) is the default choice: each version's controller only ever has to deal with its own version's request/response shapes, with no conditionals. Code that both versions genuinely need (e.g. the storage layer in [`examples/products`](examples/products)) lives in its own plain, version-agnostic module that both controllers call into -- ordinary Erlang code reuse, no framework mechanism involved.
```erl
%% v1's controller: translates to/from the shape the shared store keeps internally.
-module(users_v1_callback).
get_user(Request) -> ... users_store:get(...) ...

%% v2's controller: its own module, its own shape, nothing to do with v1.
-module(users_v2_callback).
get_user(Request) -> ... users_store:get(...) ...
```
- **A single shared module** is still fully supported: set `callback` to one module (instead of a map) even when `spec_path` is versioned, and every version's requests reach it. This is convenient when an API's versions are close enough that most operations are identical; `erf` adds a `version` field to `erf:request()` for exactly this case, so the shared function can branch on it where the (small) divergence actually is:
```erl
%% One shared module, small divergence handled inline.
create_user(#{version := <<"v2">>} = Request) ->
    %% v2-specific behaviour
    ...;
create_user(Request) ->
    %% shared behaviour (also used by v1)
    ...
```

Middlewares are unaffected either way: `preprocess_middlewares` and `postprocess_middlewares` keep receiving/returning `erf:request()`/`erf:response()` as before, and can read the same `version` field if they need to behave differently per version (e.g. to add a deprecation header to responses for a version that is being phased out).

### Migrating an existing, single-spec API without breaking its clients

Turning on versioning changes the URL of every route (a version prefix is added), which would break any client of an API that used to be served unprefixed. To adopt versioning without that disruption, set `default_version` to one of the configured versions: `erf` will then **also** expose that version's routes without the prefix, alongside the prefixed ones.
```erl
UsersAPIConf = #{
    spec_path => #{
        <<"v1">> => <<"priv/users_v1.openapi.json">>,
        <<"v2">> => <<"priv/users_v2.openapi.json">>
    },
    default_version => <<"v1">>,
    callback => #{
        <<"v1">> => users_v1_callback,
        <<"v2">> => users_v2_callback
    },
    port => 8080
}.
```
With this configuration, `POST /users` (the original, unprefixed route) and `POST /v1/users` both reach v1, while `POST /v2/users` reaches v2. Once every client has migrated to the prefixed routes, `default_version` can be dropped in a later `reload_conf/2`.

### Swagger UI

With several versions configured, `swagger_ui => true` serves each version's raw specification under its own path -- `/swagger/v1/spec.json`, `/swagger/v2/spec.json`, etc. -- instead of the single `/swagger/spec.json` route used in single-spec mode. If `default_version` is set, `/swagger/spec.json` is additionally served as an alias for that version's specification. The bundled `/swagger` UI page itself is not currently version-aware (it renders whichever specification `/swagger/spec.json` resolves to, if any).

### A note on hot-reloading a versioned `spec_path`/`callback`

As described in [Hot-configuration reloading](#hot-configuration-reloading), `reload_conf/2` merges the map you pass with the running configuration **one level deep only**: whatever value you provide for a key replaces the previous value for that key entirely, it is not merged recursively. This applies to `spec_path` and `callback` exactly as it applies to every other key. This means adding a new version requires resending the *entire* map for both, including the versions that were already there:
```erl
%% Adds `v3` to a running instance that was serving `v1` and `v2`.
%% Omitting `v1`/`v2` here would remove them, not just leave them untouched.
erf:reload_conf(users, #{
    spec_path => #{
        <<"v1">> => <<"priv/users_v1.openapi.json">>,
        <<"v2">> => <<"priv/users_v2.openapi.json">>,
        <<"v3">> => <<"priv/users_v3.openapi.json">>
    },
    callback => #{
        <<"v1">> => users_v1_callback,
        <<"v2">> => users_v2_callback,
        <<"v3">> => users_v3_callback
    }
}).
```
Retiring a version is the same operation in reverse: resend both maps without that version's key.

## Hot-configuration reloading

The design principles behind `erf` allow its instances to be reconfigured in runtime with no needed downtime. While not every configuration key is updatable once the server is started (e.g., the port), some interesting features of the framework can be updated on-the-fly.

The following type spec corresponds to the runtime configuration of an `erf` instance. At the same time, is the type spec of the second argument for the `erf:reload/2` function.
```erl
%%% erf_conf.erl
-type t() :: #{
    callback => module() | #{erf:version() => module()},
    log_level => logger:level(),
    preprocess_middlewares => [module()],
    postprocess_middlewares => [module()],
    router => erl_syntax:syntaxTree(), % not manually updatable
    router_mod => module(), % not manually updatable
    spec_path => binary() | #{erf:version() => binary()},
    spec_parser => module(),
    default_version => erf:version(),
    static_routes => [erf:static_route()],
    swagger_ui => boolean()
}.
```
> __NOTE:__ the `router` and `router_mod` keys are not updatable as they are automatically computed when new configuration is provided.
>
> __NOTE:__ `reload_conf/2` merges its argument with the running configuration one level deep only, so a map-valued `spec_path` is replaced wholesale, not merged version by version. See [API versioning](#api-versioning) for the practical implications of this.

## Static routes

As shown in [`erf` configuration](#erf-configuration), the server supports routes that serve static files. The type spec for static routes is the following:
```erl
%%% erf.erl
-type static_dir() :: {dir, binary()}.
-type static_file() :: {file, binary()}.
-type static_route() :: {Path :: binary(), Resource :: static_file() | static_dir()}.
```

This feature enables `erf` to serve a [Swagger UI](https://github.com/swagger-api/swagger-ui) version with your API specification. Just set the `swagger_ui` flag to `true` and open your web browser in the server host under the `/swagger` path.

## Validation errors

A request that fails schema validation gets a `400` response with the content type `application/problem+json`, in the format that [RFC 9457](https://www.rfc-editor.org/rfc/rfc9457) defines:

```json
{
  "type": "about:blank",
  "title": "Bad Request",
  "status": 400,
  "detail": "Request body failed schema validation",
  "errors": [
    {
      "in": "body",
      "pointer": "/username",
      "keyword": "minLength",
      "detail": "String length is less than 3"
    }
  ]
}
```

Each entry in `errors` names one failure:

- `in` is the part of the request that failed: `body`, `path`, `query`, `header`, or `cookie`.
- `pointer` is a JSON Pointer ([RFC 6901](https://www.rfc-editor.org/rfc/rfc6901)) to the value that failed. For a parameter, the first segment is the parameter name.
- `keyword` is the JSON Schema keyword that rejected the value, for example `minLength`, `type`, or `required`.
- `detail` explains the constraint.

The response never holds a value that the caller sent. It names constraints from the specification, which the server already publishes through Swagger UI.

## Troubleshooting

Diagnosing the cause of a `400 Bad Request error` for a specific request can become challenging due to the automated generation of the router's source code. To simplify the process of analyzing this generated code, `erf` provides the `get_router/1` function. This function offers the router's source code in binary form, allowing you to conveniently manipulate it using the most suitable handler for your particular use case, whether it's printing the code to a file or using `io` operations.

## Specification constraints

### OAS 3.0
- Path parameters MUST be of type `string`. You can use the `pattern` keyword to refine your type spec.
- Query parameters MAY be of type `string`, `integer`, `number`, or `array`. If the type is `array`, it MUST use `style:form` and `explode:true`.

## Contributing

We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `erf` in the open-source community. Don't forget to check out our [contribution guidelines](CONTRIBUTING.md) to ensure smooth collaboration! :rocket:

## Support

If you need help or have any questions, please don't hesitate to open an issue or contact the maintainers directly.

## License

`erf` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.

### Additional Licenses

This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.

Additionally, it allows for `swagger-ui` hosting, which is licensed under the Apache 2.0 license. For more details, please refer to the associated [LICENSE](priv/swagger-ui/LICENSE) file.
