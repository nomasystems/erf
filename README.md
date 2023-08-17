# erf - A design-first Erlang REST Framework
[![erf ci](https://github.com/nomasystems/erf/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/erf/actions/workflows/ci.yml)
[![erf docs](https://github.com/nomasystems/erf/actions/workflows/docs.yml/badge.svg)](https://nomasystems.github.io/erf)

`erf` is a design-first Erlang REST framework. It provides an interface to spawn specification-driven HTTP servers with several automated features that aim to ease the development, operation and maintenance of design-first RESTful services. Its HTTP protocol features are provided as a wrapper of the [elli](https://github.com/elli-lib/elli) HTTP 1.1 server.

## What is design-first?

When following a code-first approach to develop APIs, the interface is produced as a result of the implementation and, therefore, client-side code, integration tests and other parts of the system that depend on the API behaviour, need to wait until the server-side work is done.

Design-first is an approach to API development that prioritises the design of the API before its implementation. The explicit contract produced in this design, that should be the result of an agreement between the stakeholders of the API, aims to reduce bottlenecks in the development process.

## How does `erf` help developing design-first RESTful services?

`erf` is an HTTP server framework that, taking an API design in the form of an specification file and a callback module as input, starts a server and dynamically generates code to efficiently type-check and route requests to callback functions. Its main goal is to provide a tool to REST API development in Erlang that reduces the development time by automating the implementation of boilerplate code that can be inferred from the API specification.

## Quickstart

1. Design your API using OpenAPI 3.0. For example: [users.json](examples/users/users.json).

2. Add `erf` as a dependency in your `rebar3` project.
```erl
{deps, [
    {erf, {git, "git@github.com:nomasystems/erf.git", {branch, "main"}}}
]}.
```

3. Implement a callback module for your API. A hypothetical example for [users.json](examples/users/users.json) would be [users_callback.erl](examples/users/users_callback.erl).
```erl
%% An <code>erf</code> callback for the users REST API.
-module(users_callback).

%%% EXTERNAL EXPORTS
-export([
    create_user/4,
    get_user/4,
    delete_user/4
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
create_user(_PathParameters, _QueryParameters, _Headers, Body) ->
    Id = base64:encode(crypto:strong_rand_bytes(16)),
    ets:insert(users, {Id, Body#{<<"id">> => Id}}),
    {201, [], Body#{<<"id">> => Id}}.

get_user(PathParameters, _QueryParameters, _Headers, _Body) ->
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

delete_user(PathParameters, _QueryParameters, _Headers, _Body) ->
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
        spec_path => <<"doc/openapi/users.openapi.json">>,
        callback => users_callback,
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

5. Start requesting your service.
```sh
$ curl -vvv 'localhost:8080/users' -H 'Content-Type: application/json' -d '{"username": "foo", "password": "fooBAR123"}'
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
{"id":"b7R7bJSbaTmoiWwecy2IwA==","password":"fooBAR123","username":"foo"}
```

## `erf` configuration

`erf`'s main entry point (i.e., the `start_link/1` function) receives an API specification, a callback module and a set of optional values that enable its configuration.

The configuration is provided as map with the following type spec:
```erl
%%% erf.erl
-type conf() :: #{
    spec_path := binary(),
    callback := module(),
    port => inet:port_number(),
    name => atom(),
    spec_parser => module(),
    preprocess_middlewares => [module()],
    postprocess_middlewares => [module()],
    ssl => boolean(),
    certfile => binary(),
    keyfile => binary(),
    static_routes => [static_route()],
    swagger_ui => boolean(),
    min_acceptors => pos_integer(), % defaults to 20
    accept_timeout => pos_integer(), % defaults to 10000
    request_timeout => pos_integer(), % defaults to 60000
    header_timeout => pos_integer(), % defaults to 10000
    body_timeout => pos_integer(), % defaults to 30000
    max_body_size => pos_integer(), % defaults to 1024000
    log_level => logger:level() % defaults to error
}.
```

A detailed description of each parameter can be found in the following list:
- `spec_path` : Path to API specification file.
- `callback`: Name of the callback module.
- `port`: Port the server will listen to. Defaults to `8080`.
- `name`: Name under which the server is registered. Defaults to `erf`.
- `spec_parser`: Name of the specification parser module. Defaults to `erf_oas_3_0`.
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
- `log_level`: Severity asociated to logged messages. Defaults to `error`.

## Callback modules

`erf` dynamically generates a router that type check the received requests against the API specification. If the request passes the validation, it is deconstructed and passed to the callback module. But, how does the callback module must look like?

The router expects your callback module to export one function per operation defined in your API specification. It also expects each operation to include an `operationId` that, after being transformed to _snake_case_, will identify the function that is going to be called. Regarding the expected arguments in those functions, `erf` will provide 4 arguments that include the variable data of the request (i.e., that data that cannot be inferred just from the `operationId`):
- `PathParameters :: [{Name :: binary(), Value :: binary()}]`
- `QueryParameters :: [{Name :: binary(), Value :: binary()}]`
- `Headers :: [{Name :: binary(), Value :: binary()}]`
- `Body :: njson:t()`
> [`njson`](https://github.com/nomasystems/njson) is the library used in `erf` to deserialize JSON values to Erlang terms.

An example of an API specification and a supported callback can be seen in [Quickstart](#quickstart).

## Hot-configuration reloading

The design principles behind `erf` allow its instances to be reconfigured in runtime with no needed downtime. While not every configuration key is updatable once the server is started (e.g., the port), some interesting features of the framework can be updated on-the-fly.

The following type spec corresponds to the runtime configuration of an `erf` instance. At the same time, is the type spec of the second argument for the `erf:reload/2` function.
```erl
%%% erf_conf.erl
-type t() :: #{
    callback => module(),
    log_level => logger:level(),
    preprocess_middlewares => [module()],
    postprocess_middlewares => [module()],
    router => erl_syntax:syntaxTree(), % not manually updatable
    router_mod => module(), % not manually updatable
    spec_path => binary(),
    spec_parser => module(),
    static_routes => [erf:static_route()],
    swagger_ui => boolean()
}.
```
> __NOTE:__ the `router` and `router_mod` keys are not updatable as they are automatically computed when new configuration is provided.

## Static routes

As shown in [`erf` configuration](#erf-configuration), the server supports routes that serve static files. The type spec for static routes is the following:
```erl
%%% erf.erl
-type static_dir() :: {dir, binary()}.
-type static_file() :: {file, binary()}.
-type static_route() :: {Path :: binary(), Resource :: static_file() | static_dir()}.
```

This feature enables `erf` to serve a [Swagger UI](https://github.com/swagger-api/swagger-ui) version with your API specification. Just set the `swagger_ui` flag to `true` and open your web browser in the server host under the `/swagger` path.

## Specification constraints

### OAS 3.0
- Path parameters MUST be of type `string`. You can use the `pattern` keyword to refine your type spec.

## Contributing

We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `erf` in the open-source community. Don't forget to check out our [contribution guidelines](CONTRIBUTING.md) to ensure smooth collaboration! :rocket:

## Support

If you need help or have any questions, please don't hesitate to open an issue or contact the maintainers directly.

## License

`erf` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.

### Additional Licenses

This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.

Additionally, it allows for `swagger-ui` hosting, which is licensed under the Apache 2.0 license. For more details, please refer to the associated [LICENSE](priv/swagger-ui/LICENSE) file.
