%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(erf_parser_oas_3_0_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        {group, parse}
    ].

groups() ->
    [
        {parse, [parallel], [
            petstore,
            with_refs,
            invalid
        ]}
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
petstore(_Conf) ->
    PetstoreOAS = unicode:characters_to_binary(
        code:priv_dir(erf) ++ "/oas/3.0/examples/petstore.json"
    ),

    {ok, PetstoreAPI} = erf_parser:parse(PetstoreOAS, erf_parser_oas_3_0),
    ?assertMatch(
        #{
            name := <<"Swagger Petstore">>,
            version := <<"1.0.0">>,
            schemas := #{
                <<"petstore_error">> := #{
                    properties :=
                        #{
                            <<"code">> := #{type := integer},
                            <<"message">> := #{type := string}
                        },
                    required := [<<"code">>, <<"message">>],
                    type := object
                },
                <<"petstore_pet">> := #{
                    properties :=
                        #{
                            <<"id">> := #{type := integer},
                            <<"name">> := #{type := string},
                            <<"tag">> := #{type := string}
                        },
                    required := [<<"id">>, <<"name">>],
                    type := object
                },
                <<"petstore_pets">> := #{
                    items := #{ref := <<"petstore_pet">>},
                    max_items := 100,
                    type := array
                },
                <<"petstore_list_pets_limit">> := #{
                    type := integer,
                    maximum := 100
                },
                <<"petstore_list_pets_request_body">> := true,
                <<"petstore_list_pets_response_body_200">> := #{
                    any_of := [#{ref := <<"petstore_pets">>}]
                },
                <<"petstore_list_pets_response_body_default">> := #{
                    any_of := [#{ref := <<"petstore_error">>}]
                },
                <<"petstore_create_pets_request_body">> := true,
                <<"petstore_create_pets_response_body_201">> := true,
                <<"petstore_create_pets_response_body_default">> := #{
                    any_of := [#{ref := <<"petstore_error">>}]
                },
                <<"petstore_show_pet_by_id_pet_id">> := #{
                    type := string
                },
                <<"petstore_show_pet_by_id_request_body">> := true,
                <<"petstore_show_pet_by_id_response_body_200">> := #{
                    any_of := [#{ref := <<"petstore_pet">>}]
                },
                <<"petstore_show_pet_by_id_response_body_default">> := #{
                    any_of := [#{ref := <<"petstore_error">>}]
                }
            },
            endpoints := [
                #{
                    path := <<"/pets">>,
                    parameters := [],
                    operations := [
                        #{
                            id := <<"list_pets">>,
                            method := get,
                            parameters := [
                                #{
                                    ref := <<"petstore_list_pets_limit">>,
                                    name := <<"limit">>,
                                    type := query,
                                    required := false
                                }
                            ],
                            request := #{
                                body := #{
                                    ref := <<"petstore_list_pets_request_body">>,
                                    required := false
                                }
                            },
                            responses := #{
                                200 := #{
                                    body := #{
                                        ref := <<"petstore_list_pets_response_body_200">>,
                                        required := false
                                    }
                                },
                                '*' := #{
                                    body := #{
                                        ref := <<"petstore_list_pets_response_body_default">>,
                                        required := false
                                    }
                                }
                            }
                        },
                        #{
                            id := <<"create_pets">>,
                            method := post,
                            parameters := [],
                            request := #{
                                body := #{
                                    ref := <<"petstore_create_pets_request_body">>,
                                    required := false
                                }
                            },
                            responses := #{
                                201 := #{
                                    body := #{
                                        ref := <<"petstore_create_pets_response_body_201">>,
                                        required := false
                                    }
                                },
                                '*' := #{
                                    body := #{
                                        ref := <<"petstore_create_pets_response_body_default">>,
                                        required := false
                                    }
                                }
                            }
                        }
                    ]
                },
                #{
                    path := <<"/pets/{petId}">>,
                    parameters := [],
                    operations := [
                        #{
                            id := <<"show_pet_by_id">>,
                            method := get,
                            parameters := [
                                #{
                                    ref := <<"petstore_show_pet_by_id_pet_id">>,
                                    name := <<"petId">>,
                                    type := path
                                }
                            ],
                            request := #{
                                body := #{
                                    ref := <<"petstore_show_pet_by_id_request_body">>,
                                    required := false
                                }
                            },
                            responses := #{
                                200 := #{
                                    body := #{
                                        ref := <<"petstore_show_pet_by_id_response_body_200">>,
                                        required := false
                                    }
                                },
                                '*' := #{
                                    body := #{
                                        ref := <<"petstore_show_pet_by_id_response_body_default">>,
                                        required := false
                                    }
                                }
                            }
                        }
                    ]
                }
            ]
        },
        PetstoreAPI
    ),

    ok.

with_refs(_Conf) ->
    WithRefsOAS = unicode:characters_to_binary(
        code:lib_dir(erf, test) ++ "/fixtures/with_refs_oas_3_0_spec.json"
    ),

    {ok, WithRefsAPI} = erf_parser:parse(WithRefsOAS, erf_parser_oas_3_0),
    ?assertMatch(
        #{
            name := <<"With refs">>,
            version := <<"1.0.0">>,
            schemas := #{
                <<"common_oas_3_0_spec_enabled">> := #{
                    type := boolean
                },
                <<"common_oas_3_0_spec_version">> := #{
                    type := string,
                    pattern := <<"^[0-9]+$">>
                },
                <<"with_refs_oas_3_0_spec_delete_foo_request_body">> := true,
                <<"with_refs_oas_3_0_spec_delete_foo_response_body_204">> := true,
                <<"with_refs_oas_3_0_spec_delete_foo_response_body_404">> := #{
                    any_of := [
                        #{ref := <<"common_oas_3_0_spec_error">>}
                    ]
                }
            }
        },
        WithRefsAPI
    ),

    ok.

invalid(_Conf) ->
    Invalid = unicode:characters_to_binary(
        code:lib_dir(erf, test) ++ "/fixtures/invalid_oas_3_0_spec.json"
    ),

    {error, {invalid_spec, <<"Invalid OpenAPI Specification 3.0">>}} = erf_parser:parse(
        Invalid, erf_parser_oas_3_0
    ),

    ok.
