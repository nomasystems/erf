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
-module(erf_oas_3_0_SUITE).

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

    {ok, PetstoreAPI} = erf_parser:parse(PetstoreOAS, erf_oas_3_0),
    ?assertMatch(
        #{
            name := <<"Swagger Petstore">>,
            version := <<"1.0.0">>,
            schemas := #{
                <<"petstore_list_pets_limit">> := #{
                    <<"type">> := <<"integer">>,
                    <<"maximum">> := 100
                },
                <<"petstore_list_pets_request_body">> := undefined,
                <<"petstore_list_pets_response_body_200">> := #{
                    <<"anyOf">> := [#{<<"$ref">> := <<"petstore_Pets">>}]
                },
                <<"petstore_list_pets_response_body_default">> := #{
                    <<"anyOf">> := [#{<<"$ref">> := <<"petstore_Error">>}]
                },
                <<"petstore_create_pets_request_body">> := undefined,
                <<"petstore_create_pets_response_body_201">> := undefined,
                <<"petstore_create_pets_response_body_default">> := #{
                    <<"anyOf">> := [#{<<"$ref">> := <<"petstore_Error">>}]
                },
                <<"petstore_show_pet_by_id_pet_id">> := #{
                    <<"type">> := <<"string">>,
                    <<"nullable">> := false
                },
                <<"petstore_show_pet_by_id_request_body">> := undefined,
                <<"petstore_show_pet_by_id_response_body_200">> := #{
                    <<"anyOf">> := [#{<<"$ref">> := <<"petstore_Pet">>}]
                },
                <<"petstore_show_pet_by_id_response_body_default">> := #{
                    <<"anyOf">> := [#{<<"$ref">> := <<"petstore_Error">>}]
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
                                    type := query
                                }
                            ],
                            request_body := <<"petstore_list_pets_request_body">>,
                            responses := #{
                                200 := <<"petstore_list_pets_response_body_200">>,
                                '*' := <<"petstore_list_pets_response_body_default">>
                            }
                        },
                        #{
                            id := <<"create_pets">>,
                            method := post,
                            parameters := [],
                            request_body := <<"petstore_create_pets_request_body">>,
                            responses := #{
                                201 := <<"petstore_create_pets_response_body_201">>,
                                '*' := <<"petstore_create_pets_response_body_default">>
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
                            request_body := <<"petstore_show_pet_by_id_request_body">>,
                            responses := #{
                                200 := <<"petstore_show_pet_by_id_response_body_200">>,
                                '*' := <<"petstore_show_pet_by_id_response_body_default">>
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

    {ok, WithRefsAPI} = erf_parser:parse(WithRefsOAS, erf_oas_3_0),
    ?assertMatch(
        #{
            name := <<"With refs">>,
            version := <<"1.0.0">>,
            schemas := #{
                <<"common_oas_3_0_spec_enabled">> := #{
                    <<"type">> := <<"boolean">>
                },
                <<"common_oas_3_0_spec_version">> := #{
                    <<"type">> := <<"string">>,
                    <<"pattern">> := <<"^[0-9]+$">>,
                    <<"nullable">> := false
                },
                <<"with_refs_oas_3_0_spec_delete_foo_request_body">> := undefined,
                <<"with_refs_oas_3_0_spec_delete_foo_response_body_204">> := #{
                    <<"$ref">> := <<"common_oas_3_0_spec_NoContent">>
                },
                <<"with_refs_oas_3_0_spec_delete_foo_response_body_404">> := #{
                    <<"$ref">> := <<"common_oas_3_0_spec_NotFound">>
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
        Invalid, erf_oas_3_0
    ),

    ok.
