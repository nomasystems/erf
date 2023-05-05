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

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        parse
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
parse(_Conf) ->
    OAS = unicode:characters_to_binary(code:priv_dir(erf) ++ "/oas/3.0/examples/petstore.json"),
    Invalid = unicode:characters_to_binary(
        code:lib_dir(erf, test) ++ "/fixtures/invalid_oas_3_0_spec.json"
    ),

    {ok, #{
        name := <<"swagger_petstore">>,
        version := <<"1.0.0">>,
        schemas := #{
            <<"list_pets_limit">> := #{
                <<"type">> := <<"integer">>,
                <<"maximum">> := 100
            },
            <<"list_pets_request_body">> := undefined,
            <<"list_pets_response_body">> := #{
                <<"anyOf">> := [
                    #{
                        <<"anyOf">> := [
                            #{
                                <<"type">> := <<"array">>,
                                <<"items">> := #{
                                    <<"type">> := <<"object">>,
                                    <<"required">> := [<<"id">>, <<"name">>],
                                    <<"properties">> := #{
                                        <<"id">> := #{
                                            <<"type">> := <<"integer">>
                                        },
                                        <<"name">> := #{
                                            <<"type">> := <<"string">>
                                        },
                                        <<"tag">> := #{
                                            <<"type">> := <<"string">>
                                        }
                                    }
                                }
                            }
                        ]
                    },
                    #{
                        <<"anyOf">> := [
                            #{
                                <<"type">> := <<"object">>,
                                <<"required">> := [<<"code">>, <<"message">>],
                                <<"properties">> := #{
                                    <<"code">> := #{
                                        <<"type">> := <<"integer">>
                                    },
                                    <<"message">> := #{
                                        <<"type">> := <<"string">>
                                    }
                                }
                            }
                        ]
                    }
                ]
            },
            <<"create_pets_request_body">> := undefined,
            <<"create_pets_response_body">> := #{
                <<"anyOf">> := [
                    undefined,
                    #{
                        <<"anyOf">> := [
                            #{
                                <<"type">> := <<"object">>,
                                <<"required">> := [<<"code">>, <<"message">>],
                                <<"properties">> := #{
                                    <<"code">> := #{
                                        <<"type">> := <<"integer">>
                                    },
                                    <<"message">> := #{
                                        <<"type">> := <<"string">>
                                    }
                                }
                            }
                        ]
                    }
                ]
            },
            <<"show_pet_by_id_pet_id">> := #{
                <<"type">> := <<"string">>,
                <<"nullable">> := false
            },
            <<"show_pet_by_id_request_body">> := undefined,
            <<"show_pet_by_id_response_body">> := #{
                <<"anyOf">> := [
                    #{
                        <<"anyOf">> := [
                            #{
                                <<"type">> := <<"object">>,
                                <<"required">> := [<<"id">>, <<"name">>],
                                <<"properties">> := #{
                                    <<"id">> := #{
                                        <<"type">> := <<"integer">>
                                    },
                                    <<"name">> := #{
                                        <<"type">> := <<"string">>
                                    },
                                    <<"tag">> := #{
                                        <<"type">> := <<"string">>
                                    }
                                }
                            }
                        ]
                    },
                    #{
                        <<"anyOf">> := [
                            #{
                                <<"type">> := <<"object">>,
                                <<"required">> := [<<"code">>, <<"message">>],
                                <<"properties">> := #{
                                    <<"code">> := #{
                                        <<"type">> := <<"integer">>
                                    },
                                    <<"message">> := #{
                                        <<"type">> := <<"string">>
                                    }
                                }
                            }
                        ]
                    }
                ]
            }
        },
        % TODO: add expected endpoints when parsing feature is implemented
        endpoints := []
    }} = erf_parser:parse(OAS),

    {error, {invalid_spec, <<"Invalid OpenAPI Specification 3.0">>}} = erf_parser:parse(Invalid),

    ok.
