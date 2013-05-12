-module(k6_bytea).
-vsn("1.0.0").

-export([count/0, new/1, delete/1, size/1, get/3, set/3]).
-on_load(init/0).

%% Test support
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% NIF initialisation and hooks
%% ===================================================================

init() ->
    SoName =
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?MODULE]);
                    false ->
                        filename:join([priv, ?MODULE])
                end;
            Dir ->
                filename:join(Dir, ?MODULE)
        end,
    ok = erlang:load_nif(SoName, 0).

count() ->
    exit(nif_not_loaded).

new(_) ->
    exit(nif_not_loaded).

delete(_) ->
    exit(nif_not_loaded).

size(_) ->
    exit(nif_not_loaded).

get(_, _, _) ->
    exit(nif_not_loaded).

set(_, _, _) ->
    exit(nif_not_loaded).

%% ===================================================================
%% Unit tests
%% ===================================================================
-ifdef(TEST).

allocation_test() ->
    Bytea = k6_bytea:new(256),
    ?assertMatch(<<>>, Bytea),
    ?assertEqual(1, k6_bytea:count()),
    ?assertEqual(256, k6_bytea:size(Bytea)),
    ?assertEqual(ok, k6_bytea:delete(Bytea)),
    ?assertError(badarg, k6_bytea:size(Bytea)),
    ?assertError(badarg, k6_bytea:delete(Bytea)),
    k6_bytea:new(128),
    erlang:garbage_collect(),
    % This seems prone to error.
    ?assertEqual(0, k6_bytea:count()).

usage_test() ->
    Bytea = k6_bytea:new(5),
    ?assertEqual(<<0, 0, 0, 0, 0>>, k6_bytea:get(Bytea, 0, 5)),
    ?assertEqual(ok, k6_bytea:set(Bytea, 2, <<$H, $I>>)),
    ?assertEqual(<<0, $H, $I, 0>>, k6_bytea:get(Bytea, 1, 4)),
    ?assertEqual(ok, k6_bytea:delete(Bytea)),
    ?assertError(badarg, k6_bytea:get(Bytea, 0, 1)),
    ?assertError(badarg, k6_bytea:set(Bytea, 0, <<1>>)).

-endif.
