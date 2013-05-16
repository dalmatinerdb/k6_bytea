-module(k6_bytea).
-vsn("1.1.1").

-export([count/0, new/1, delete/1, size/1, get/3, set/3, from_binary/1, to_binary/1]).
-on_load(init/0).

-opaque bytea() :: bytea_opaque_resource_type.
%% The byte array resource handle.
-export_type([bytea/0]).

%% Test support
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% NIF initialisation and hooks
%% ===================================================================

%% @private @doc Finds the NIF shared object and attempts to load it.
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

%% @doc Returns the number of currently allocated byte arrays.
-spec count() -> integer().
count() ->
    exit(nif_not_loaded).

%% @doc Creates a new byte array and returns it.
-spec new(Size::integer()) -> bytea().
new(_) ->
    exit(nif_not_loaded).

%% @doc Frees a byte array immediately.  It is no longer valid for use by this
%% module; any attempts will result in `badarg'.
-spec delete(Bytea::bytea()) -> ok.
delete(_) ->
    exit(nif_not_loaded).

%% @doc Returns the size of the byte array in bytes.
-spec size(Bytea::bytea()) -> integer().
size(_) ->
    exit(nif_not_loaded).

%% @doc Gets `Len' bytes from the byte array, starting at `From'.  If the slice
%% specified exceeds any boundaries, `badarg' results.
-spec get(Bytea::bytea(), From::integer(), Len::integer()) -> binary().
get(_, _, _) ->
    exit(nif_not_loaded).

%% @doc Replaces the substring of byte array, starting at `From', with `Value'.
%% If the slice so specified exceeds boundaries of the byte array, `badarg'
%% results.
-spec set(Bytea::bytea(), From::integer(), Value::binary()) -> ok.
set(_, _, _) ->
    exit(nif_not_loaded).

%% @doc Creates a new byte array from the given binary string.
-spec from_binary(Binary::binary()) -> bytea().
from_binary(Binary) ->
    Size = erlang:size(Binary),
    Bytea = ?MODULE:new(Size),
    ?MODULE:set(Bytea, 0, Binary),
    Bytea.

%% @doc Gets the entire contents of the byte array as a binary string.
-spec to_binary(Bytea::bytea()) -> binary().
to_binary(Bytea) ->
    Size = ?MODULE:size(Bytea),
    ?MODULE:get(Bytea, 0, Size).

%% ===================================================================
%% Unit tests
%% ===================================================================
-ifdef(TEST).

allocation_test() ->
    Bytea = ?MODULE:new(256),
    ?assertMatch(<<>>, Bytea),
    ?assertEqual(1, ?MODULE:count()),
    ?assertEqual(256, ?MODULE:size(Bytea)),
    ?assertEqual(ok, ?MODULE:delete(Bytea)),
    ?assertError(badarg, ?MODULE:size(Bytea)),
    ?assertError(badarg, ?MODULE:delete(Bytea)),
    ?MODULE:new(128),
    erlang:garbage_collect(),
    % This seems prone to error.
    ?assertEqual(0, ?MODULE:count()).

usage_test() ->
    Bytea = ?MODULE:new(5),
    ?assertEqual(<<0, 0, 0, 0, 0>>, ?MODULE:get(Bytea, 0, 5)),
    ?assertEqual(ok, ?MODULE:set(Bytea, 2, <<$H, $I>>)),
    ?assertEqual(<<0, $H, $I, 0>>, ?MODULE:get(Bytea, 1, 4)),
    ?assertEqual(ok, ?MODULE:delete(Bytea)),
    ?assertError(badarg, ?MODULE:get(Bytea, 0, 1)),
    ?assertError(badarg, ?MODULE:set(Bytea, 0, <<1>>)).

-endif.
