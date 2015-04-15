-module(test_asset_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

-define(BAHAMAS_PROXY, proxy_javabali_asset).

-define(VALID_URL, "http://localhost:8888").
-define(VALID_USER_NAME, "user").
-define(VALID_PASSWORD, "password").

%% API
-export([prop_assets/0]).

%% Internal functions
-export([init/3, create/1, find_by_id/1, find_all/0, update/1, delete/1, reset/0]).

%% eqc callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
        next_state/3]).

-compile(export_all).

-record(state, {started = false,
                valid_url = false,
                valid_login = false,
                assets = []
                }).

%%===============================================================
%% API
%%===============================================================
prop_assets() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin 
            {H, S, Res} = run_commands(?MODULE, Cmds),
            clear_assets(),
            ?WHENFAIL(io:format("H: ~p\nS: ~p\nRes: ~p\n", [H, S, Res]), Res == ok)
            %Res==ok
        end).

%%===============================================================
%% eqc callbacks
%%===============================================================
%%---------------------------------------------------------------
%% initial_state
%%---------------------------------------------------------------
initial_state()->
    #state {
        started = false,
        valid_url = false,
        valid_login = false,
        assets = []
    }.

%%---------------------------------------------------------------
%% command
%%---------------------------------------------------------------
%% TODO Add transition for controlling timeouts, see paper of LiveScheduler
command(S) ->
    oneof(
        %login
        [{call, ?MODULE, init, [gen_url(), gen_user_name(), gen_password()]}]

        ++

        %create
        [{call, ?MODULE, create,
            [oneof([oneof(S#state.assets) || S#state.assets /= []] ++
                   [gen_asset()])
            ]}]
            
        ++
                
        %find_by_id
        [{call, ?MODULE, find_by_id,
            [oneof([?LET(Asset, oneof(S#state.assets), get_asset_field(Asset, id)) ||
                        S#state.assets /= []] ++
                   [gen_valid_id()])
            ]}]
            
        ++

        %find_all
        [{call, ?MODULE, find_all, []}]
            
        ++

        %update
        [{call, ?MODULE, update,
            [oneof([oneof(S#state.assets) || S#state.assets /= []] ++
                   [gen_asset()])
            ]}]
            
        ++

        %delete
        [{call, ?MODULE, delete,
            [oneof([?LET(Asset, oneof(S#state.assets), get_asset_field(Asset, id)) ||
                        S#state.assets /= []] ++
                   [gen_valid_id()])
            ]}]

        ++

        %reset
        [{call, ?MODULE, reset, []}]

    ).

%%---------------------------------------------------------------
%% precondition
%%---------------------------------------------------------------
precondition(_S, _C)->
    true.

%%---------------------------------------------------------------
%% postcondition
%%---------------------------------------------------------------
postcondition(S, {call, ?MODULE, init, [_Url, _UserName, _Password]}, Result)->
    case S#state.started of
        true ->
            Result == {error, already_started};
        _ ->
            Result == ok
    end;

postcondition(S, {call, ?MODULE, create, [Asset]}, {CreateResult, AllAssets})->
    common_postcondition(S, CreateResult, fun()->
        case search_asset(get_asset_field(Asset, id), S#state.assets) of
            {value, _Asset} ->
                CreateResult == {error, duplicated_asset};
            _ ->
                CreateResult == ok andalso lists:member(Asset, AllAssets)
        end
    end);

postcondition(S, {call, ?MODULE, find_by_id, [AssetId]}, Result)->
    common_postcondition(S, Result, fun()->
        case search_asset(AssetId, S#state.assets) of
            {value, Asset} ->
                Result == Asset;
            _ ->
                Result == {error, not_found}
        end
    end);

postcondition(S, {call, ?MODULE, find_all, []}, Result)->
    common_postcondition(S, Result, fun()->
        lists:all(
            fun(Element)->
                lists:member(Element, S#state.assets)
            end,
            Result
        )
        andalso
        lists:all(
            fun(Element)->
                lists:member(Element, Result)
            end,
            S#state.assets
        )
    end);

postcondition(S, {call, ?MODULE, update, [Asset]}, {UpdateResult, AllAssets})->
    common_postcondition(S, UpdateResult, fun()->
        case search_asset(get_asset_field(Asset, id), S#state.assets) of
            {value, OldAsset} ->
                % Creation date can not be updated
                NewAsset = update_asset_field(Asset, creation_date,
                        get_asset_field(OldAsset, creation_date)),
                UpdateResult == ok andalso lists:member(NewAsset, AllAssets);
            _ ->
                UpdateResult == {error, not_found}                
        end
    end);

postcondition(S, {call, ?MODULE, delete, [AssetId]}, {DeleteResult, AllAssetIds})->
    common_postcondition(S, DeleteResult, fun()->
        case search_asset(AssetId, S#state.assets) of
            {value, _Asset} ->
                DeleteResult == ok andalso not lists:member(AssetId, AllAssetIds);
            _ ->
                DeleteResult == {error, not_found}                
        end
    end);

postcondition(S, {call, ?MODULE, reset, []}, Result)->
    case {S#state.started, S#state.valid_url, S#state.valid_login} of
        {false, _, _} ->
            Result == {error, not_started};
        _ ->
            Result == ok
    end;
    
postcondition(_S, _C, _R)->
    false.

%%---------------------------------------------------------------
%% next_state
%%---------------------------------------------------------------
next_state(S, _Result, {call, ?MODULE, init,
        [?VALID_URL, ?VALID_USER_NAME, ?VALID_PASSWORD]})->
    case S#state.started of
        false ->
            S#state {
                started = true,
                valid_url = true,
                valid_login = true
            };
        _ ->
            S
    end;

next_state(S, _Result, {call, ?MODULE, init,
        [?VALID_URL, _UserName, _Password]})->
    case S#state.started of
        false ->
            S#state {
                started = true,
                valid_url = true,
                valid_login = false
            };
        _ ->
            S
    end;

next_state(S, _Result, {call, ?MODULE, init, [_Url, _UserName, _Password]})->
    case S#state.started of
        false ->
            S#state {
                started = true,
                valid_url = false,
                valid_login = false
            };
        _ ->
            S
    end;

next_state(S, _Result, {call, ?MODULE, create, [Asset]})->
    common_next_state(S, fun() ->
        case search_asset(get_asset_field(Asset, id), S#state.assets) of
            {value, _Asset} ->
                S;
            _ ->
                S#state {
                    assets = [Asset | S#state.assets]
                }
        end
    end);

next_state(S, _Result, {call, ?MODULE, find_by_id, [_AssetId]})->
    common_next_state(S, fun() ->
        S
    end);

next_state(S, _Result, {call, ?MODULE, find_all, []})->
    common_next_state(S, fun() ->
        S
    end);

next_state(S, _Result, {call, ?MODULE, update, [Asset]})->
    common_next_state(S, fun() ->
        case search_asset(get_asset_field(Asset, id), S#state.assets) of
            {value, OldAsset} ->
                % Creation date can not be updated
                NewAsset = update_asset_field(Asset, creation_date,
                        get_asset_field(OldAsset, creation_date)),
                S#state {
                    assets = [NewAsset | lists:delete(OldAsset, S#state.assets)]
                };
            _ ->
                S
        end
    end);

next_state(S, _Result, {call, ?MODULE, delete, [AssetId]})->
    common_next_state(S, fun() ->
        case search_asset(AssetId, S#state.assets) of
            {value, Asset} ->
                S#state {
                    assets = lists:delete(Asset, S#state.assets)
                };
            _ ->
                S
        end
    end);

next_state(S, _Result, {call, ?MODULE, reset, []})->
    case S#state.started of
        true ->
            S#state {
                started = false,
                valid_url = false,
                valid_login = false
            };
        _ ->
            S
    end;

next_state(S, _R, _C)->
    S.

%%---------------------------------------------------------------
%% Generators
%%---------------------------------------------------------------
gen_url()->
    %eqc_gen:elements([?VALID_URL, "http://localhost:8887", "http://10.10.10.10:8888"]).
    eqc_gen:elements([?VALID_URL]). %, "http://localhost:8887"]).

gen_user_name()->
    eqc_gen:elements([?VALID_USER_NAME]). %, "not_valid_user"]).

gen_password()->
    eqc_gen:elements([?VALID_PASSWORD]). %, "not_valid_password"]).

gen_valid_id()->
    eqc_gen:non_empty(eqc_gen:list(eqc_gen:elements(
        [eqc_gen:choose(65,90), eqc_gen:choose(97, 122)]
    ))).

gen_asset()->
    [
	    {id, gen_valid_id()},
	    {creation_date, ?LET(X, eqc_gen:choose(
            calendar:datetime_to_gregorian_seconds(
                {{1900,1,1},{0,0,0}}),
            calendar:datetime_to_gregorian_seconds(
                {{2200,1,1},{0,0,0}})),
            erlang:integer_to_list(X))},
	    {title, gen_string()},
	    {date, gen_string()},
	    {summary, gen_string()},
	    {rating, gen_string()},
	    {genre, gen_string()},
	    {run_time, ?LET(T, eqc_gen:nat(),
            format_run_time(T))},
	    {content, []},
	    {still_image, <<>>},
	    {collections, []}
    ].

gen_string()->
    eqc_gen:non_empty(eqc_gen:list(eqc_gen:elements(
        [eqc_gen:choose(65,90), eqc_gen:choose(97, 122)]
    ))).

%%---------------------------------------------------------------
%% Common utilities for callbacks
%%---------------------------------------------------------------
common_next_state(S, CustomNextState)->
    case {S#state.started, S#state.valid_url, S#state.valid_login} of
        {true, true, true} ->
            CustomNextState();
        _ ->
            S
    end.

common_postcondition(S, Result, CustomPostcondition)->
    case {S#state.started, S#state.valid_url, S#state.valid_login} of
        {false, _, _} ->
            Result == {error, not_started};
        {true, true, true} ->
            CustomPostcondition();
        {true, true, false}->
            Result == {error, authentication_error};
        {true, false, _} ->
            Result == {error, connection_error}
    end.

%%---------------------------------------------------------------
%% Asset utilities
%%---------------------------------------------------------------
get_asset_field(Asset, Field) ->
    case lists:keysearch(Field, 1, Asset) of
        {value, {Field, Value}} ->
            Value;
        false ->
            undefined
    end.

update_asset_field(Asset, Field, Value) ->
    lists:keyreplace(Field, 1, Asset, {Field, Value}).

search_asset(_AssetId, []) ->
    false;
search_asset(AssetId, [Asset | MoreAssets]) ->
    case get_asset_field(Asset, id) of
        AssetId ->
            {value, Asset};
        _ ->
          search_asset(AssetId, MoreAssets)
    end.

format_run_time(T) ->
    Seconds = T rem 60,
    Minutes = ((T - Seconds) div 60) rem 60,
    Hours = (T - Seconds - (Minutes * 60)) div 3600,
    erlang:integer_to_list(Hours) ++ ":" ++
        format_field_time(Minutes) ++ ":" ++
        format_field_time(Seconds).

format_field_time(T) ->
    if
        T < 10 ->
            "0" ++ erlang:integer_to_list(T);
        true ->
            erlang:integer_to_list(T)
    end.
    
%%---------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------
init(Url, UserName, Password)->
    ?BAHAMAS_PROXY:init(Url, UserName, Password).

create(Asset)->
    CreateResult = ?BAHAMAS_PROXY:create(Asset),
    AllAssets = case ?BAHAMAS_PROXY:find_all() of
        {error, _Reason} ->
            [];
        Assets ->
            Assets
    end,
    {CreateResult, AllAssets}.

find_by_id(AssetId)->
    ?BAHAMAS_PROXY:find_by_id(AssetId).

find_all()->
    ?BAHAMAS_PROXY:find_all().

update(Asset)->
    UpdateResult = ?BAHAMAS_PROXY:update(Asset),
    AllAssets = case ?BAHAMAS_PROXY:find_all() of
        {error, _Reason} ->
            [];
        Assets ->
            Assets
    end,
    {UpdateResult, AllAssets}.

delete(AssetId)->
    DeleteResult = ?BAHAMAS_PROXY:delete(AssetId),
    AllAssets = case ?BAHAMAS_PROXY:find_all() of
        {error, _Reason} ->
            [];
        Assets ->
            Assets
    end,
    {DeleteResult,
     lists:map(fun(Asset) -> get_asset_field(Asset, id) end, AllAssets)
    }.

reset() ->
    ?BAHAMAS_PROXY:reset().

clear_assets() ->
    ?BAHAMAS_PROXY:reset(),
    ?BAHAMAS_PROXY:init(?VALID_URL, ?VALID_USER_NAME,
            ?VALID_PASSWORD),
    case ?BAHAMAS_PROXY:find_all() of
        {error, _} ->
            ok;
        Assets ->
            lists:map(
                fun(Asset) ->
                    ?BAHAMAS_PROXY:delete(get_asset_field(Asset, id))
                end,
                Assets
            )
    end,
    ?BAHAMAS_PROXY:reset().
