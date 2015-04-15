-module(dummy_asset).

-behaviour(gen_server).

-include_lib("software/erl_bali-v6.2/src2/bali.hrl").

%% API
-export([start_link/3, stop/0]).
-export([create/1, find_by_id/1, find_all/0, update/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {valid_url = false,
                valid_login = false}).

-define(URL, "http://localhost:8888").
-define(USER_NAME, "user").
-define(PASSWORD, "password").
-define(DETS_ASSETS, assets).
-define(DETS_ASSETS_KEY, assets).

%%===============================================================
%% API
%%===============================================================
%%---------------------------------------------------------------
%% start_link
%%---------------------------------------------------------------
start_link(Url, UserName, Password) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Url, UserName, Password],
        []).

%%---------------------------------------------------------------
%% create
%%---------------------------------------------------------------
create(Asset)->
    gen_server:call(?MODULE, {create, Asset}).

%%---------------------------------------------------------------
%% find_by_id
%%---------------------------------------------------------------
find_by_id(AssetId)->
    gen_server:call(?MODULE, {find_by_id, AssetId}).

%%---------------------------------------------------------------
%% find_all
%%---------------------------------------------------------------
find_all()->
    gen_server:call(?MODULE, find_all).

%%---------------------------------------------------------------
%% update
%%---------------------------------------------------------------
update(Asset)->
    gen_server:call(?MODULE, {update, Asset}).

%%---------------------------------------------------------------
%% delete
%%---------------------------------------------------------------
delete(AssetId)->
    gen_server:call(?MODULE, {delete, AssetId}).

%%---------------------------------------------------------------
%% stop
%%---------------------------------------------------------------
stop() ->
	gen_server:call(?MODULE, stop).

%%===============================================================
%% gen_server callbacks
%%===============================================================
init([?URL, ?USER_NAME, ?PASSWORD]) ->
    init_storage(),
    State = #state {
        valid_url = true,
        valid_login = true
    },
    {ok, State};

init([?URL, _UserName, _Password]) ->
    init_storage(),
    State = #state {
        valid_url = true,
        valid_login = false
    },
    {ok, State};

init([_Url, _UserName, _Password]) ->
    init_storage(),
    State = #state {
        valid_url = false,
        valid_login = false
    },
    {ok, State}.

handle_call({create, Asset}, _From, State) ->
    check_preconditions(State, fun()->
        Assets = get_data_storage(),
        case lists:keysearch(Asset#bali_asset.id, 2, Assets) of
            {value, _Asset} ->
                {reply, {error, duplicated_asset}, State};
            _ ->
                NewAssets = [Asset | Assets],
                update_data_storage(NewAssets),
                {reply, ok, State}
        end
    end);

handle_call({find_by_id, AssetId}, _From, State) ->
    check_preconditions(State, fun()->
        Assets = get_data_storage(),
        case lists:keysearch(AssetId, 2, Assets) of
            {value, Asset} ->
                {reply, Asset, State};
            _ ->
                {reply, {error, not_found}, State}
        end
    end);

handle_call(find_all, _From, State) ->
    check_preconditions(State, fun()->
        Assets = get_data_storage(),
        {reply, Assets, State}
    end);

handle_call({update, Asset}, _From, State) ->
    check_preconditions(State, fun()->
        Assets = get_data_storage(),
        case lists:keysearch(Asset#bali_asset.id, 2, Assets) of
            {value, OldAsset} ->
                NewAsset = Asset#bali_asset {
                    creation_date = OldAsset#bali_asset.creation_date
                },
                NewAssets = [NewAsset | lists:delete(OldAsset, Assets)],
                update_data_storage(NewAssets),
                {reply, ok, State};
            _ ->
                {reply, {error, not_found}, State}
        end
    end);

handle_call({delete, AssetId}, _From, State) ->
    check_preconditions(State, fun()->
        Assets = get_data_storage(),
        case lists:keysearch(AssetId, 2, Assets) of
            {value, _Asset} ->
                NewAssets = lists:keydelete(AssetId, 2, Assets),
                update_data_storage(NewAssets),
                {reply, ok, State};
            _ ->
                {reply, {error, not_found}, State}
        end
    end);

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
                              
handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
    remove_storage(),
	ok.

%%===============================================================
%% Internal functions
%%===============================================================
init_storage() ->
    dets:close(?DETS_ASSETS),
    dets:open_file(?DETS_ASSETS, [{type, bag}]),
    case dets:lookup(?DETS_ASSETS, ?DETS_ASSETS_KEY) of
        [] ->
            dets:insert(?DETS_ASSETS, {?DETS_ASSETS_KEY, []});
        _ ->
            ok
    end.

get_data_storage() ->
    [{?DETS_ASSETS_KEY, Assets}] = dets:lookup(?DETS_ASSETS, ?DETS_ASSETS_KEY),
    Assets.

update_data_storage(Assets)->
    dets:delete(?DETS_ASSETS, ?DETS_ASSETS_KEY),
    dets:insert(?DETS_ASSETS, {?DETS_ASSETS_KEY, Assets}).

remove_storage()->
    dets:close(?DETS_ASSETS).

check_preconditions(State, GenResponse) ->
    case {State#state.valid_url, State#state.valid_login} of
        {true, true} ->
            GenResponse();

        {false, _} ->
            {reply, {error, connection_error}, State};

        {true, false} ->
            {reply, {error, authentication_error}, State}

    end.
