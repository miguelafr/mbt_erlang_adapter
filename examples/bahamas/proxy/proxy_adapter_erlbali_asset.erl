-module(proxy_adapter_erlbali_asset).

-export([init/3, create/1, find_by_id/1, find_all/0, update/1, delete/1, reset/0]).

-include_lib("software/erl_bali-v6.2/src2/bali.hrl").

%%===============================================================
%% API
%%===============================================================
init(Url, UserName, Password)->
    to_abstract_response(
        bali_server:start_link([Url, UserName, Password])).

create(Asset)->
    handle_response(fun() ->
         % TODO BUG: create_assets does not return an error if the asset already
         % exists, so we find the asset before to force the error.
        case bali_server:asset(get_field(Asset, id)) of
            {error,not_found} ->
                to_abstract_response(
                    bali_server:create_assets([to_asset(Asset)]));
            {error, _} ->
                to_abstract_response(
                    bali_server:create_assets([to_asset(Asset)]));
            _ ->
                to_abstract_response(
                        {error, duplicated_asset})
        end
    end).

find_by_id(AssetId)->
    handle_response(fun() ->
        to_abstract_response(
                bali_server:asset(AssetId))
    end).

find_all()->
    handle_response(fun() ->
        to_abstract_response(
                bali_server:assets())
    end).

update(Asset)->
    handle_response(fun() ->
         % TODO BUG: delete_asset does not return an error if the asset does
         % not exist, so we find the asset before to force the error.
        case bali_server:asset(get_field(Asset, id)) of
            {error,not_found} ->
                to_abstract_response(
                        {error,not_found});
            _ ->
                to_abstract_response(
                        bali_server:update_assets([to_asset(Asset)]))
        end
    end).

delete(AssetId)->
    handle_response(fun() ->
         % TODO BUG: delete_asset does not return an error if the asset does
         % not exist, so we find the asset before to force the error.
        case bali_server:asset(AssetId) of
            {error,not_found} ->
                to_abstract_response(
                        {error,not_found});
            _ ->
                to_abstract_response(
                        bali_server:delete_asset(AssetId))
        end
    end).

reset()->
    handle_response(fun() ->
        to_abstract_response(
                bali_server:stop())
    end).

%%---------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------
handle_response(F)->
    try
        F()
    catch
        exit:{noproc,{gen_server,call,[bali_server | _]}} ->
            {error, not_started}
    end.

to_asset(Asset) ->
    #bali_asset {
	    id = get_field(Asset, id),
	    creation_date = get_field(Asset, creation_date),
	    title = get_field(Asset, title),
	    date = get_field(Asset, date),
	    summary = get_field(Asset, summary),
	    rating = get_field(Asset, rating),
	    genre = get_field(Asset, genre),
	    run_time = get_field(Asset, run_time),
	    content = get_field(Asset, content),
	    still_image = get_field(Asset, still_image),
	    collections = get_field(Asset, collections)
    }.

to_abstract_response(ok)->
    ok;
to_abstract_response({ok, Pid}) when is_pid(Pid) ->
    ok;
to_abstract_response({error, econnrefused})->
    {error, connection_error};
to_abstract_response({error, session_cookie_not_found})->
    {error, authentication_error};
to_abstract_response({error,{already_started, Pid}}) when is_pid(Pid) ->
    {error, already_started};
to_abstract_response({error, Error}) ->
    {error, Error};
to_abstract_response({ok, Assets}) when is_list(Assets)->
    to_abstract_response(Assets);
to_abstract_response({ok, Asset}) when is_tuple(Asset) ->
    to_abstract_response(Asset);
to_abstract_response([])->
    [];
to_abstract_response(Assets) when is_list(Assets)->
    lists:map(fun(Asset)->
        to_abstract_response(Asset)
    end, Assets);
to_abstract_response(Asset) when is_tuple(Asset) ->
    [
	    {id, Asset#bali_asset.id},
	    {creation_date, Asset#bali_asset.creation_date},
	    {title, Asset#bali_asset.title},
	    {date, Asset#bali_asset.date},
	    {summary, Asset#bali_asset.summary},
	    {rating, Asset#bali_asset.rating},
	    {genre, Asset#bali_asset.genre},
	    {run_time, Asset#bali_asset.run_time},
	    {content, Asset#bali_asset.content},
	    {still_image, Asset#bali_asset.still_image},
	    {collections, Asset#bali_asset.collections}
    ].

get_field(Asset, Field) ->
    case lists:keysearch(Field, 1, Asset) of
        {value, {Field, Value}} ->
            Value;
        false ->
            undefined
    end.
