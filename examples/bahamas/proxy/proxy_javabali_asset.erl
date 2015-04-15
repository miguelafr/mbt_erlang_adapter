-module(proxy_javabali_asset).

-export([init/3, create/1, find_by_id/1, find_all/0, update/1, delete/1, reset/0]).

-define(ASSET_TYPE, {list, {
        {tuple, {atom, string}},
        {tuple, {atom, string}}, 
        {tuple, {atom, string}}, 
        {tuple, {atom, string}}, 
        {tuple, {atom, string}}, 
        {tuple, {atom, string}}, 
        {tuple, {atom, string}},
        {tuple, {atom, string}}, 
        {tuple, {atom, string}},
        {tuple, {atom, binary}},
        {tuple, {atom, string}}
    }}).

%%===============================================================
%% API
%%===============================================================
init(Url, UserName, Password) ->
    proxy_java:call(init, [
            {
                {type, string},
                {value, Url}
            },
            {
                {type, string},
                {value, UserName}
            },
            {
                {type, string},
                {value, Password}
            }
        ]
    ).

create(Asset)->
    proxy_java:call(create, [
            {
                {type, ?ASSET_TYPE},
                {value, Asset}
            }
        ]
    ).    

find_by_id(AssetId) ->
    proxy_java:call(find_by_id, [
            {
                {type, string},
                {value, AssetId}
            }
        ]
    ). 

find_all() ->
    proxy_java:call(find_all, []). 

update(Asset)->
    proxy_java:call(update, [
            {
                {type, ?ASSET_TYPE},
                {value, Asset}
            }
        ]
    ).

delete(AssetId) ->
    proxy_java:call(delete, [
            {
                {type, string},
                {value, AssetId}
            }
        ]
    ).

reset() ->
    proxy_java:call(reset,[]).

