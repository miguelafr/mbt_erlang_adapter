-module(proxy_java).

-export([call/2]).

call(Operation, Params)->
    {testservice, 'testnode@localhost'} !
        {self(), {Operation, Params}},
    receive
        Any ->
            Any
        after
            2000 ->
                throw({error, timeout})
    end.
