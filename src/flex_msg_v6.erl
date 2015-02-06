-module(flex_msg_v6).

-export([encode/1, decode/1]).

-include("ofp_v6.hrl").

encode(Message) ->
    try
        flex_msg_v6_encode:do(Message)
    catch
        _:Exception ->
            { error, Exception }
    end.

decode(Binary) ->
    try
        flex_msg_v6_decode:do(Binary)
    catch
        _:Exception ->
            { error, Exception }
    end.
