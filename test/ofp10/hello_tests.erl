-module(hello_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

hello_test_() ->
    [{ "hello with xid 123", fun hello_xid/0 },
     { "hello with not xid", fun hello/0 }].

hello() ->
    Msg = #ofp_header{ version = 1,
                       type = hello,
                       body = #ofp_hello{} },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

hello_xid() ->
    Msg = #ofp_header{ version = 1,
                       type = hello,
                       xid = 123,
                       body = #ofp_hello{} },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
