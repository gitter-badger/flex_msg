-module(of15_hello_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v6.hrl").

-define(MODNAME, flex_msg_v6).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

hello_test_() ->
    [{ "hello with xid 123", fun hello_xid/0 },
     { "hello with not xid and not elements", fun hello/0 }].

hello() ->
    Msg = #ofp_hello{ version = 6,
                      elements = [] },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

hello_xid() ->
    Msg = #ofp_hello{ version = 6,
                      xid = 123,
                      elements = [{versionbitmap, [6]}] },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.
