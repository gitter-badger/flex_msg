-module(barrier_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

barrier_reply_test_() ->
    [{ "barrier_reply with xid 123", fun barrier_reply_with_xid/0 }].

barrier_reply_with_xid() ->
    Body = #ofp_barrier_reply{},
    Msg = #ofp_header{ type = barrier_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
