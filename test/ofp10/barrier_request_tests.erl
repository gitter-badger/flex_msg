-module(barrier_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

barrier_request_test_() ->
    [{ "barrier_request with xid 123", fun barrier_request_with_xid/0 }].

barrier_request_with_xid() ->
    Body = #ofp_barrier_request{},
    Msg = #ofp_header{ type = barrier_request,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
