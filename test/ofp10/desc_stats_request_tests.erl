-module(desc_stats_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

desc_stats_request_test_() ->
    [{ "desc_stats_request with xid 123", fun desc_stats_request_with_xid/0 }].

desc_stats_request_with_xid() ->
    Body = #ofp_stats_request{ type = desc,
                               flags = [more],
                               body = #ofp_desc_stats_request{} },
    Msg = #ofp_header{ type = stats_request,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
