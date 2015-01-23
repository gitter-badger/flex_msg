-module(aggregate_stats_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

aggregate_stats_reply_test_() ->
    [{ "aggregate_stats_reply with xid 123", fun aggregate_stats_reply_with_xid/0 }].

aggregate_stats_reply_with_xid() ->
    AggregateStats = #ofp_aggregate_stats{ packet_count = 0,
                                           byte_count = 0,
                                           flow_count = 0 },
    Body = #ofp_stats_reply{ type = aggregate, flags = [], body = AggregateStats },
    Msg = #ofp_header{ type = stats_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
