-module(port_stats_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

port_stats_reply_test_() ->
    [{ "port_stats_reply with xid 123", fun port_stats_reply_with_xid/0 }].

port_stats_reply_with_xid() ->
    PortStats1 = #ofp_port_stats{ port_no = 1 },
    PortStats2 = #ofp_port_stats{ port_no = 2 },
    PortStatsLocal = #ofp_port_stats{ port_no = local },
    Body = #ofp_stats_reply{ type = port, flags = [], body = [PortStats2,
                                                              PortStatsLocal,
                                                              PortStats1] },
    Msg = #ofp_header{ type = stats_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
