-module(queue_stats_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

queue_stats_reply_test_() ->
    [{ "queue_stats_reply with xid 123", fun queue_stats_reply_with_xid/0 }].

queue_stats_reply_with_xid() ->
    Body = #ofp_stats_reply{ type = queue,
                             flags = [],
                             body = [#ofp_queue_stats{ port_no = 1,
                                                       queue_id = 1 },
                                     #ofp_queue_stats{ port_no = 1,
                                                       queue_id = 2 }] },
    Msg = #ofp_header{ type = stats_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
