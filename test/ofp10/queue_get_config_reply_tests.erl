-module(queue_get_config_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

queue_get_config_reply_test_() ->
    [{ "queue_get_config_reply with xid 123", fun queue_get_config_reply_with_xid/0 }].

queue_get_config_reply_with_xid() ->
    Queues = [#ofp_packet_queue{ queue_id = 1,
                                 properties = [#ofp_queue_prop_min_rate{ rate = 5 }]},
              #ofp_packet_queue{ queue_id = 2,
                                 properties = [#ofp_queue_prop_min_rate{ rate = 6 },
                                               #ofp_queue_prop_min_rate{ rate = 7 }]}],
    Body = #ofp_queue_get_config_reply{ port_no = 1,
                                        queues = Queues },
    Msg = #ofp_header{ type = queue_get_config_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
