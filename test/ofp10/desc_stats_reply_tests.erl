-module(desc_stats_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

desc_stats_reply_test_() ->
    [{ "desc_stats_reply with xid 123", fun desc_stats_reply_with_xid/0 }].

desc_stats_reply_with_xid() ->
    Body = #ofp_stats_reply{ type = desc, flags = [],
                             body = #ofp_desc_stats{ mfr_desc = <<"Nicira, Inc.">>,
                                                     hw_desc = <<"Open vSwitch">>,
                                                     sw_desc = <<"2.0.1">>,
                                                     serial_num = <<"None">>,
                                                     dp_desc = <<"None">> } },
    Msg = #ofp_header{ type = stats_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
