-module(vendor_stats_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

vendor_stats_reply_test_() ->
    [{ "vendor_stats_reply with xid 123", fun vendor_stats_reply_with_xid/0 }].

vendor_stats_reply_with_xid() ->
    Body = #ofp_stats_reply{ type = vendor, flags = [],
                             body = #ofp_vendor_stats{ vendor = 16#00002320,
                                                       data = <<0,0,0,0,0,0,0,0>> } },
    Msg = #ofp_header{ type = stats_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
