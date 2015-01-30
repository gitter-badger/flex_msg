-module(vendor_stats_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

vendor_stats_request_test_() ->
    [{ "vendor_stats_request with xid 123", fun vendor_stats_request_with_xid/0 }].

vendor_stats_request_with_xid() ->
    Body = #ofp_stats_request{ type = vendor,
                               flags = [],
                               body = #ofp_vendor_stats_request{
                                         vendor = 16#00002320,
                                         data = <<0,0,0,0,0,
                                                  0,0,0,255,255,
                                                  0,0,255,0,0,0>> } },
    Msg = #ofp_header{ type = stats_request,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
