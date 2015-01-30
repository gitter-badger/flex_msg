-module(aggregate_stats_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

aggregate_stats_request_test_() ->
    [{ "aggregate_stats_request with xid 123", fun aggregate_stats_request_with_xid/0 }].

aggregate_stats_request_with_xid() ->
    Match = #ofp_match{ wildcards = 1056768, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 32, tp_src = 32,
                        tp_dst = 0 },
    Body = #ofp_stats_request{ type = aggregate, flags = [],
                               body = #ofp_aggregate_stats_request{ match = Match,
                                                                    table_id = all,
                                                                    out_port = none } },
    Msg = #ofp_header{ type = stats_request,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
