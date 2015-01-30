-module(flow_stats_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

flow_stats_request_test_() ->
    [{ "flow_stats_request with xid 123", fun flow_stats_request_with_xid/0 }].

flow_stats_request_with_xid() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_stats_request{ type = flow, flags = [],
                               body = #ofp_flow_stats_request{ match = Match,
                                                               table_id = all,
                                                               out_port = none } },
    Msg = #ofp_header{ type = stats_request,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
