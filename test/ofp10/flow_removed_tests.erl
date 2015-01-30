-module(flow_removed_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

flow_removed_test_() ->
    [{ "flow_remove with reason delete", fun reason_delete/0 },
     { "flow_remove with reason hard_timeout", fun reason_hard_timeout/0 },
     { "flow_remove with reason idle_timeout", fun reason_idle_timeout/0 }].

reason_delete() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_flow_removed{ match = Match, cookie = <<0,0,0,0,0,0,0,1>>,
                              priority = 65535, reason = delete,
                              duration_sec = 0, duration_nsec = 0,
                              idle_timeout = 0, packet_count = 0,
                              byte_count = 0 },
    Msg = #ofp_header{ type = flow_removed, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

reason_hard_timeout() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_flow_removed{ match = Match, cookie = <<0,0,0,0,0,0,0,1>>,
                              priority = 65535, reason = hard_timeout,
                              duration_sec = 0, duration_nsec = 0,
                              idle_timeout = 0, packet_count = 0,
                              byte_count = 0 },
    Msg = #ofp_header{ type = flow_removed, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

reason_idle_timeout() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1,% not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_flow_removed{ match = Match, cookie = <<0,0,0,0,0,0,0,1>>,
                              priority = 65535, reason = idle_timeout,
                              duration_sec = 0, duration_nsec = 0,
                              idle_timeout = 0, packet_count = 0,
                              byte_count = 0 },
    Msg = #ofp_header{ type = flow_removed, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
