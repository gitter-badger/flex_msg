-module(flow_stats_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

flow_stats_reply_test_() ->
    [{ "flow_stats_reply with xid 123", fun flow_stats_reply_with_xid/0 }].

flow_stats_reply_with_xid() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Actions1 = [#ofp_action_header{ type = output,
                                    body = #ofp_action_output{port = 1, max_len = 0 }},
                #ofp_action_header{ type = output,
                                    body = #ofp_action_output{port = 2, max_len = 0 }}],
    Actions2 = [#ofp_action_header{ type = output,
                                    body = #ofp_action_output{port = 1, max_len = 0 }},
                #ofp_action_header{ type = output,
                                    body = #ofp_action_output{port = 2, max_len = 0 }},
                #ofp_action_header{ type = output,
                                    body = #ofp_action_output{port = 3, max_len = 0 }}],
    FlowStats1 = #ofp_flow_stats{ table_id = 3, match = Match, duration_sec = 1,
                                  duration_nsec = 2, priority = 100, idle_timeout = 5,
                                  hard_timeout = 10, cookie = <<1,35,69,103,137,171,205,239>>,
                                  packet_count = 10, byte_count = 1000, actions = Actions1 },
    FlowStats2 = #ofp_flow_stats{ table_id = 4, match = Match, duration_sec = 1,
                                  duration_nsec = 2, priority = 100, idle_timeout = 5,
                                  hard_timeout = 10, cookie = <<1,35,69,103,137,171,205,239>>,
                                  packet_count = 10, byte_count = 1000, actions = Actions2 },
    Body = #ofp_stats_reply{ type = flow, flags = [], body = [FlowStats1, FlowStats2] },
    Msg = #ofp_header{ type = stats_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
