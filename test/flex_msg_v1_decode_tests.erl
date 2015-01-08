-module(flex_msg_v1_decode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).

hello_decode_test() ->
    Binary = packet(hello),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(hello, Type).

error_decode_test() ->
    Binary = packet(error),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(error, Type).

echo_request_decode_test() ->
    Binary = packet(echo_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(echo_request, Type).

echo_reply_decode_test() ->
    Binary = packet(echo_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(echo_reply, Type).

vendor_decode_test() ->
    Binary = packet(vendor),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(vendor, Type).

features_request_decode_test() ->
    Binary = packet(features_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(features_request, Type).

features_reply_decode_test() ->
    Binary = packet(features_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(features_reply, Type).

get_config_request_decode_test() ->
    Binary = packet(get_config_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(get_config_request, Type).

get_config_reply_decode_test() ->
    Binary = packet(get_config_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(get_config_reply, Type).

set_config_decode_test() ->
    Binary = packet(set_config),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(set_config, Type).

packet_in_decode_test() ->
    Binary = packet(packet_in),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Type = DMsg#ofp_header.type,
    ?assertEqual(packet_in, Type).

flow_removed_decode_test() ->
    Binary = packet(flow_removed),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Match = #ofp_match{ wildcards = 3678462, in_port = 1,
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
    ?assertEqual(Msg, DMsg).

port_status_decode_test() ->
    Binary = packet(port_status),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Port = #ofp_phy_port{ port_no = controller,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete, desc = Port },
    Msg = #ofp_header{ type = port_status, xid = 4,  body = Body },
    ?assertEqual(Msg, DMsg).

packet_out_decode_test() ->
    Binary = packet(packet_out),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Data = <<1,128,194,0,0,14,1,2,3,4,5,6,136,204,2,9,7,0,0,0,0,0,0,1,35,
             4,5,7,0,0,0,12,6,2,0,120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0>>,
    Actions = [#ofp_action_header{ type = output,
                                   body = #ofp_action_output{port = 2, max_len = 65535}}],
    Body = #ofp_packet_out{ buffer_id = no_buffer, in_port = none,
                            actions = Actions, data = Data },
    Msg = #ofp_header{ type = packet_out, xid = 22,  body = Body },
    ?assertEqual(Msg, DMsg).

flow_mod_decode_test() ->
    Binary = packet(flow_mod),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Match = #ofp_match{ wildcards = 3678462, in_port = 1,
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Actions = [#ofp_action_header{ type = output,
                                   body = #ofp_action_output{port = 2, max_len = 65535}}],
    Body = #ofp_flow_mod{ match = Match, cookie = <<0,0,0,0,0,0,0,1>>,
                          command = add, flags = [send_flow_rem, check_overlap],
                          actions = Actions, out_port = 2 },
    Msg = #ofp_header{ type = flow_mod, xid = 21,  body = Body },
    ?assertEqual(Msg, DMsg).

port_mod_decode_test() ->
    Binary = packet(port_mod),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_port_mod{ port_no = 1, hw_addr = <<17,34,51,68,85,102>>,
                          config = [port_down], mask = [port_down], advertised = [] },
    Msg = #ofp_header{ type = port_mod, xid = 19, body = Body },
    ?assertEqual(Msg, DMsg).

barrier_request_decode_test() ->
    Binary = packet(barrier_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_barrier_request{},
    Msg = #ofp_header{ type = barrier_request, body = Body },
    ?assertEqual(Msg, DMsg).

barrier_reply_decode_test() ->
    Binary = packet(barrier_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_barrier_reply{},
    Msg = #ofp_header{ type = barrier_reply, body = Body },
    ?assertEqual(Msg, DMsg).

queue_get_config_request_decode_test() ->
    Binary = packet(queue_get_config_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_queue_get_config_request{ port_no = 1 },
    Msg = #ofp_header{ type = queue_get_config_request, xid = 12, body = Body },
    ?assertEqual(Msg, DMsg).

queue_get_config_reply_decode_test() ->
    Binary = packet(queue_get_config_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Queues = [#ofp_packet_queue{ queue_id = 1,
                                 properties = [#ofp_queue_prop_min_rate{ rate = 5 }]},
              #ofp_packet_queue{ queue_id = 2,
                                 properties = [#ofp_queue_prop_min_rate{ rate = 6 },
                                               #ofp_queue_prop_min_rate{ rate = 7 }]}],
    Body = #ofp_queue_get_config_reply{ port_no = local, queues = Queues },
    Msg = #ofp_header{ type = queue_get_config_reply, xid = 16#12345678, body = Body },
    ?assertEqual(Msg, DMsg).

desc_stats_request_decode_test() ->
    Binary = packet(desc_stats_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_stats_request{ type = desc, flags = [],
                               body = #ofp_desc_stats_request{} },
    Msg = #ofp_header{ type = stats_request, xid = 12, body = Body },
    ?assertEqual(Msg, DMsg).

flow_stats_request_decode_test() ->
    Binary = packet(flow_stats_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Match = #ofp_match{ wildcards = 3678463, in_port = 0,
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
    Msg = #ofp_header{ type = stats_request, xid = 13, body = Body },
    ?assertEqual(Msg, DMsg).

aggregate_stats_request_decode_test() ->
    Binary = packet(aggregate_stats_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Match = #ofp_match{ wildcards = 3678463, in_port = 0,
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_stats_request{ type = aggregate, flags = [],
                               body = #ofp_aggregate_stats_request{ match = Match,
                                                               table_id = all,
                                                               out_port = none } },
    Msg = #ofp_header{ type = stats_request, xid = 14, body = Body },
    ?assertEqual(Msg, DMsg).

table_stats_request_decode_test() ->
    Binary = packet(table_stats_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_stats_request{ type = table, flags = [],
                               body = #ofp_table_stats_request{} },
    Msg = #ofp_header{ type = stats_request, xid = 15, body = Body },
    ?assertEqual(Msg, DMsg).

port_stats_request_decode_test() ->
    Binary = packet(port_stats_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_stats_request{ type = port, flags = [],
                               body = #ofp_port_stats_request{ port_no = 1 } },
    Msg = #ofp_header{ type = stats_request, xid = 15, body = Body },
    ?assertEqual(Msg, DMsg).

vendor_stats_request_decode_test() ->
    Binary = packet(vendor_stats_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_stats_request{ type = vendor, flags = [],
                               body = #ofp_vendor_stats_request{ vendor = 16#00002320,
                                                                 data = <<0,0,0,0,0,
                                                                          0,0,0,255,255,
                                                                          0,0,255,0,0,0>> } },
    Msg = #ofp_header{ type = stats_request, xid = 5, body = Body },
    ?assertEqual(Msg, DMsg).

desc_stats_reply_decode_test() ->
    Binary = packet(desc_stats_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_stats_reply{ type = desc, flags = [],
                             body = #ofp_desc_stats{ mfr_desc = <<"Nicira, Inc.">>,
                                                     hw_desc = <<"Open vSwitch">>,
                                                     sw_desc = <<"2.0.1">>,
                                                     serial_num = <<"None">>,
                                                     dp_desc = <<"None">> } },
    Msg = #ofp_header{ type = stats_reply, xid = 12, body = Body },
    ?assertEqual(Msg, DMsg).

flow_stats_reply_decode_test() ->
    Binary = packet(flow_stats_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Match = #ofp_match{ wildcards = 4194303, in_port = 0,
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
    Msg = #ofp_header{ type = stats_reply, xid = 6, body = Body },
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

aggregate_stats_reply_decode_test() ->
    Binary = packet(aggregate_stats_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    AggregateStats = #ofp_aggregate_stats{},
    Body = #ofp_stats_reply{ type = aggregate, flags = [], body = AggregateStats },
    Msg = #ofp_header{ type = stats_reply, xid = 15, body = Body },
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

table_stats_reply_decode_test() ->
    Binary = packet(table_stats_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    TableStats = #ofp_table_stats{ table_id = 0, name = <<"classifier">>,
                                    wildcards = 4194303, max_entries = 1000000 },
    Body = #ofp_stats_reply{ type = table, flags = [], body = [TableStats] },
    Msg = #ofp_header{ type = stats_reply, xid = 16, body = Body },
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

port_stats_reply_decode_test() ->
    Binary = packet(port_stats_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    PortStats1 = #ofp_port_stats{ port_no = 1 },
    PortStats2 = #ofp_port_stats{ port_no = 2 },
    PortStatsLocal = #ofp_port_stats{ port_no = local },
    Body = #ofp_stats_reply{ type = port, flags = [], body = [PortStats2,
                                                              PortStatsLocal,
                                                              PortStats1] },
    Msg = #ofp_header{ type = stats_reply, xid = 17, body = Body },
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

vendor_stats_reply_decode_test() ->
    Binary = packet(vendor_stats_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Body = #ofp_stats_reply{ type = vendor, flags = [],
                             body = #ofp_vendor_stats{ vendor = 16#00002320,
                                                       data = <<0,0,0,0,0,0,0,0>> } },
    Msg = #ofp_header{ type = stats_reply, xid = 5, body = Body },
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

nx_flow_mod_table_id_decode_test() ->
    Binary = packet(nx_flow_mod_table_id),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    NXData = #nicira_header{ sub_type = flow_mod_table_id,
                             body = #nx_flow_mod_table_id{ set = true }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 7, body = Body },
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

nx_set_packet_in_format_decode_test() ->
    Binary = packet(nx_set_packet_in_format),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    NXData = #nicira_header{ sub_type = set_packet_in_format,
                             body = #nx_set_packet_in_format{ format = nxm }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 6, body = Body },
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

nx_flow_mod_add_decode_test() ->
    Binary = packet(nx_flow_mod_add),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    %io:format("~w~n", [DMsg]),
    FMS1 = #learn_match_field{ 
              src = #nxm_field_header{ vendor = nxm0, 
                                       field = vlan_tci,
                                       has_mask = false },
              dst = #nxm_field_header{ vendor = nxm0,
                                       field = vlan_tci,
                                       has_mask = false }},
    FMS2 = #learn_match_field{
              src = #nxm_field_header{ vendor = nxm0,
                                       field = eth_src,
                                       has_mask = false },
              dst = #nxm_field_header{ vendor = nxm0,
                                       field = eth_dst,
                                       has_mask = false}},
    FMS3 = #learn_output_action{
              port = #nxm_field_header{ vendor = nxm0,
                                        field = in_port,
                                        has_mask = false }},
    Learn = #ofp_action_header{
               type = vendor,
               body = #ofp_action_vendor{
                         vendor = nicira,
                         data = #nx_action_learn{
                                   idle_timeout = 0,
                                   hard_timeout = 10,
                                   priority = 32768,
                                   cookie = <<0:64>>,
                                   flags = [],
                                   table_id = 1, 
                                   fin_idle_timeout = 0,
                                   fin_hard_timeout = 0,
                                   flow_mod_spec = [FMS1, FMS2, FMS3]}}},
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 1 }}},
    FlowMod = #nx_flow_mod{
                 command = add, 
                 priority = 32768,
                 actions = [Learn, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    io:format("Msg: ~w~n", [Msg]),
    ?assertEqual(Msg, DMsg).

%%------------------------------------------------------------------------------
%% Packets
%%------------------------------------------------------------------------------

packet(Type) ->
    case Type of
        hello ->
            <<1, 0, 0, 8, 0, 0, 0, 0>>;
        error ->
            <<1, 1, 0, 20, 0, 0, 0, 0,
              0, 1, 0, 3, 102, 117, 103, 97,
              102, 117, 103, 97>>;
        echo_request ->
            <<1, 2, 0, 12, 0, 0, 0, 0, 104, 111, 103, 101>>;
        echo_reply ->
            <<1, 3, 0, 12, 0, 0, 0, 0, 104, 111, 103, 101>>;
        vendor ->
            <<1, 4, 0, 12, 0, 0, 0, 20, 0, 0, 0, 0>>;
        features_request ->
            <<1, 5, 0, 8, 0, 0, 0, 2>>;
        features_reply ->
            <<1,6,0,176,0,0,0,2,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,135,
              0,0,15,255,0,2,22,125,164,55,186,16,116,114,101,109,97,48,
              45,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,0,0,0,0,0,0,
              0,0,0,0,0,0,255,254,42,180,214,60,102,186,118,115,119,95,48,
              120,49,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,130,0,0,0,0,
              0,0,0,0,0,0,0,0,0,1,98,148,58,246,64,219,116,114,101,109,97,
              49,45,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,0,0,0,0,
              0,0,0,0,0,0,0,0>>;
        get_config_request ->
            <<1, 7, 0, 8, 0, 0, 0, 10>>;
        get_config_reply ->
            <<1, 8, 0, 12, 0, 0, 0, 10, 0, 0, 0, 128>>;
        set_config ->
            <<1, 9, 0, 12, 0, 0, 0, 9, 0, 0, 0, 128>>;
        packet_in ->
            <<1, 10, 0, 78, 0, 0, 0, 0, 255, 255, 255, 0, 0, 60, 0, 1, 0, 0,
              255, 255, 255, 255, 255, 255, 172, 93, 16, 49, 55, 121, 8, 6,
              0, 1, 8, 0, 6, 4, 0, 1, 172, 93, 16, 49, 55, 121, 192, 168, 2,
              254, 255, 255, 255, 255, 255, 255, 192, 168, 2, 5, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>;
        flow_removed ->
            <<1, 11, 0, 88, 0, 0, 0, 0, 0, 56, 32, 254, 0, 1, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 255, 255, 2,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0>>;
        port_status ->
            <<1, 12, 0, 64, 0, 0, 0, 4, 1, 0, 0, 0, 0, 0, 0, 0, 255, 253,
              1, 2, 3, 4, 5, 6, 102, 111, 111, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 2, 0, 0, 0, 0, 1, 0, 0,
              0, 32, 0, 0, 2, 0, 0, 0, 8, 0>>;
        packet_out ->
            <<1, 13, 0, 88, 0, 0, 0, 22, 255, 255, 255, 255, 255, 255, 0,
              8, 0, 0, 0, 8, 0, 2, 255, 255, 1, 128, 194, 0, 0, 14, 1, 2,
              3, 4, 5, 6, 136, 204, 2, 9, 7, 0, 0, 0, 0, 0, 0, 1, 35, 4,
              5, 7, 0, 0, 0, 12, 6, 2, 0, 120, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>;
        flow_mod ->
            <<1, 14, 0, 80, 0, 0, 0, 21, 0, 56, 32, 254, 0, 1, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
              0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 0, 2, 0, 3, 0, 0,
              0, 8, 0, 2, 255, 255>>;
        port_mod ->
            <<1, 15, 0, 32, 0, 0, 0, 19, 0, 1, 17, 34, 51, 68, 85, 102,
              0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0>>;
        barrier_request ->
            <<1, 18, 0, 8, 0, 0, 0, 0>>;
        barrier_reply ->
            <<1, 19, 0, 8, 0, 0, 0, 0>>;
        queue_get_config_request ->
            <<1, 20, 0, 12, 0, 0, 0, 12, 0, 1, 0, 0>>;
        queue_get_config_reply ->
            <<1, 21, 0, 80, 18, 52, 86, 120, 255, 254, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 24, 0, 0, 0, 1, 0, 16, 0, 0, 0, 0, 0, 5, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 40, 0, 0, 0, 1, 0, 16, 0, 0,
              0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 1, 0, 16, 0, 0, 0, 0, 0,
              7, 0, 0, 0, 0, 0, 0>>;
        desc_stats_request ->
            <<1, 16, 0, 12, 0, 0, 0, 12, 0, 0, 0, 0>>;
        flow_stats_request ->
            <<1, 16, 0, 56, 0, 0, 0, 13, 0, 1, 0, 0, 0, 56, 32, 255, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0,
              255, 255>>;
        aggregate_stats_request ->
            <<1, 16, 0, 56, 0, 0, 0, 14, 0, 2, 0, 0, 0, 56, 32, 255, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0,
              255, 255>>;
        table_stats_request ->
            <<1, 16, 0, 12, 0, 0, 0, 15, 0, 3, 0, 0>>;
        port_stats_request ->
            <<1, 16, 0, 20, 0, 0, 0, 15, 0, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0>>;
        vendor_stats_request ->
            <<16#01, 16#10, 16#00, 16#20, 16#00, 16#00, 16#00, 16#05, 16#ff,
              16#ff, 16#00, 16#00, 16#00, 16#00, 16#23, 16#20, 16#00, 16#00,
              16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#ff, 16#ff, 16#00,
              16#00, 16#ff, 16#00, 16#00, 16#00>>;
        desc_stats_reply ->
            <<1, 17, 4, 44, 0, 0, 0, 12, 0, 0, 0, 0, 78, 105, 99, 105, 114, 97,
              44, 32, 73, 110, 99, 46, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              79, 112, 101, 110, 32, 118, 83, 119, 105, 116, 99, 104, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 50, 46, 48, 46, 49, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 78, 111, 110,
              101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 78, 111, 110, 101, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>;
        flow_stats_reply ->
            <<1, 17, 0, 228, 0, 0, 0, 6, 0, 1, 0, 0, 0, 104, 3, 0, 0, 63, 255,
              255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
              0, 2, 0, 100, 0, 5, 0, 10, 0, 0, 0, 0, 0, 0, 1, 35, 69, 103, 137,
              171, 205, 239, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 3, 232,
              0, 0, 0, 8, 0, 1, 0, 0, 0, 0, 0, 8, 0, 2, 0, 0, 0, 112, 4, 0, 0,
              63, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 1, 0, 0, 0, 2, 0, 100, 0, 5, 0, 10, 0, 0, 0, 0, 0, 0, 1, 35,
              69, 103, 137, 171, 205, 239, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0,
              0, 0, 0, 3, 232, 0, 0, 0, 8, 0, 1, 0, 0, 0, 0, 0, 8, 0, 2, 0,
              0, 0, 0, 0, 8, 0, 3, 0, 0>>;
        aggregate_stats_reply ->
            <<1, 17, 0, 36, 0, 0, 0, 15, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>;
        table_stats_reply ->
            <<1, 17, 0, 76, 0, 0, 0, 16, 0, 3, 0, 0, 0, 0, 0, 0, 99, 108, 97,
              115, 115, 105, 102, 105, 101, 114, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, 255, 255, 0, 15, 66, 64,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>;
        port_stats_reply ->
            <<1, 17, 1, 68, 0, 0, 0, 17, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 255, 254, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>;
        vendor_stats_reply ->
            <<16#01, 16#11, 16#00, 16#18, 16#00, 16#00, 16#00, 16#05, 16#ff,
              16#ff, 16#00, 16#00, 16#00, 16#00, 16#23, 16#20, 16#00, 16#00,
              16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>;
        nx_flow_mod_table_id ->
            <<16#01,
              16#04,
              16#00, 16#18,
              16#00, 16#00, 16#00, 16#07,
              16#00, 16#00, 16#23, 16#20,
              16#00, 16#00, 16#00, 16#0f,
              16#01,
              16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>;
        nx_set_packet_in_format ->
            <<16#01,
              16#04,
              16#00, 16#14,
              16#00, 16#00, 16#00, 16#06,
              16#00, 16#00, 16#23, 16#20,
              16#00, 16#00, 16#00, 16#10,
              16#00, 16#00, 16#00, 16#01>>;
        nx_flow_mod_add ->
            <<16#01, 16#04, 16#00, 16#88, 16#00, 16#00, 16#00, 16#0d,
              16#00, 16#00, 16#23, 16#20, 16#00, 16#00, 16#00, 16#0d,
              16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
              16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#80, 16#00,
              16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#00, 16#00,
              16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
              16#ff, 16#ff, 16#00, 16#48, 16#00, 16#00, 16#23, 16#20,
              16#00, 16#10, 16#00, 16#00, 16#00, 16#0a, 16#80, 16#00,
              16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
              16#00, 16#00, 16#01, 16#00, 16#00, 16#00, 16#00, 16#00,
              16#00, 16#0c, 16#00, 16#00, 16#08, 16#02, 16#00, 16#00,
              16#00, 16#00, 16#08, 16#02, 16#00, 16#00, 16#00, 16#30,
              16#00, 16#00, 16#04, 16#06, 16#00, 16#00, 16#00, 16#00,
              16#02, 16#06, 16#00, 16#00, 16#10, 16#10, 16#00, 16#00,
              16#00, 16#02, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
              16#ff, 16#ff, 16#00, 16#10, 16#00, 16#00, 16#23, 16#20,
              16#00, 16#0e, 16#ff, 16#f8, 16#01, 16#00, 16#00, 16#00>>
    end.
