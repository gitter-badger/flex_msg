-module(flex_msg_v1_decode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).

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

nx_flow_mod_add_multicast_drop_encode_test() ->
    Binary = packet(multicast_drop_flow),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_dst,
                           value = <<16#01, 16#00, 16#5e, 16#00, 16#00, 16#00>>,
                           mask = <<16#00, 16#00, 16#00, 16#7f, 16#ff, 16#ff>>,
                           has_mask = true }],
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 65535,
                 actions = [] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    ?assertEqual(Msg, DMsg).

nx_flow_mod_actions_decode_test() ->
    Binary = packet(nx_flow_mod_actions),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
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
    Tun = #ofp_action_header{
             type = vendor,
             body = #ofp_action_vendor{
                       vendor = nicira,
                       data = #nx_action_set_tunnel{ tun_id = 1 }}},
    Tun64 = #ofp_action_header{
             type = vendor,
             body = #ofp_action_vendor{
                       vendor = nicira,
                       data = #nx_action_set_tunnel64{ tun_id = 2 }}},
    SetQ = #ofp_action_header{
              type = vendor,
              body = #ofp_action_vendor{
                        vendor = nicira,
                        data = #nx_action_set_queue{ queue_id = 1 }}},
    PopQ = #ofp_action_header{
              type = vendor,
              body = #ofp_action_vendor{
                        vendor = nicira,
                        data = #nx_action_pop_queue{}}},
    Fin = #ofp_action_header{
             type = vendor,
             body = #ofp_action_vendor{
                       vendor = nicira,
                       data = #nx_action_fin_timeout{
                                 fin_idle_timeout = 10,
                                 fin_hard_timeout = 0 }}},
    RegMove = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_move{
                                     n_bits = 17,
                                     src_offset = 40,
                                     dst_offset = 0,
                                     src = #nxm_field_header{ vendor = nxm1,
                                                              field = nx_tun_id,
                                                              has_mask = false },
                                     dst = #nxm_field_header{ vendor = nxm1,
                                                              field = nx_reg0,
                                                              has_mask = false }}}},
    RegLoad = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_load{
                                     offset_nbits = 31,
                                     value = 16#62,
                                     dst = #nxm_field_header{ vendor = nxm1,
                                                              field = nx_tun_id,
                                                              has_mask = false }}}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 priority = 32768,
                 actions = [Learn, Tun, Tun64, SetQ, RegMove, RegLoad, PopQ, Fin, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    ?assertEqual(Msg, DMsg).

nx_set_flow_format_decode_test() ->
    Binary = packet(set_flow_mod_format),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    NXData = #nicira_header{ sub_type = set_flow_format,
                             body = #nx_set_flow_format{ format = nxm }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 7, body = Body },
    io:format("EMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

nx_role_request_decode_test() ->
    Binary = packet(role_request),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    NXData = #nicira_header{ sub_type = role_request,
                             body = #nx_role{ role = master }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 7, body = Body },
    io:format("EMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

nx_role_reply_decode_test() ->
    Binary = packet(role_reply),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    NXData = #nicira_header{ sub_type = role_reply,
                             body = #nx_role{ role = master }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 7, body = Body },
    io:format("EMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

nx_set_controller_id_decode_test() ->
    Binary = packet(set_controller_id),
    { ok, DMsg, _Rest } = ?MODNAME:decode(Binary),
    NXData = #nicira_header{ sub_type = set_controller_id,
                             body = #nx_controller_id{ id = 1 }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 7, body = Body },
    io:format("EMsg: ~w~n", [DMsg]),
    ?assertEqual(Msg, DMsg).

%%------------------------------------------------------------------------------
%% Packets
%%------------------------------------------------------------------------------

packet(Type) ->
    case Type of
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
              16#00, 16#0e, 16#ff, 16#f8, 16#01, 16#00, 16#00, 16#00>>;
        multicast_drop_flow ->
            <<1,4,0,64,0,0,0,13,0,0,35,32,0,0,0,13,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,255,255,255,255,255,255,255,255,0,0,0,16,0,0,
              0,0,0,0,0,0,3,12,1,0,94,0,0,0,0,0,0,127,255,255>>;
        nx_flow_mod_actions ->
            <<1,4,1,16,0,0,0,13,0,0,35,32,0,0,0,13,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,128,0,255,255,255,255,255,255,0,0,0,0,0,0,0,0,0,
              0,255,255,0,72,0,0,35,32,0,16,0,0,0,10,128,0,0,0,0,0,0,0,
              0,0,0,0,1,0,0,0,0,0,0,12,0,0,8,2,0,0,0,0,8,2,0,0,0,48,0,0,
              4,6,0,0,0,0,2,6,0,0,16,16,0,0,0,2,0,0,0,0,0,0,255,255,0,16,
              0,0,35,32,0,2,0,0,0,0,0,1,255,255,0,24,0,0,35,32,0,9,0,0,0,
              0,0,0,0,0,0,0,0,0,0,2,255,255,0,16,0,0,35,32,0,4,0,0,0,0,0,
              1,255,255,0,24,0,0,35,32,0,6,0,17,0,40,0,0,0,1,32,8,0,1,0,4,
              255,255,0,24,0,0,35,32,0,7,0,31,0,1,32,8,0,0,0,0,0,0,0,98,255,
              255,0,16,0,0,35,32,0,5,0,0,0,0,0,0,255,255,0,16,0,0,35,32,0,19,
              0,10,0,0,0,0,255,255,0,16,0,0,35,32,0,14,255,248,1,0,0,0>>;
        set_flow_mod_format ->
            <<1,4,0,20,0,0,0,7,0,0,35,32,0,0,0,12,0,0,0,2>>;
        role_request ->
            <<1,4,0,20,0,0,0,7,0,0,35,32,0,0,0,10,0,0,0,1>>;
        role_reply ->
            <<1,4,0,20,0,0,0,7,0,0,35,32,0,0,0,11,0,0,0,1>>;
        set_controller_id ->
            <<1,4,0,24,0,0,0,7,0,0,35,32,0,0,0,20,0,0,0,0,0,0,0,1>>
    end.
