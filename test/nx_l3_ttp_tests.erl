-module(nx_l3_ttp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).

% enable multitable
enable_multitable_capability_test() ->
    NXData = #nicira_header{ sub_type = flow_mod_table_id,
                             body = #nx_flow_mod_table_id{ set = true }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

control_frame_goto_50_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = in_port,
                           value = <<16#fffd:16>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0806:16>> },
               #oxm_field{ vendor = nxm0,
                           field = arp_op,
                           value = <<2:16>> }],
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 50 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 3,
                 actions = [Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

control_frame_goto_10_test() ->
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 10 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 priority = 3,
                 actions = [Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

mac_learning_from_arp_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0806:16>> }],
    FMS1 = #learn_match_field{
              src = #nxm_field_header{ vendor = nxm0,
                                       field = vlan_tci,
                                       has_mask = false },
              dst = #nxm_field_header{ vendor = nxm0,
                                       field = vlan_tci,
                                       has_mask = false }},
    FMS2 = #learn_match_field{
              src = #nxm_field_header{ vendor = nxm1,
                                       field = nx_arp_sha,
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
                                   priority = 65535,
                                   cookie = <<0:64>>,
                                   flags = [],
                                   table_id = 50,
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
                                      table_id = 20 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 10,
                 match = Matches,
                 priority = 1,
                 actions = [Learn, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

mac_learning_from_ip_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> }],
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
                                   priority = 65535,
                                   cookie = <<0:64>>,
                                   flags = [],
                                   table_id = 50,
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
                                      table_id = 20 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 10,
                 match = Matches,
                 priority = 1,
                 actions = [Learn, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

protocol_filter_arp_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0806:16>> }],
    Output = #ofp_action_header{ type = output,
                                 body = #ofp_action_output{
                                           port = controller,
                                           max_len = 65535 }},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 20,
                 match = Matches,
                 priority = 1,
                 actions = [Output] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

protocol_filter_ipv4_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> }],
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 30 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 20,
                 match = Matches,
                 priority = 1,
                 actions = [Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

ipv4_routing_table_self_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_dst,
                           value = <<11,22,33,44>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_proto,
                           value = <<1:8>> }],
    RegMove1 = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_move{
                                     n_bits = 32,
                                     src_offset = 0,
                                     dst_offset = 0,
                                     src = #nxm_field_header{ vendor = nxm0,
                                                              field = ip_dst,
                                                              has_mask = false },
                                     dst = #nxm_field_header{ vendor = nxm0,
                                                              field = ip_src,
                                                              has_mask = false }}}},
    RegMove2 = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_move{
                                     n_bits = 32,
                                     src_offset = 0,
                                     dst_offset = 0,
                                     src = #nxm_field_header{ vendor = nxm0,
                                                              field = ip_src,
                                                              has_mask = false },
                                     dst = #nxm_field_header{ vendor = nxm0,
                                                              field = ip_dst,
                                                              has_mask = false }}}},
    RegMove3 = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_move{
                                     n_bits = 48,
                                     src_offset = 0,
                                     dst_offset = 0,
                                     src = #nxm_field_header{ vendor = nxm0,
                                                              field = eth_src,
                                                              has_mask = false },
                                     dst = #nxm_field_header{ vendor = nxm0,
                                                              field = eth_dst,
                                                              has_mask = false }}}},
    RegMove4 = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_move{
                                     n_bits = 48,
                                     src_offset = 0,
                                     dst_offset = 0,
                                     src = #nxm_field_header{ vendor = nxm0,
                                                              field = eth_dst,
                                                              has_mask = false },
                                     dst = #nxm_field_header{ vendor = nxm0,
                                                              field = eth_src,
                                                              has_mask = false }}}},
    SetTpSrc = #ofp_action_header{ type = set_tp_src,
                                   body = #ofp_action_tp_port{ tp_port = 0 }},
    SetTpDst = #ofp_action_header{ type = set_tp_dst,
                                   body = #ofp_action_tp_port{ tp_port = 0 }},
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 50 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 30,
                 match = Matches,
                 priority = 4,
                 actions = [RegMove1, RegMove2, RegMove3, RegMove4,
                            SetTpSrc, SetTpDst, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

ipv4_routing_table_local_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
              #oxm_field{ vendor = nxm0,
                          field = ip_src,
                          value = <<11,22,33,44>>,
                          mask = <<11,22,33,44>>,
                          has_mask = true },
              #oxm_field{ vendor = nxm0,
                          field = ip_dst,
                          value = <<11,22,33,44>>,
                          mask = <<11,22,33,44>>,
                          has_mask = true }],
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 50 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 30,
                 match = Matches,
                 priority = 4,
                 actions = [Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

ipv4_routing_table_connected_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_dst,
                           value = <<11,22,33,44>>,
                           mask = <<11,22,33,44>>,
                           has_mask = true }],
    SetDlSrc = #ofp_action_header{
                  type = set_dl_src,
                  body = #ofp_action_dl_addr{
                            dl_addr = <<11,22,33,44,55,66>> }},
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 40 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 30,
                 match = Matches,
                 priority = 3,
                 actions = [SetDlSrc, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

ipv4_routing_table_static_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_dst,
                           value = <<11,22,33,44>>,
                           mask = <<11,22,33,44>>,
                           has_mask = true }],
    SetDlSrc = #ofp_action_header{
                  type = set_dl_src,
                  body = #ofp_action_dl_addr{
                            dl_addr = <<11,22,33,44,55,66>> }},
    RegLoad = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_load{
                                     offset_nbits = 32,
                                     value = 16#fffffffe,
                                     dst = #nxm_field_header{ vendor = nxm1,
                                                              field = nx_reg0,
                                                              has_mask = false }}}},
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 40 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 30,
                 match = Matches,
                 priority = 2,
                 actions = [SetDlSrc, RegLoad, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

ipv4_routing_table_dgw_test() ->
    SetDlSrc = #ofp_action_header{
                  type = set_dl_src,
                  body = #ofp_action_dl_addr{
                            dl_addr = <<11,22,33,44,55,66>> }},
    RegLoad = #ofp_action_header{
                 type = vendor,
                 body = #ofp_action_vendor{
                           vendor = nicira,
                           data = #nx_action_reg_load{
                                     offset_nbits = 32,
                                     value = 16#fffffffe,
                                     dst = #nxm_field_header{ vendor = nxm1,
                                                              field = nx_reg0,
                                                              has_mask = false }}}},
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 40 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 30,
                 priority = 1,
                 actions = [SetDlSrc, RegLoad, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

nexthop_connected_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg0,
                           value = <<11,22,33,44>> }],
    SetDlDst = #ofp_action_header{
                  type = set_dl_dst,
                  body = #ofp_action_dl_addr{
                            dl_addr = <<11,22,33,44,55,66>> }},
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 50 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 40,
                 match = Matches,
                 priority = 3,
                 actions = [SetDlDst, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

nexthop_static_dgw_test() ->
    Matches = [#oxm_field{ vendor = nxm1,
                           field = nx_reg0,
                           value = <<11,22,33,44>> }],
    SetDlDst = #ofp_action_header{
                  type = set_dl_dst,
                  body = #ofp_action_dl_addr{
                            dl_addr = <<11,22,33,44,55,66>> }},
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 50 }}},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 40,
                 match = Matches,
                 priority = 2,
                 actions = [SetDlDst, Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).

nexthop_notfound_test() ->
    Output = #ofp_action_header{ type = output,
                                 body = #ofp_action_output{
                                           port = controller,
                                           max_len = 65535 }},
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 40,
                 priority = 1,
                 actions = [Output] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("EMsg: ~w~n", [EMsg]),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).
