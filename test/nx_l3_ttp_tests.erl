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

% to drop multicast frame
control_frame_filter_flow_table_1_test() ->
    Match = #oxm_field{ vendor = nxm0,
                        field = eth_dst,
                        value = <<16#01, 16#00, 16#5e, 16#00, 16#00, 16#00>>,
                        mask = <<16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#f0>>,
                        has_mask = true },
    FlowMod = #nx_flow_mod{ command = add,
                            table_id = 0,
                            priority = 1,
                            match = [Match],
                            actions = [] },
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

% to accept non control frame
control_frame_filter_flow_table_2_test() ->
    % goto ingress vlan flow table
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 1 }}},
    FlowMod = #nx_flow_mod{ command = add,
                            table_id = 0,
                            priority = 1,
                            match = [],
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

% allow untagged
ingress_vlan_flow_table_test() ->
    Match = #oxm_field{ vendor = nxm0,
                        field = vlan_tci,
                        value = <<16#0000:16>>,
                        mask = <<16#1fff:16>>,
                        has_mask = true },
    % goto mac learning flow table
    Resubmit = #ofp_action_header{
                  type = vendor,
                  body = #ofp_action_vendor{
                            vendor = nicira,
                            data = #nx_action_resubmit{
                                      subtype = resubmit_table,
                                      in_port = in_port,
                                      table_id = 1 }}},
    FlowMod = #nx_flow_mod{ command = add,
                            table_id = 0,
                            priority = 3,
                            match = [Match],
                            actions = [Resubmit] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor, xid = 13, body = Body },
    EMsg = ?MODNAME:encode(Msg),
    io:format("EMsg: ~w~n", [EMsg]),
    { ok, DMsg, _Rest } = ?MODNAME:decode(EMsg),
    io:format("DMsg: ~w~n", [DMsg]),
    ?assertEqual(DMsg, Msg).
