-module(flex_msg_v1_usecases_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).

% enable multitable
enabling_multitable_capability_test() ->
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

% check destination mac addr.
controlling_frame_test() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = eth_dst,
                           value = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>> }],
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
                 match = Matches,
                 priority = 32768,
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
