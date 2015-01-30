-module(flow_mod_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

flow_mod_test_() ->
    [{ "flow_mod_add with flow_mod_flags", fun flow_mod_add_with_flags/0 },
     { "flow_mod_add with no flow_mod_flags", fun flow_mod_add_with_no_flags/0 },
     { "flow_mod_modify", fun flow_mod_modify/0 },
     { "flow_mod_modify_strict", fun flow_mod_modify_strict/0 },
     { "flow_mod_delete", fun flow_mod_delete/0 },
     { "flow_mod_delete_strict", fun flow_mod_delete_strict/0 }].

flow_mod_add_with_flags() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Actions = [#ofp_action_header{ type = output,
                                   body = #ofp_action_output{ port = 2,
                                                              max_len = 65535}}],
    Body = #ofp_flow_mod{ match = Match, cookie = <<0,0,0,0,0,0,0,1>>,
                          command = add,
                          flags = [send_flow_rem,
                                   check_overlap,
                                   emerg],
                          actions = Actions,
                          out_port = none },
    Msg = #ofp_header{ type = flow_mod, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_add_with_no_flags() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Actions = [#ofp_action_header{ type = output,
                                   body = #ofp_action_output{ port = 2,
                                                              max_len = 65535}}],
    Body = #ofp_flow_mod{ match = Match, cookie = <<0,0,0,0,0,0,0,1>>,
                          command = add,
                          actions = Actions,
                          out_port = none },
    Msg = #ofp_header{ type = flow_mod, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_modify() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_flow_mod{ match = Match,
                          command = modify,
                          out_port = 2 },
    Msg = #ofp_header{ type = flow_mod, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_modify_strict() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_flow_mod{ match = Match,
                          command = modify_strict,
                          out_port = 2 },
    Msg = #ofp_header{ type = flow_mod, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_delete() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_flow_mod{ match = Match,
                          command = delete,
                          out_port = 2 },
    Msg = #ofp_header{ type = flow_mod, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_delete_strict() ->
    Match = #ofp_match{ wildcards = 1581056, in_port = 1, % not OFPFW_ALL
                        dl_src = <<0,0,0,0,0,0>>, dl_dst = <<0,0,0,0,0,0>>,
                        dl_vlan = 0, dl_vlan_pcp = 0,
                        dl_type = 0, nw_tos = 0,
                        nw_proto = 0, nw_src = <<0,0,0,0>>, nw_src_mask = 0,
                        nw_dst = <<0,0,0,0>>, nw_dst_mask = 0, tp_src = 0,
                        tp_dst = 0 },
    Body = #ofp_flow_mod{ match = Match,
                          command = delete_strict,
                          out_port = 2 },
    Msg = #ofp_header{ type = flow_mod, body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
