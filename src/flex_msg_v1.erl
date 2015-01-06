-module(flex_msg_v1).

-export([encode/1, decode/1]).
-export([prepend_of_header/2]).
-export([hello/0,
         openflow_error/3,
         echo_request/1,
         echo_reply/1,
         barrier/0,
         vendor/2,
         get_features/0,
         get_config/0,
         set_config/2,
         send_packet/2,
         add_flow/6,
         add_flow/7,
         modify_flow/6,
         modify_flow/7,
         delete_flow/3,
         set_port_up/2,
         set_port_down/2,
         set_port_packet_in/2,
         set_port_no_packet_in/2,
         set_port_modes/3,
         get_queue_config/1,
         get_switch_description/0,
         get_flow_stats/2,
         get_aggregate_stats/2,
         get_table_stats/0,
         get_port_stats_all/0,
         get_port_stats/1]).

-include("ofp_v1.hrl").

encode(Message) ->
    try
        flex_msg_v1_encode:do(Message)
    catch
        _:Exception ->
            { error, Exception }
    end.

decode(Binary) ->
    try
        flex_msg_v1_decode:do(Binary)
    catch
        _:Exception ->
            { error, Exception }
    end.

prepend_of_header(Xid, #ofp_hello{} = Body) ->
    #ofp_header{ type = hello, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_error_msg{} = Body) ->
    #ofp_header{ type = error, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_echo_request{} = Body) ->
    #ofp_header{ type = echo_request, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_echo_reply{} = Body) ->
    #ofp_header{ type = echo_reply, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_vendor_header{} = Body) ->
    #ofp_header{ type = vendor, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_features_request{} = Body) ->
    #ofp_header{ type = features_request, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_get_config_request{} = Body) ->
    #ofp_header{ type = get_config_request, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_switch_config{} = Body) ->
    #ofp_header{ type = set_config, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_packet_out{} = Body) ->
    #ofp_header{ type = packet_out, xid = Xid,  body = Body };
prepend_of_header(Xid, #ofp_flow_mod{} = Body) ->
    #ofp_header{ type = flow_mod, xid = Xid,  body = Body };
prepend_of_header(Xid, #ofp_port_mod{} = Body) ->
    #ofp_header{ type = port_mod, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_barrier_request{} = Body) ->
    #ofp_header{ type = barrier_request, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_queue_get_config_request{} = Body) ->
    #ofp_header{ type = queue_get_config_request, xid = Xid, body = Body };
prepend_of_header(Xid, #ofp_stats_request{} = Body) ->
    #ofp_header{ type = stats_request, xid = Xid, body = Body }.

hello() ->
    #ofp_hello{}.

openflow_error(Type, Code, Data) ->
    #ofp_error_msg{ type = Type, code = Code, data = Data }.

echo_request(Data) ->
    #ofp_echo_request{ data = Data }.

echo_reply(Data) ->
    #ofp_echo_reply{ data = Data }.

barrier() ->
    #ofp_barrier_request{}.

vendor(Vendor, Data) ->
    #ofp_vendor_header{ vendor = Vendor, data = Data }.

get_features() ->
    #ofp_features_request{}.

get_config() ->
    #ofp_get_config_request{}.

set_config(Flags, MissSendLen) ->
    #ofp_switch_config{ flags = Flags, miss_send_len = MissSendLen }.

send_packet(Data, Actions) ->
    #ofp_packet_out{ buffer_id = no_buffer,
                     in_port = none,
                     actions = create_actions(Actions),
                     data = Data }.

add_flow(Cookie, Priority, Idle, Hard, Matches, Actions) ->
    #ofp_flow_mod{ match = create_matches(Matches),
                   cookie = Cookie,
                   priority = Priority,
                   idle_timeout = Idle,
                   hard_timeout = Hard,
                   command = add,
                   flags = [send_flow_rem],
                   actions = create_actions(Actions) }.

add_flow(Cookie, Priority, Idle, Hard, Matches, Actions, Flags) ->
    #ofp_flow_mod{ match = create_matches(Matches),
                   priority = Priority,
                   idle_timeout = Idle,
                   hard_timeout = Hard,
                   cookie = Cookie,
                   command = add,
                   flags = Flags,
                   actions = create_actions(Actions) }.

modify_flow(Cookie, Priority, Idle, Hard, Matches, Actions) ->
    #ofp_flow_mod{ match = create_matches(Matches),
                   cookie = Cookie,
                   priority = Priority,
                   idle_timeout = Idle,
                   hard_timeout = Hard,
                   command = modify,
                   flags = [send_flow_rem],
                   actions = create_actions(Actions) }.

modify_flow(Cookie, Priority, Idle, Hard, Matches, Actions, Flags) ->
    #ofp_flow_mod{ match = create_matches(Matches),
                   priority = Priority,
                   idle_timeout = Idle,
                   hard_timeout = Hard,
                   cookie = Cookie,
                   command = modify,
                   flags = Flags,
                   actions = create_actions(Actions) }.

delete_flow(Cookie, Matches, OutPort) ->
    #ofp_flow_mod{ match = create_matches(Matches),
                   cookie = Cookie,
                   command = delete,
                   out_port = OutPort }.

set_port_up(PortNo, HwAddr) ->
    #ofp_port_mod{ port_no = PortNo,
                   hw_addr = HwAddr,
                   config = [],
                   mask = [port_down]}.

set_port_down(PortNo, HwAddr) ->
    #ofp_port_mod{ port_no = PortNo,
                   hw_addr = HwAddr,
                   config = [port_down],
                   mask = [port_down]}.

set_port_packet_in(PortNo, HwAddr) ->
    #ofp_port_mod{ port_no = PortNo,
                   hw_addr = HwAddr,
                   config = [],
                   mask = [no_packet_in]}.

set_port_no_packet_in(PortNo, HwAddr) ->
    #ofp_port_mod{ port_no = PortNo,
                   hw_addr = HwAddr,
                   config = [no_packet_in],
                   mask = [no_packet_in]}.

set_port_modes(PortNo, HwAddr, Modes) ->
    #ofp_port_mod{ port_no = PortNo,
                   hw_addr = HwAddr,
                   config = [],
                   mask = [],
                   advertised = Modes }.

get_queue_config(PortNo) ->
    #ofp_queue_get_config_request{ port_no = PortNo }.

get_switch_description() ->
    #ofp_stats_request{ type = desc, flags = [],
                        body = #ofp_desc_stats_request{} }.

get_flow_stats(Matches, TableId) ->
    Body = #ofp_flow_stats_request{ match = create_matches(Matches),
                                    table_id = TableId },
    #ofp_stats_request{ type = flow, body = Body }.

get_aggregate_stats(Matches, TableId) ->
    Body = #ofp_aggregate_stats_request{ match = create_matches(Matches),
                                         table_id = TableId },
    #ofp_stats_request{ type = aggregate, body = Body }.

get_table_stats() ->
    #ofp_stats_request{ type = table, body = #ofp_table_stats_request{} }.

get_port_stats_all() ->
    #ofp_stats_request{ type = port, body = #ofp_port_stats_request{ port_no = none }}.

get_port_stats(PortNo) ->
    #ofp_stats_request{ type = port, body = #ofp_port_stats_request{ port_no = PortNo }}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

create_matches(Matches) -> create_matches(Matches, #ofp_match{}).

create_matches([], EncMatches) -> EncMatches;
create_matches([Match | Rest], EncMatches) ->
    EncMatches2 = create_match(EncMatches, Match),
    create_matches(Rest, EncMatches2).

create_match(#ofp_match{wildcards = Wildcards } = Match,
             { in_port, InPort }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_IN_PORT),
    Match#ofp_match{ in_port = InPort, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { dl_src, DlSrc }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_DL_SRC),
    Match#ofp_match{ dl_src = DlSrc, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { dl_dst, DlDst }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_DL_DST),
    Match#ofp_match{ dl_dst = DlDst, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { dl_vlan, DlVlan }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_DL_VLAN),
    Match#ofp_match{ dl_vlan = DlVlan, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { dl_vlan_pcp, DlVlanPcp }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_DL_VLAN_PCP),
    Match#ofp_match{ dl_vlan_pcp = DlVlanPcp, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { dl_type, DlType }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_DL_TYPE),
    Match#ofp_match{ dl_type = DlType, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { nw_tos, NwTos }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_NW_TOS),
    Match#ofp_match{ nw_tos = NwTos, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { nw_proto, NwProto }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_NW_PROTO),
    Match#ofp_match{ nw_proto = NwProto, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { nw_src, NwAddr, NwAddrMask }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_NW_SRC_MASK),
    Wildcards3 = Wildcards2 bor ((32 - NwAddrMask) bsl ?OFPFW_NW_SRC_SHIFT),
    Match#ofp_match{ nw_src = NwAddr, nw_src_mask = NwAddrMask, wildcards = Wildcards3 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { nw_dst, NwAddr, NwAddrMask }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_NW_DST_MASK),
    Wildcards3 = Wildcards2 bor ((32 - NwAddrMask) bsl ?OFPFW_NW_DST_SHIFT),
    Match#ofp_match{ nw_dst = NwAddr, nw_dst_mask = NwAddrMask, wildcards = Wildcards3 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { tp_src, TpSrc }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_TP_SRC),
    Match#ofp_match{ tp_src = TpSrc, wildcards = Wildcards2 };
create_match(#ofp_match{wildcards = Wildcards } = Match,
             { tp_dst, TpDst }) ->
    Wildcards2 = Wildcards band (bnot ?OFPFW_TP_DST),
    Match#ofp_match{ tp_dst = TpDst, wildcards = Wildcards2 }.

create_actions(Actions) -> create_actions(Actions, []).

create_actions([], EncActions) -> lists:reverse(EncActions);
create_actions([Action | Rest], EncActions) ->
    EncAction = create_action(Action),
    create_actions(Rest, [EncAction | EncActions]).

create_action({ send_out_port,  PortNo, MaxLen }) ->
    Body = #ofp_action_output{ port = PortNo, max_len = MaxLen },
    #ofp_action_header{ type = output, body = Body };
create_action({ set_vlan_vid, VlanVID }) ->
    Body = #ofp_action_vlan_vid{ vlan_vid = VlanVID },
    #ofp_action_header{ type = set_vlan_vid, body = Body };
create_action({ set_vlan_priority, VlanPcp }) ->
    Body = #ofp_action_vlan_pcp{ vlan_pcp = VlanPcp },
    #ofp_action_header{ type = set_vlan_pcp, body = Body };
create_action({ strip_vlan_header }) ->
    Body = #ofp_action_strip_vlan{},
    #ofp_action_header{ type = strip_vlan, body = Body };
create_action({ set_eth_src_addr, DlAddr }) ->
    Body = #ofp_action_dl_addr{ dl_addr = DlAddr },
    #ofp_action_header{ type = set_dl_src, body = Body };
create_action({ set_eth_dst_addr, DlAddr }) ->
    Body = #ofp_action_dl_addr{ dl_addr = DlAddr },
    #ofp_action_header{ type = set_dl_dst, body = Body };
create_action({ set_ip_src_addr, NwAddr }) ->
    Body = #ofp_action_nw_addr{ nw_addr = NwAddr },
    #ofp_action_header{ type = set_nw_src, body = Body };
create_action({ set_ip_dst_addr, NwAddr }) ->
    Body = #ofp_action_nw_addr{ nw_addr = NwAddr },
    #ofp_action_header{ type = set_nw_dst, body = Body };
create_action({ set_ip_tos, NwTos }) ->
    Body = #ofp_action_nw_tos{ nw_tos = NwTos },
    #ofp_action_header{ type = set_nw_tos, body = Body };
create_action({ set_transport_src_port, TpPort }) ->
    Body = #ofp_action_tp_port{ tp_port = TpPort },
    #ofp_action_header{ type = set_tp_src, body = Body };
create_action({ set_transport_dst_port, TpPort }) ->
    Body = #ofp_action_tp_port{ tp_port = TpPort },
    #ofp_action_header{ type = set_tp_dst, body = Body };
create_action({ enqueue, Port, QueueID }) ->
    Body = #ofp_action_enqueue{ port = Port, queue_id = QueueID },
    #ofp_action_header{ type = enqueue, body = Body };
create_action({ vendor_action, Vendor, Data }) ->
    Body = #ofp_action_vendor{ vendor = Vendor, data = Data },
    #ofp_action_header{ type = vendor, body = Body }.
