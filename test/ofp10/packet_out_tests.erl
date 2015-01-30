-module(packet_out_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

packet_out_test_() ->
    [{ "packet_out with no data", fun packet_out_no_data/0 },
     { "packet_out with all actions, data and xid 123", fun packet_out_with_data_xid/0 },
     { "packet_out with no actions", fun packet_out_no_actions/0 }].

packet_out_no_data() ->
    Data = <<>>,
    Actions = [#ofp_action_header{ type = output,
                                   body = #ofp_action_output{port = 2,
                                                             max_len = 65535}}],
    Body = #ofp_packet_out{ buffer_id = no_buffer,
                            in_port = none,
                            actions = Actions,
                            data = Data },
    Msg = #ofp_header{ type = packet_out,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

packet_out_with_data_xid() ->
    Data = <<1,128,194,0,0,14,1,2,3,4,5,6,136,204,2,9,7,0,0,0,0,0,0,1,35,
             4,5,7,0,0,0,12,6,2,0,120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0>>,
    Actions = [#ofp_action_header{ type = set_vlan_vid,
                                   body = #ofp_action_vlan_vid{ vlan_vid = 10 }},
               #ofp_action_header{ type = set_vlan_pcp,
                                   body = #ofp_action_vlan_pcp{ vlan_pcp = 0 }},
               #ofp_action_header{ type = strip_vlan,
                                   body = #ofp_action_strip_vlan{} },
               #ofp_action_header{ type = set_dl_src,
                                   body = #ofp_action_dl_addr{
                                             dl_addr = <<0,0,0,0,0,1>> }},
               #ofp_action_header{ type = set_dl_dst,
                                   body = #ofp_action_dl_addr{
                                             dl_addr = <<0,0,0,0,0,2>> }},
               #ofp_action_header{ type = set_nw_src,
                                   body = #ofp_action_nw_addr{
                                             nw_addr = <<10,0,0,1>> }},
               #ofp_action_header{ type = set_nw_dst,
                                   body = #ofp_action_nw_addr{
                                             nw_addr = <<20,0,0,2>> }},
               #ofp_action_header{ type = set_nw_tos,
                                   body = #ofp_action_nw_tos{ nw_tos = 16#b8 }},
               #ofp_action_header{ type = set_tp_src,
                                   body = #ofp_action_tp_port{ tp_port = 8 }},
               #ofp_action_header{ type = set_tp_dst,
                                   body = #ofp_action_tp_port{ tp_port = 0 }},
               #ofp_action_header{ type = enqueue,
                                   body = #ofp_action_enqueue{ port = 1,
                                                               queue_id = 1}},
               #ofp_action_header{ type = vendor,
                                   body = #ofp_action_vendor{
                                             vendor = 1234, data = <<"nazo">> }},
               #ofp_action_header{ type = output,
                                   body = #ofp_action_output{ port = 1,
                                                              max_len = 128 }}] ,
    Body = #ofp_packet_out{ buffer_id = no_buffer,
                            in_port = none,
                            actions = Actions,
                            data = Data },
    Msg = #ofp_header{ type = packet_out,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

packet_out_no_actions() ->
    Data = <<>>,
    Actions = [],
    Body = #ofp_packet_out{ buffer_id = no_buffer,
                            in_port = none,
                            actions = Actions,
                            data = Data },
    Msg = #ofp_header{ type = packet_out,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
