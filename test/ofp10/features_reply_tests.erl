-module(features_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

features_reply_test_() ->
    [{ "features_reply with no ports", fun features_reply_with_no_ports/0 },
     { "features_reply with ports and xid", fun features_reply_with_ports_xid/0 }].

features_reply_with_no_ports() ->
    FeaturesReply = #ofp_switch_features{ datapath_id = <<16#00, 16#00, 16#00, 16#00,
                                                          16#00, 16#00, 16#00, 16#01>>,
                                          n_buffers = 256, n_tables = 1,
                                          capabilities = [flow_stats, table_stats,
                                                          port_stats, arp_match_ip],
                                          actions = [output, set_vlan_vid, set_vlan_pcp,
                                                     strip_vlan, set_dl_src, set_dl_dst,
                                                     set_nw_src, set_nw_dst, set_nw_tos,
                                                     set_tp_src, set_tp_dst, enqueue],
                                          ports = [] },
    Msg = #ofp_header{ type = features_reply, body = FeaturesReply },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

features_reply_with_ports_xid() ->
    Port1 = #ofp_phy_port{ port_no = 1,
                           hw_addr = <<16#62, 16#94, 16#3a, 16#f6, 16#40, 16#db>>,
                           name = <<"eth1">>,
                           config = [port_down,
                                     no_stp,
                                     no_recv,
                                     no_recv_stp,
                                     no_flood,
                                     no_fwd,
                                     no_packet_in],
                           state = [stp_listen, link_down,
                                    stp_learn, stp_forward],
                           curr = ['10gb_fd', copper],
                           advertised = [],
                           supported = ['10mb_hd', '10mb_fd', '100mb_hd',
                                        '100mb_fd','1gb_hd', '1gb_fd',
                                        '10gb_fd', copper, fibre,
                                        autoneg, pause, pause_asym],
                           peer = []},
    Port2 = #ofp_phy_port{ port_no = 2,
                           hw_addr = <<16#16, 16#7d, 16#a4, 16#37, 16#ba, 16#10>>,
                           name = <<"eth2">>,
                           config = [], state = [],
                           curr = ['10gb_fd', copper], advertised = [], supported = [],
                           peer = []},
    PortLocal = #ofp_phy_port{ port_no = local,
                               hw_addr = <<16#2a, 16#b4, 16#d6, 16#3c, 16#66, 16#ba>>,
                               name = <<"br0">>,
                               config = [port_down], state = [link_down],
                               curr = ['10mb_fd', copper], advertised = [], supported = [],
                               peer = []},
    FeaturesReply = #ofp_switch_features{ datapath_id = <<16#00, 16#00, 16#00, 16#00,
                                                          16#00, 16#00, 16#00, 16#01>>,
                                          n_buffers = 256, n_tables = 1,
                                          capabilities = [flow_stats, table_stats,
                                                          port_stats, arp_match_ip],
                                          actions = [output, set_vlan_vid, set_vlan_pcp,
                                                     strip_vlan, set_dl_src, set_dl_dst,
                                                     set_nw_src, set_nw_dst, set_nw_tos,
                                                     set_tp_src, set_tp_dst, enqueue],
                                          ports = [Port1, Port2, PortLocal] },
    Msg = #ofp_header{ type = features_reply, xid = 123, body = FeaturesReply },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
