-module(nx_flow_mod_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

nx_flow_mod_test_() ->
    [{ "nx flow mod add with arp matches and xid 123", fun nx_flow_mod_add_arp/0 },
     { "nx flow mod add with ipv4 tcp matches and xid 123",
       fun nx_flow_mod_add_ipv4_tcp/0 },
     { "nx flow mod add with ipv4 udp matches and xid 123",
       fun nx_flow_mod_add_ipv4_udp/0 },
     { "nx flow mod add with ipv4 icmp matches and xid 123",
       fun nx_flow_mod_add_ipv4_icmp/0 },
     { "nx flow mod add with ipv6 icmp matches and xid 123",
       fun nx_flow_mod_add_ipv6_icmp/0 },
     { "nx flow mod add with regs and xid 123",
       fun nx_flow_mod_add_regs/0 }].

nx_flow_mod_add_arp() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = in_port,
                           value = <<16#fffd:16>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_dst,
                           value = <<255,255,255,255,255,255>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_src,
                           value = <<1,1,1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0806:16>> },
               #oxm_field{ vendor = nxm0,
                           field = arp_op,
                            % arp request
                           value = <<1:16>> },
               #oxm_field{ vendor = nxm0,
                           field = arp_spa,
                           value = <<1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = arp_tpa,
                           value = <<2,2,2,2>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_arp_sha,
                           value = <<1,1,1,1,1,1>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_arp_tha,
                           value = <<0,0,0,0,0,0>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_cookie,
                           value = <<0:64>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_tun_ipv4_src,
                           value = <<0:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_tun_ipv4_dst,
                           value = <<0:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_pkt_mark,
                           value = <<0:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_dp_hash,
                           value = <<0:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_recirc_id,
                           value = <<0:32>> }],
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 1,
                 actions = [] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_flow_mod_add_ipv4_tcp() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = in_port,
                           value = <<16#fffd:16>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_dst,
                           value = <<255,255,255,255,255,255>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_src,
                           value = <<1,1,1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_tos,
                           value = <<16#b8:8>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_proto,
                           value = <<6:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ip_ecn,
                           value = <<1:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ip_ttl,
                           value = <<1:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ip_frag,
                           value = <<0:8>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_src,
                           value = <<1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_dst,
                           value = <<2,2,2,2>> },
               #oxm_field{ vendor = nxm0,
                           field = tcp_src,
                           value = <<8888:16>> },
               #oxm_field{ vendor = nxm0,
                           field = tcp_dst,
                           value = <<80:16>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_tcp_flags,
                           value = <<80:16>> }],
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 1,
                 actions = [] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_flow_mod_add_ipv4_udp() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = in_port,
                           value = <<16#fffd:16>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_dst,
                           value = <<255,255,255,255,255,255>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_src,
                           value = <<1,1,1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_tos,
                           value = <<16#b8:8>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_proto,
                           value = <<17:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ip_ecn,
                           value = <<1:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ip_ttl,
                           value = <<1:8>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_src,
                           value = <<1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_dst,
                           value = <<2,2,2,2>> },
               #oxm_field{ vendor = nxm0,
                           field = udp_src,
                           value = <<8888:16>> },
               #oxm_field{ vendor = nxm0,
                           field = udp_dst,
                           value = <<80:16>> }],
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 1,
                 actions = [] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_flow_mod_add_ipv4_icmp() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = in_port,
                           value = <<16#fffd:16>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_dst,
                           value = <<255,255,255,255,255,255>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_src,
                           value = <<1,1,1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#0800:16>> },
               #oxm_field{ vendor = nxm0,
                           field = vlan_tci,
                           value = <<0:16>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_tos,
                           value = <<16#b8:8>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_proto,
                           value = <<17:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ip_ecn,
                           value = <<1:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ip_ttl,
                           value = <<1:8>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_src,
                           value = <<1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = ip_dst,
                           value = <<2,2,2,2>> },
               #oxm_field{ vendor = nxm0,
                           field = icmp_type,
                           value = <<8:8>> },
               #oxm_field{ vendor = nxm0,
                           field = icmp_code,
                           value = <<0:8>> }],
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 1,
                 actions = [] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_flow_mod_add_ipv6_icmp() ->
    Matches = [#oxm_field{ vendor = nxm0,
                           field = in_port,
                           value = <<16#fffd:16>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_dst,
                           value = <<255,255,255,255,255,255>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_src,
                           value = <<1,1,1,1,1,1>> },
               #oxm_field{ vendor = nxm0,
                           field = eth_type,
                           value = <<16#86dd:16>> },
               #oxm_field{ vendor = nxm0,
                           field = vlan_tci,
                           value = <<0:16>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ipv6_src,
                           value = <<1:128>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ipv6_dst,
                           value = <<2:128>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_icmpv6_type,
                           value = <<8:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_icmpv6_code,
                           value = <<0:8>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_nd_target,
                           value = <<0:128>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_nd_sll,
                           value = <<0:48>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_nd_tll,
                           value = <<0:48>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_ipv6_label,
                           value = <<0:32>> }],
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 1,
                 actions = [] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_flow_mod_add_regs() ->
    Matches = [#oxm_field{ vendor = nxm1,
                           field = nx_tun_id,
                           value = <<0:64>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg0,
                           value = <<0:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg1,
                           value = <<1:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg2,
                           value = <<2:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg3,
                           value = <<3:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg4,
                           value = <<4:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg5,
                           value = <<5:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg6,
                           value = <<6:32>> },
               #oxm_field{ vendor = nxm1,
                           field = nx_reg7,
                           value = <<7:32>> }],
    FlowMod = #nx_flow_mod{
                 command = add,
                 table_id = 0,
                 match = Matches,
                 priority = 1,
                 actions = [] },
    NXData = #nicira_header{ sub_type = flow_mod,
                             body = FlowMod },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.