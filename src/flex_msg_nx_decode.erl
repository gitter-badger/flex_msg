-module(flex_msg_nx_decode).

-export([do/1, decode_action/1]).

-include("ofp_nx.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

do(<<?NXT_FLOW_MOD_TABLE_ID:32, SetInt:8, _:7/bytes>>) ->
    Set = case SetInt of
             0 -> false;
             _ -> true
          end,
    #nicira_header{ sub_type = flow_mod_table_id,
                    body = #nx_flow_mod_table_id{ set = Set }};
do(<<?NXT_SET_PACKET_IN_FORMAT:32, FormatInt:32>>) ->
    Format = case FormatInt of
                 ?NXPIF_OPENFLOW10 -> openflow10;
                 ?NXPIF_NXM        -> nxm
             end,
    #nicira_header{ sub_type = set_packet_in_format,
                    body = #nx_set_packet_in_format{ format = Format }};
do(<<?NXT_FLOW_MOD:32, Cookie:8/bytes, TableId:8, CommandInt:8, Idle:16,
     Hard:16, Priority:16, BufferIdInt:32, OutPortInt:16,
     FlagsBin:2/bytes, MatchLen:16, _:48, Data/bytes>>) ->
    PadLen = pad_length(8, MatchLen),
    Command = get_id(flow_mod_command, CommandInt),
    Flags = binary_to_flags(flow_mod_flags, FlagsBin),
    OutPortNo = get_id(port_no, OutPortInt),
    BufferId = get_id(max_len, BufferIdInt),
    <<MatchBin:MatchLen/bytes, _:PadLen/bytes, ActionsBin/bytes>> = Data,
    Match = decode_matches(MatchBin),
    Actions = flex_msg_v1_decode:decode_actions(ActionsBin),
    FlowMod = #nx_flow_mod{ cookie = Cookie,
                            table_id = TableId,
                            command = Command,
                            idle_timeout = Idle,
                            hard_timeout = Hard,
                            priority = Priority,
                            out_port = OutPortNo,
                            flags = Flags,
                            buffer_id = BufferId,
                            match = Match,
                            actions = Actions },
    #nicira_header{ sub_type = flow_mod, body = FlowMod }.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

decode_matches(Binary) -> decode_matches(Binary, []).

decode_matches(<<>>, Matches) -> lists:reverse(Matches);
decode_matches(Binary, Matches) ->
    { Match, Rest } = decode_match(Binary),
    decode_matches(Rest, [Match | Matches]).

decode_match(<<VendorInt:16, FieldInt:7, 0:1, Length:8, Rest/bytes>>) ->
    <<Value:Length/bytes, Next/bytes>> = Rest,
    Vendor = nxm_vendor(VendorInt),
    Field = nxm_field(Vendor, FieldInt),
    BitLength = flex_msg_nx_map:nxm_field_bit_length(Field),
    { #oxm_field{ vendor =Vendor,
                  field = Field,
                  value = flex_msg_v1_utils:uncut_bits(Value, BitLength) },
      Next };
decode_match(<<VendorInt:16, FieldInt:7, 1:1, Length:8, Rest/bytes>>) ->
    Length2 = Length div 2,
    <<Value:Length2/bytes, Mask:Length2/bytes, Next/bytes>> = Rest,
    Vendor = nxm_vendor(VendorInt),
    Field = nxm_field(Vendor, FieldInt),
    BitLength = flex_msg_nx_map:nxm_field_bit_length(Field),
    { #oxm_field{ vendor = Vendor,
                  field = Field,
                  value = flex_msg_v1_utils:uncut_bits(Value, BitLength),
                  mask = flex_msg_v1_utils:uncut_bits(Mask, BitLength) },
      Next }.

decode_match_header(<<VendorInt:16, FieldInt:7, HasMaskInt:1, _Length:8>>) ->
    Vendor = nxm_vendor(VendorInt),
    Field = nxm_field(Vendor, FieldInt),
    HasMask = case HasMaskInt of
                  0 -> false;
                  1 -> true
              end,
    #nxm_field_header{ vendor = Vendor, field = Field, has_mask = HasMask  }.

decode_action(<<?NXAST_RESUBMIT:16, InPortInt:16, TableIdInt:8, _:24>>) ->
    InPort = get_id(port_no, InPortInt),
    TableId = get_id(table_id, TableIdInt),
    #nx_action_resubmit{ subtype = resubmit,
                         in_port = InPort,
                         table_id = TableId };
decode_action(<<?NXAST_RESUBMIT_TABLE:16, InPortInt:16, TableIdInt:8, _:24>>) ->
    InPort = get_id(port_no, InPortInt),
    TableId = get_id(table_id, TableIdInt),
    #nx_action_resubmit{ subtype = resubmit_table,
                         in_port = InPort,
                         table_id = TableId };
decode_action(<<?NXAST_LEARN:16, Idle:16, Hard:16, Priority:16,
                Cookie:8/bytes, FlagsBin:2/bytes, TableIdInt:8,
                _:8, FinIdle:16, FinHard:16, FMSsBin/bytes>>) ->
    FlowModSpecs = decode_flow_mod_specs(FMSsBin),
    TableId = get_id(table_id, TableIdInt),
    Flags = binary_to_flags(nx_learn_flags, FlagsBin),
    #nx_action_learn{
       idle_timeout = Idle,
       hard_timeout = Hard,
       priority = Priority,
       cookie = Cookie,
       flags = Flags,
       table_id = TableId,
       fin_idle_timeout = FinIdle,
       fin_hard_timeout = FinHard,
       flow_mod_spec = FlowModSpecs }.

decode_flow_mod_specs(Binary) -> decode_flow_mod_specs(Binary, []).

decode_flow_mod_specs(<<>>, Specs) -> lists:reverse(Specs);
decode_flow_mod_specs(<<0:32>>, Specs) -> lists:reverse(Specs);
decode_flow_mod_specs(<<_:1, SrcInt:2, DstInt:2, Nbits:11, Data/bytes>>, Specs) ->
    SrcSpec = nx_learn(src, SrcInt),
    DstSpec = nx_learn(dst, DstInt),
    {Body, Rest} =
        case nx_learn_flow_mod_spec(SrcSpec, DstSpec) of
            match_field ->
                <<Src:4/bytes, _:2/bytes, Dst:4/bytes,
                  _:2/bytes, Next/bytes>> = Data,
                SrcMatch = decode_match_header(Src),
                DstMatch = decode_match_header(Dst),
                Action = #learn_match_field{ src = SrcMatch, dst = DstMatch },
                { Action, Next };
            match_immediate ->
                <<Value:Nbits/bits, Dst:4/bytes, _:2/bytes, Next/bytes>> = Data,
                DstMatch = decode_match_header(Dst),
                Action = #learn_immediate_field{ value = Value, dst = DstMatch },
                { Action, Next };
            load_field ->
                <<Src:4/bytes, _:2/bytes, Dst:4/bytes, _:2/bytes, Next/bytes>> = Data,
                SrcMatch = decode_match_header(Src),
                DstMatch = decode_match_header(Dst),
                Action = #learn_load_field{ src = SrcMatch, dst = DstMatch },
                { Action, Next };
            load_immediate ->
                <<Value:Nbits/bits, Dst:4/bytes, _:2/bytes, Next/bytes>> = Data,
                DstMatch = decode_match_header(Dst),
                Action = #learn_load_immediate_field{ value = Value, dst = DstMatch },
                { Action, Next };
            output_action ->
                <<PortBin:4/bytes, _:2/bytes, Next/bytes>> = Data,
                PortMatch = decode_match_header(PortBin),
                Action = #learn_output_action{ port = PortMatch },
                { Action, Next }
        end,
    decode_flow_mod_specs(Rest, [Body | Specs]).

pad_length(Width, Length) ->
    (Width - Length rem Width) rem Width.

binary_to_flags(Type, Binary) ->
    flex_msg_v1_utils:binary_to_flags(flex_msg_v1_enum, Type, Binary).

get_id(Enum, Value) ->
    flex_msg_v1_utils:get_enum_name(flex_msg_v1_enum, Enum, Value).

nx_learn(src, 0) -> field;
nx_learn(src, 1) -> immediate;
nx_learn(dst, 0) -> match;
nx_learn(dst, 1) -> load;
nx_learn(dst, 2) -> output;
nx_learn(dst, 3) -> reserved.

nx_learn_flow_mod_spec(field, match)     -> match_field;
nx_learn_flow_mod_spec(immediate, match) -> match_immediate;
nx_learn_flow_mod_spec(field, load)      -> load_field;
nx_learn_flow_mod_spec(immediate, load)  -> load_immediate;
nx_learn_flow_mod_spec(field, output)    -> output_action.

nxm_vendor(?NXM0) -> nxm0;
nxm_vendor(?NXM1) -> nxm1.

nxm_field(nxm0, ?NXM_OF_IN_PORT) -> in_port;
nxm_field(nxm0, ?NXM_OF_ETH_DST) -> eth_dst;
nxm_field(nxm0, ?NXM_OF_ETH_SRC) -> eth_src;
nxm_field(nxm0, ?NXM_OF_ETH_TYPE) -> eth_type;
nxm_field(nxm0, ?NXM_OF_VLAN_TCI) -> vlan_tci;
nxm_field(nxm0, ?NXM_OF_IP_TOS) -> ip_tos;
nxm_field(nxm0, ?NXM_OF_IP_PROTO) -> ip_proto;
nxm_field(nxm0, ?NXM_OF_IP_SRC) -> ip_src;
nxm_field(nxm0, ?NXM_OF_IP_DST) -> ip_dst;
nxm_field(nxm0, ?NXM_OF_TCP_SRC) -> tcp_src;
nxm_field(nxm0, ?NXM_OF_TCP_DST) -> tcp_dst;
nxm_field(nxm0, ?NXM_OF_UDP_SRC) -> udp_src;
nxm_field(nxm0, ?NXM_OF_UDP_DST) -> udp_dst;
nxm_field(nxm0, ?NXM_OF_ICMP_TYPE) -> icmp_type;
nxm_field(nxm0, ?NXM_OF_ICMP_CODE) -> icmp_code;
nxm_field(nxm0, ?NXM_OF_ARP_OP) -> arp_op;
nxm_field(nxm0, ?NXM_OF_ARP_SPA) -> arp_spa;
nxm_field(nxm0, ?NXM_OF_ARP_TPA) -> arp_tpa;
nxm_field(nxm1, ?NXM_NX_REG0) -> nx_reg0;
nxm_field(nxm1, ?NXM_NX_REG1) -> nx_reg1;
nxm_field(nxm1, ?NXM_NX_REG2) -> nx_reg2;
nxm_field(nxm1, ?NXM_NX_REG3) -> nx_reg3;
nxm_field(nxm1, ?NXM_NX_REG4) -> nx_reg4;
nxm_field(nxm1, ?NXM_NX_REG5) -> nx_reg5;
nxm_field(nxm1, ?NXM_NX_REG6) -> nx_reg6;
nxm_field(nxm1, ?NXM_NX_REG7) -> nx_reg7;
nxm_field(nxm1, ?NXM_NX_TUN_ID) -> nx_tun_id;
nxm_field(nxm1, ?NXM_NX_ARP_SHA) -> nx_arp_sha;
nxm_field(nxm1, ?NXM_NX_ARP_THA) -> nx_arp_tha;
nxm_field(nxm1, ?NXM_NX_IPV6_SRC) -> nx_ipv6_src;
nxm_field(nxm1, ?NXM_NX_IPV6_DST) -> nx_ipv6_dst;
nxm_field(nxm1, ?NXM_NX_ICMPV6_TYPE) -> nx_icmpv6_type;
nxm_field(nxm1, ?NXM_NX_ICMPV6_CODE) -> nx_icmpv6_code;
nxm_field(nxm1, ?NXM_NX_ND_TARGET) -> nx_nd_target;
nxm_field(nxm1, ?NXM_NX_ND_SLL) -> nx_nd_sll;
nxm_field(nxm1, ?NXM_NX_ND_TLL) -> nx_nd_tll;
nxm_field(nxm1, ?NXM_NX_IP_FRAG) -> nx_ip_frag;
nxm_field(nxm1, ?NXM_NX_IPV6_LABEL) -> nx_ipv6_label;
nxm_field(nxm1, ?NXM_NX_IP_ECN) -> nx_ip_ecn;
nxm_field(nxm1, ?NXM_NX_IP_TTL) -> nx_ip_ttl;
nxm_field(nxm1, ?NXM_NX_COOKIE) -> nx_cookie;
nxm_field(nxm1, ?NXM_NX_TUN_IPV4_SRC) -> nx_tun_ipv4_src;
nxm_field(nxm1, ?NXM_NX_TUN_IPV4_DST) -> nx_tun_ipv4_dst;
nxm_field(nxm1, ?NXM_NX_PKT_MARK) -> nx_pkt_mark;
nxm_field(nxm1, ?NXM_NX_TCP_FLAGS) -> nx_tcp_flags;
nxm_field(nxm1, ?NXM_NX_DP_HASH) -> nx_dp_hash;
nxm_field(nxm1, ?NXM_NX_RECIRC_ID) -> nx_recirc_id.
