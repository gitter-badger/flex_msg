-module(flex_msg_nx_encode).

-export([do/1, encode_action/1]).

-include("ofp_nx.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

do(#nicira_header{ sub_type = flow_mod_table_id,
                   body = #nx_flow_mod_table_id{ set = Set }}) ->
    SetInt = case Set of
                 true  -> 1;
                 false -> 0
             end,
    <<?NXT_FLOW_MOD_TABLE_ID:32, SetInt:8, 0:56>>;
do(#nicira_header{ sub_type = set_packet_in_format,
                   body = #nx_set_packet_in_format{ format = Format }}) ->
    FormatInt = case Format of
                    openflow10 -> ?NXPIF_OPENFLOW10;
                    nxm        -> ?NXPIF_NXM
                end,
    <<?NXT_SET_PACKET_IN_FORMAT:32, FormatInt:32>>;
do(#nicira_header{ sub_type = flow_mod,
                   body = #nx_flow_mod{ cookie = Cookie, table_id = TableId,
                                        command = Command, idle_timeout = Idle,
                                        hard_timeout = Hard, priority = Priority,
                                        out_port = OutPort, flags = Flags,
                                        buffer_id = BufferId, match = Match,
                                        actions = Actions }}) ->
    CommandInt = get_id(flow_mod_command, Command),
    FlagsBin = flags_to_binary(flow_mod_flags, Flags, 2),
    OutPortInt = get_id(port_no, OutPort),
    BufferIdInt = get_id(max_len, BufferId),
    MatchBin = encode_matches(Match),
    MatchLen = byte_size(MatchBin),
    ActionsBin = flex_msg_v1_encode:encode_actions(Actions),
    <<?NXT_FLOW_MOD:32, Cookie:8/bytes, TableId:8, CommandInt:8, Idle:16,
      Hard:16, Priority:16, BufferIdInt:32, OutPortInt:16,
      FlagsBin:2/bytes, MatchLen:16, 0:48, (pad_to(8, MatchBin))/bytes,
      ActionsBin/bytes>>;
do(#nicira_header{ sub_type = set_flow_format,
                   body = #nx_set_flow_format{ format = Format }}) ->
    FormatInt = case Format of
                    nxm        -> ?NXFF_NXM;
                    openflow10 -> ?NXFF_OPENFLOW10
                end,
    <<?NXT_SET_FLOW_FORMAT:32, FormatInt:32>>;
do(#nicira_header{ sub_type = role_request,
                   body = #nx_role{ role = Role }}) ->
    RoleInt = case Role of
                  other  -> ?NX_ROLE_OTHER;
                  master -> ?NX_ROLE_MASTER;
                  slave  -> ?NX_ROLE_SLAVE
              end,
    <<?NXT_ROLE_REQUEST:32, RoleInt:32>>;
do(#nicira_header{ sub_type = role_reply,
                   body = #nx_role{ role = Role }}) ->
    RoleInt = case Role of
                  other  -> ?NX_ROLE_OTHER;
                  master -> ?NX_ROLE_MASTER;
                  slave  -> ?NX_ROLE_SLAVE
              end,
    <<?NXT_ROLE_REPLY:32, RoleInt:32>>;
do(#nicira_header{ sub_type = set_controller_id,
                   body = #nx_controller_id{ id = Id }}) ->
    <<?NXT_SET_CONTROLLER_ID:32, 0:48, Id:16>>.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

encode_matches(Matches) -> encode_matches(Matches, <<>>).

encode_matches([], Binary) -> Binary;
encode_matches([Match | Rest], Binary) ->
    MatchBin = encode_match(Match),
    encode_matches(Rest, <<Binary/bytes, MatchBin/bytes>>).

encode_match(#oxm_field{ vendor = Vendor, field = Field,
                         value = Value, mask = Mask,
                         has_mask = HasMask }) ->
    case HasMask of
        false ->
            HasMaskInt = 0,
            BitLength = flex_msg_nx_map:nxm_field_bit_length(Field),
            VendorInt = nxm_vendor(Vendor),
            FieldInt = nxm_field(VendorInt, Field),
            ValueBin = <<Value:BitLength/bits>>,
            Length = byte_size(ValueBin),
            <<VendorInt:16, FieldInt:7, HasMaskInt:1, Length:8, ValueBin/bytes>>;
        true ->
            HasMaskInt = 1,
            BitLength = flex_msg_nx_map:nxm_field_bit_length(Field),
            VendorInt = nxm_vendor(Vendor),
            FieldInt = nxm_field(VendorInt, Field),
            ValueBin = <<Value:BitLength/bits, Mask:BitLength/bits>>,
            Length = byte_size(ValueBin),
            <<VendorInt:16, FieldInt:7, HasMaskInt:1, Length:8, ValueBin/bytes>>
    end.

encode_match_header(#nxm_field_header{vendor = Vendor, field = Field,
                                      has_mask = HasMask }) ->
    VendorInt = nxm_vendor(Vendor),
    FieldInt = nxm_field(VendorInt, Field),
    HasMaskInt = case HasMask of
                     true  -> 1;
                     false -> 0
                 end,
    BitLength = flex_msg_nx_map:nxm_field_bit_length(Field),
    ByteLength = (BitLength + 7) div 8,
    <<VendorInt:16, FieldInt:7, HasMaskInt:1, ByteLength:8>>.

encode_action(#nx_action_resubmit{ subtype = SubType,
                                   in_port = InPort, table_id = TableId }) ->
    SubTypeInt = case SubType of
                     resubmit       -> ?NXAST_RESUBMIT;
                     resubmit_table -> ?NXAST_RESUBMIT_TABLE
                 end,
    InPortInt = get_id(port_no, InPort),
    TableIdInt = get_id(table_id, TableId),
    <<SubTypeInt:16, InPortInt:16, TableIdInt:8, 0:24>>;
encode_action(#nx_action_learn{ idle_timeout = Idle, hard_timeout = Hard,
                                priority = Priority, cookie = Cookie,
                                flags = Flags, table_id = TableId,
                                fin_idle_timeout = FinIdle,
                                fin_hard_timeout = FinHard,
                                flow_mod_spec = FMS }) ->
    FMSBin = encode_flow_mod_specs(FMS),
    TableIdInt = get_id(table_id, TableId),
    FlagsBin = flags_to_binary(nx_learn_flags, Flags, 2),
    <<?NXAST_LEARN:16, Idle:16, Hard:16, Priority:16,
      Cookie:8/bytes, FlagsBin:2/bytes, TableIdInt:8,
      0:8, FinIdle:16, FinHard:16, FMSBin/bytes>>;
encode_action(#nx_action_note{ note = Note }) ->
    BodyBin = <<?NXAST_NOTE:16, Note/bytes>>,
    pad_to(8, BodyBin);
encode_action(#nx_action_set_tunnel{ tun_id = TunId }) ->
    <<?NXAST_SET_TUNNEL:16, 0:16, TunId:32>>;
encode_action(#nx_action_set_tunnel64{ tun_id = TunId }) ->
    <<?NXAST_SET_TUNNEL64:16, 0:48, TunId:64>>;
encode_action(#nx_action_set_queue{ queue_id = QueueId }) ->
    <<?NXAST_SET_QUEUE:16, 0:16, QueueId:32>>;
encode_action(#nx_action_pop_queue{}) ->
    <<?NXAST_POP_QUEUE:16, 0:48>>;
encode_action(#nx_action_fin_timeout{ fin_idle_timeout = FinIdle,
                                      fin_hard_timeout = FinHard }) ->
    <<?NXAST_FIN_TIMEOUT:16, FinIdle:16, FinHard:16, 0:16>>;
encode_action(#nx_action_reg_move{ n_bits = Nbits, src_offset = SrcOfs,
                                   dst_offset = DstOfs, src = SrcMatch,
                                   dst = DstMatch }) ->
    SrcMatchBin = encode_match_header(SrcMatch),
    DstMatchBin = encode_match_header(DstMatch),
    <<?NXAST_REG_MOVE:16, Nbits:16, SrcOfs:16, DstOfs:16,
      SrcMatchBin:4/bytes, DstMatchBin:4/bytes>>;
encode_action(#nx_action_reg_load{ offset_nbits = OffSet,
                                   dst = DstMatch, value = Value }) ->
    DstMatchBin = encode_match_header(DstMatch),
    <<?NXAST_REG_LOAD:16, OffSet:16, DstMatchBin:4/bytes, Value:64>>;
encode_action(#nx_action_write_metadata{ metadata = Metadata, mask = Mask }) ->
    <<?NXAST_WRITE_METADATA:16, 0:48, Metadata:8/bytes, Mask:8/bytes>>.

encode_flow_mod_specs(FMS) -> encode_flow_mod_specs(FMS, <<>>).

encode_flow_mod_specs([], Binary) ->
    FMSLen = byte_size(Binary),
    Padding = flex_msg_v1_utils:padding(FMSLen, 8) * 8,
    <<Binary/bytes, 0:Padding>>;
encode_flow_mod_specs([FMS | Rest], Binary) ->
    FMSBin = encode_flow_mod_spec(FMS),
    encode_flow_mod_specs(Rest, <<Binary/bytes, FMSBin/bytes>>).

encode_flow_mod_spec(#learn_match_field{ src = SrcMatch, dst = DstMatch }) ->
    Field = SrcMatch#nxm_field_header.field,
    NBits = flex_msg_nx_map:nxm_field_bit_length(Field),
    SrcMatchBin = encode_match_header(SrcMatch),
    DstMatchBin = encode_match_header(DstMatch),
    { Src, Dst } = nx_learn_flow_mod_spec(match_field),
    <<0:1, Src:2, Dst:2, NBits:11,
      SrcMatchBin/bytes, 0:16,
      DstMatchBin/bytes, 0:16>>;
encode_flow_mod_spec(#learn_immediate_field{ value = Value, dst = DstMatch }) ->
    Field = DstMatch#nxm_field_header.field,
    NBits = flex_msg_nx_map:nxm_field_bit_length(Field),
    DstMatchBin = encode_match_header(DstMatch),
    { Src, Dst } = nx_learn_flow_mod_spec(match_immediate),
    <<0:1, Src:2, Dst:2, NBits:11, Value:NBits/bits,
      DstMatchBin/bytes, 0:16>>;
encode_flow_mod_spec(#learn_load_field{src = SrcMatch, dst = DstMatch }) ->
    Field = SrcMatch#nxm_field_header.field,
    NBits = flex_msg_nx_map:nxm_field_bit_length(Field),
    SrcMatchBin = encode_match_header(SrcMatch),
    DstMatchBin = encode_match_header(DstMatch),
    { Src, Dst } = nx_learn_flow_mod_spec(load_field),
    <<0:1, Src:2, Dst:2, NBits:11,
      SrcMatchBin/bytes, 0:16,
      DstMatchBin/bytes, 0:16>>;
encode_flow_mod_spec(#learn_load_immediate_field{ value = Value,
                                                  dst = DstMatch }) ->
    Field = DstMatch#nxm_field_header.field,
    NBits = flex_msg_nx_map:nxm_field_bit_length(Field),
    DstMatchBin = encode_match_header(DstMatch),
    { Src, Dst } = nx_learn_flow_mod_spec(load_immediate),
    <<0:1, Src:2, Dst:2, NBits:11, Value:NBits/bits,
      DstMatchBin/bytes, 0:16>>;
encode_flow_mod_spec(#learn_output_action{ port = PortMatch }) ->
    Field = PortMatch#nxm_field_header.field,
    NBits = flex_msg_nx_map:nxm_field_bit_length(Field),
    PortMatchBin = encode_match_header(PortMatch),
    { Src, Dst } = nx_learn_flow_mod_spec(output_action),
    <<0:1, Src:2, Dst:2, NBits:11,
      PortMatchBin/bytes, 0:16>>.

-spec flags_to_binary(atom(), [atom()], integer()) -> binary().
flags_to_binary(Type, Flags, Size) ->
    flex_msg_v1_utils:flags_to_binary(flex_msg_v1_enum, Type, Flags, Size).

-spec get_id(atom(), integer() | atom()) -> integer() | atom().
get_id(Enum, Value) ->
    flex_msg_v1_utils:get_enum_value(flex_msg_v1_enum, Enum, Value).

pad_to(Width, Binary) ->
    case pad_length(Width, size(Binary)) of
        0 -> Binary;
        N -> <<Binary/binary, 0:(N*8)>>
    end.

pad_length(Width, Length) ->
     (Width - Length rem Width) rem Width.

nx_learn_flow_mod_spec(match_field) -> { 0, 0 };
nx_learn_flow_mod_spec(match_immediate) -> { 1, 0 };
nx_learn_flow_mod_spec(load_field) -> { 0, 1 };
nx_learn_flow_mod_spec(load_immediate) -> { 1, 1 };
nx_learn_flow_mod_spec(output_action) -> { 0, 2 }.

nxm_vendor(nxm0) -> ?NXM0;
nxm_vendor(nxm1) -> ?NXM1.

nxm_field(?NXM0, in_port) -> ?NXM_OF_IN_PORT;
nxm_field(?NXM0, eth_dst) -> ?NXM_OF_ETH_DST;
nxm_field(?NXM0, eth_src) -> ?NXM_OF_ETH_SRC;
nxm_field(?NXM0, eth_type) -> ?NXM_OF_ETH_TYPE;
nxm_field(?NXM0, vlan_tci) -> ?NXM_OF_VLAN_TCI;
nxm_field(?NXM0, ip_tos) -> ?NXM_OF_IP_TOS;
nxm_field(?NXM0, ip_proto) -> ?NXM_OF_IP_PROTO;
nxm_field(?NXM0, ip_src) -> ?NXM_OF_IP_SRC;
nxm_field(?NXM0, ip_dst) -> ?NXM_OF_IP_DST;
nxm_field(?NXM0, tcp_src) -> ?NXM_OF_TCP_SRC;
nxm_field(?NXM0, tcp_dst) -> ?NXM_OF_TCP_DST;
nxm_field(?NXM0, udp_src) -> ?NXM_OF_UDP_SRC;
nxm_field(?NXM0, udp_dst) -> ?NXM_OF_UDP_DST;
nxm_field(?NXM0, icmp_type) -> ?NXM_OF_ICMP_TYPE;
nxm_field(?NXM0, icmp_code) -> ?NXM_OF_ICMP_CODE;
nxm_field(?NXM0, arp_op) -> ?NXM_OF_ARP_OP;
nxm_field(?NXM0, arp_spa) -> ?NXM_OF_ARP_SPA;
nxm_field(?NXM0, arp_tpa) -> ?NXM_OF_ARP_TPA;
nxm_field(?NXM1, nx_reg0) -> ?NXM_NX_REG0;
nxm_field(?NXM1, nx_reg1) -> ?NXM_NX_REG1;
nxm_field(?NXM1, nx_reg2) -> ?NXM_NX_REG2;
nxm_field(?NXM1, nx_reg3) -> ?NXM_NX_REG3;
nxm_field(?NXM1, nx_reg4) -> ?NXM_NX_REG4;
nxm_field(?NXM1, nx_reg5) -> ?NXM_NX_REG5;
nxm_field(?NXM1, nx_reg6) -> ?NXM_NX_REG6;
nxm_field(?NXM1, nx_reg7) -> ?NXM_NX_REG7;
nxm_field(?NXM1, nx_tun_id) -> ?NXM_NX_TUN_ID;
nxm_field(?NXM1, nx_arp_sha) -> ?NXM_NX_ARP_SHA;
nxm_field(?NXM1, nx_arp_tha) -> ?NXM_NX_ARP_THA;
nxm_field(?NXM1, nx_ipv6_src) -> ?NXM_NX_IPV6_SRC;
nxm_field(?NXM1, nx_ipv6_dst) -> ?NXM_NX_IPV6_DST;
nxm_field(?NXM1, nx_icmpv6_type) -> ?NXM_NX_ICMPV6_TYPE;
nxm_field(?NXM1, nx_icmpv6_code) -> ?NXM_NX_ICMPV6_CODE;
nxm_field(?NXM1, nx_nd_target) -> ?NXM_NX_ND_TARGET;
nxm_field(?NXM1, nx_nd_sll) -> ?NXM_NX_ND_SLL;
nxm_field(?NXM1, nx_nd_tll) -> ?NXM_NX_ND_TLL;
nxm_field(?NXM1, nx_ip_frag) -> ?NXM_NX_IP_FRAG;
nxm_field(?NXM1, nx_ipv6_label) -> ?NXM_NX_IPV6_LABEL;
nxm_field(?NXM1, nx_ip_ecn) -> ?NXM_NX_IP_ECN;
nxm_field(?NXM1, nx_ip_ttl) -> ?NXM_NX_IP_TTL;
nxm_field(?NXM1, nx_cookie) -> ?NXM_NX_COOKIE;
nxm_field(?NXM1, nx_tun_ipv4_src) -> ?NXM_NX_TUN_IPV4_SRC;
nxm_field(?NXM1, nx_tun_ipv4_dst) -> ?NXM_NX_TUN_IPV4_DST;
nxm_field(?NXM1, nx_pkt_mark) -> ?NXM_NX_PKT_MARK;
nxm_field(?NXM1, nx_tcp_flags) -> ?NXM_NX_TCP_FLAGS;
nxm_field(?NXM1, nx_dp_hash) -> ?NXM_NX_DP_HASH;
nxm_field(?NXM1, nx_recirc_id) -> ?NXM_NX_RECIRC_ID.
