-module(flex_msg_v1_decode).

-export([do/1, decode_actions/1]).

-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec do(Binary :: binary()) -> { ok, ofp_message(), binary() }|
                                { error, any() }.
do(Binary) when ?OFP_HEADER_SIZE > byte_size(Binary) ->
    { error, binary_too_small };
do(<<_VERSION:8, _Type:8, Length:16, _/bytes>> = Binary) when Length > byte_size(Binary) ->
    { error, binary_too_small };
do(<<?VERSION:8, ?OFPT_HELLO:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<_:BodyLength/bytes, Rest/bytes>> = Binary2,
    DMsg = #ofp_header{ type = hello, xid = Xid, body = #ofp_hello{} },
    { ok, DMsg, Rest };
do(<<_:8, ?OFPT_HELLO:8, Length:16, Xid:32, Binary2/bytes>>) ->
    { error, { hello_failed, incompatible } };
do(<<?VERSION:8, ?OFPT_ERROR:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<TypeInt:16, CodeInt:16, Data/bytes>> = BodyBin,
    Type = flex_msg_v1_enum:to_atom(error_type, TypeInt),
    Code = flex_msg_v1_enum:to_atom(Type, CodeInt),
    Body = #ofp_error_msg{ type = Type, code = Code, data = Data },
    DMsg = #ofp_header{ type = error, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_ECHO_REQUEST:8, Length:16, Xid:32, Binary2/bytes>>) ->
    DataLength = Length - ?OFP_HEADER_SIZE,
    <<Data:DataLength/bytes, Rest/bytes>> = Binary2,
    Body = #ofp_echo_request{ data = Data },
    DMsg = #ofp_header{ type = echo_request, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_ECHO_REPLY:8, Length:16, Xid:32, Binary2/bytes>>) ->
    DataLength = Length - ?OFP_HEADER_SIZE,
    <<Data:DataLength/bytes, Rest/bytes>> = Binary2,
    Body = #ofp_echo_reply{ data = Data },
    DMsg = #ofp_header{ type = echo_reply, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_VENDOR:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<Vendor:32, Data/bytes>> = BodyBin,
    Body = case Vendor of
               ?NX_VENDOR_ID ->
                   #ofp_vendor_header{ vendor = nicira,
                                       data = flex_msg_nx_decode:do(Data) };
               _ ->
                   #ofp_vendor_header{ vendor = Vendor, data = Data }
           end,
    DMsg = #ofp_header{ type = vendor, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_FEATURES_REQUEST:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<_:BodyLength/bytes, Rest/bytes>> = Binary2,
    DMsg = #ofp_header{ type = features_request, xid = Xid,
                        body = #ofp_features_request{} },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_FEATURES_REPLY, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<Dpid:64/bits, NBuffers:32, NTables:8, _Pad:24,
      CapaBin:32/bits, ActionsBin:32/bits, PortsBin/bytes>> = BodyBin,
    Capabilities = binary_to_flags(capabilities, CapaBin),
    Actions = binary_to_flags(actions, ActionsBin),
    Ports = decode_ports(PortsBin),
    Body = #ofp_switch_features{ datapath_id = Dpid, n_buffers = NBuffers,
                                 n_tables = NTables, capabilities = Capabilities,
                                 actions = Actions, ports = Ports },
    DMsg = #ofp_header{ type = features_reply, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_GET_CONFIG_REQUEST:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<_:BodyLength/bytes, Rest/bytes>> = Binary2,
    DMsg = #ofp_header{ type = get_config_request, xid = Xid,
                        body = #ofp_get_config_request{} },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_GET_CONFIG_REPLY:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<FlagsBin:2/bytes, MissSendLen:16>> = BodyBin,
    Flags = binary_to_flags(config_flags, FlagsBin),
    Body = #ofp_switch_config{ flags = Flags, miss_send_len = MissSendLen },
    DMsg = #ofp_header{ type = get_config_reply, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_SET_CONFIG:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<FlagsBin:2/bytes, MissSendLen:16>> = BodyBin,
    Flags = binary_to_flags(config_flags, FlagsBin),
    Body = #ofp_switch_config{ flags = Flags, miss_send_len = MissSendLen },
    DMsg = #ofp_header{ type = set_config, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_PACKET_IN:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<BufferIdInt:32, TotalLen:16, PortInt:16, ReasonInt:8, _:8, Data/bytes>> = BodyBin,
    PortNo = get_id(port, PortInt),
    Reason = get_id(packet_in_reason, ReasonInt),
    BufferId = get_id(max_len, BufferIdInt),
    Body = #ofp_packet_in{ buffer_id = BufferId, total_len = TotalLen,
                           in_port = PortNo, reason = Reason, data = Data },
    DMsg = #ofp_header{ type = packet_in, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_FLOW_REMOVED:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<MatchBin:?OFP_MATCH_SIZE/bytes, Cookie:8/bytes, Prority:16, ReasonInt:8, _:8,
      Dsec:32, Dnsec:32, Idle:16, _:16, Packet:64, Byte:64>> = BodyBin,
    Reason = get_id(flow_removed_reason, ReasonInt),
    Match = decode_match(MatchBin),
    Body = #ofp_flow_removed{ match = Match, cookie = Cookie,
                              priority = Prority, reason = Reason,
                              duration_sec = Dsec, duration_nsec = Dnsec,
                              idle_timeout = Idle, packet_count = Packet,
                              byte_count = Byte },
    DMsg = #ofp_header{ type = flow_removed, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_PORT_STATUS:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<ReasonInt:8, _:7/bytes, PhyPortBin/bytes>> = BodyBin,
    PhyPort = decode_port(PhyPortBin),
    Reason = get_id(port_reason, ReasonInt),
    Body = #ofp_port_status{ reason = Reason, desc = PhyPort },
    DMsg = #ofp_header{ type = port_status, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_PACKET_OUT:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<BufferidInt:32, InPort:16, ActionsLen:16,
      ActionsBin:ActionsLen/bytes, Data/bytes>> = BodyBin,
    PortNo = get_id(port_no, InPort),
    BufferId = get_id(max_len, BufferidInt),
    Actions = decode_actions(ActionsBin),
    Body = #ofp_packet_out{ buffer_id = BufferId, in_port = PortNo,
                            actions = Actions, data = Data },
    DMsg = #ofp_header{ type = packet_out, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_FLOW_MOD:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<MatchBin:?OFP_MATCH_SIZE/bytes, Cookie:8/bytes, CommandInt:16, Idle:16,
    Hard:16, Priority:16, BufferIdInt:32, OutPortInt:16, FlagsBin:16/bits,
    ActionsBin/bytes>> = BodyBin,
    Command = get_id(flow_mod_command, CommandInt),
    Flags = binary_to_flags(flow_mod_flags, FlagsBin),
    OutPortNo = get_id(port_no, OutPortInt),
    BufferId = get_id(max_len, BufferIdInt),
    Match = decode_match(MatchBin),
    Actions = decode_actions(ActionsBin),
    Body = #ofp_flow_mod{ match = Match, cookie = Cookie, command = Command,
                          idle_timeout = Idle, hard_timeout = Hard,
                          priority = Priority, buffer_id = BufferId,
                          out_port = OutPortNo, flags = Flags, actions = Actions },
    DMsg = #ofp_header{ type = flow_mod, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_PORT_MOD:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<PortNoInt:16, HwAddr:?OFP_ETH_ALEN/bytes, ConfigBin:32/bits,
      MaskBin:32/bits, AdvertiseBin:32/bits, _:4/bytes>> = BodyBin,
    PortNo = get_id(port_no, PortNoInt),
    Config = binary_to_flags(port_config, ConfigBin),
    Mask = binary_to_flags(port_config, MaskBin),
    Advertise = binary_to_flags(port_features, AdvertiseBin),
    Body = #ofp_port_mod{ port_no = PortNo, hw_addr = HwAddr,
                          config = Config, mask = Mask, advertised = Advertise },
    DMsg = #ofp_header{ type = port_mod, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_BARRIER_REQUEST:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<_:BodyLength/bytes, Rest/bytes>> = Binary2,
    DMsg = #ofp_header{ type = barrier_request, xid = Xid, body = #ofp_barrier_request{} },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_BARRIER_REPLY:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<_:BodyLength/bytes, Rest/bytes>> = Binary2,
    DMsg = #ofp_header{ type = barrier_reply, xid = Xid, body = #ofp_barrier_reply{} },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_QUEUE_GET_CONFIG_REQUEST:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<PortNoInt:16, _:16>> = BodyBin,
    PortNo = get_id(port_no, PortNoInt),
    Body = #ofp_queue_get_config_request{ port_no = PortNo },
    DMsg = #ofp_header{ type = queue_get_config_request, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_QUEUE_GET_CONFIG_REPLY:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    <<PortNoInt:16, _:48, QueuesBin/bytes>> = BodyBin,
    PortNo = get_id(port_no, PortNoInt),
    Queues = decode_packet_queues(QueuesBin),
    Body = #ofp_queue_get_config_reply{ port_no = PortNo, queues = Queues },
    DMsg = #ofp_header{ type = queue_get_config_reply, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_STATS_REQUEST:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    RequestBodyLength = BodyLength - 4,
    <<TypeInt:16, FlagsBin:2/bytes, RequestBodyBin:RequestBodyLength/bytes,
      Rest/bytes>> = BodyBin,
    Type = get_id(stats_types, TypeInt),
    Flags = binary_to_flags(stats_request_flags, FlagsBin),
    RequestBody = case Type of
                      desc ->
                          #ofp_desc_stats_request{};
                      flow ->
                          <<MatchBin:?OFP_MATCH_SIZE/bytes, TableIdInt:8,
                            _:8, OutPortInt:16>> = RequestBodyBin,
                          Match = decode_match(MatchBin),
                          TableId = get_id(table_id, TableIdInt),
                          OutPort = get_id(port_no, OutPortInt),
                          #ofp_flow_stats_request{ match = Match,
                                                   table_id = TableId,
                                                   out_port = OutPort };
                      aggregate ->
                          <<MatchBin:?OFP_MATCH_SIZE/bytes, TableIdInt:8,
                            _:8, OutPortInt:16>> = RequestBodyBin,
                          Match = decode_match(MatchBin),
                          TableId = get_id(table_id, TableIdInt),
                          OutPort = get_id(port_no, OutPortInt),
                          #ofp_aggregate_stats_request{ match = Match,
                                                        table_id = TableId,
                                                        out_port = OutPort };
                      table ->
                          #ofp_table_stats_request{};
                      port ->
                          <<PortNoInt:16, _:6/bytes>> = RequestBodyBin,
                          PortNo = get_id(port_no, PortNoInt),
                          #ofp_port_stats_request{ port_no = PortNo };
                      queue ->
                          <<PortNoInt:16, _:2/bytes, QueueIdInt:32>> = RequestBodyBin,
                          QueueId = get_id(queue_id, QueueIdInt),
                          PortNo = get_id(port_no, PortNoInt),
                          #ofp_queue_stats_request{ port_no = PortNo, queue_id = QueueId };
                      vendor ->
                          <<VendorInt:32, Data/bytes>> = RequestBodyBin,
                          #ofp_vendor_stats_request{ vendor = VendorInt, data = Data }
                  end,
    Body = #ofp_stats_request{ type = Type, flags = Flags, body = RequestBody },
    DMsg = #ofp_header{ type = stats_request, xid = Xid, body = Body },
    { ok, DMsg, Rest };
do(<<?VERSION:8, ?OFPT_STATS_REPLY:8, Length:16, Xid:32, Binary2/bytes>>) ->
    BodyLength = Length - ?OFP_HEADER_SIZE,
    <<BodyBin:BodyLength/bytes, Rest/bytes>> = Binary2,
    ReplyBodyLength = BodyLength - 4,
    <<TypeInt:16, FlagsBin:2/bytes, ReplyBodyBin:ReplyBodyLength/bytes,
      Rest/bytes>> = BodyBin,
    Type = get_id(stats_types, TypeInt),
    Flags = binary_to_flags(stats_reply_flags, FlagsBin),
    ReplyBody = case Type of
                      desc ->
                        <<MFR:?DESC_STR_LEN/bytes, HW:?DESC_STR_LEN/bytes,
                          SW:?DESC_STR_LEN/bytes, SN:?SERIAL_NUM_LEN/bytes,
                          DP:?DESC_STR_LEN/bytes>> = ReplyBodyBin,
                        #ofp_desc_stats{ mfr_desc = flex_msg_v1_utils:strip_string(MFR),
                                         hw_desc = flex_msg_v1_utils:strip_string(HW),
                                         sw_desc = flex_msg_v1_utils:strip_string(SW),
                                         serial_num = flex_msg_v1_utils:strip_string(SN),
                                         dp_desc = flex_msg_v1_utils:strip_string(DP) };
                      flow ->
                        decode_flow_stats_list(ReplyBodyBin);
                      aggregate ->
                        <<Packet:64, Byte:64, Flow:32, _:4/bytes>> = ReplyBodyBin,
                        #ofp_aggregate_stats{ packet_count = Packet,
                                              byte_count = Byte,
                                              flow_count = Flow };
                      table ->
                        decode_table_stats_list(ReplyBodyBin);
                      port ->
                        decode_port_stats_list(ReplyBodyBin);
                      queue ->
                        decode_queue_stats_list(ReplyBodyBin);
                      vendor ->
                        <<Vendor:32, Data/bytes>> = ReplyBodyBin,
                        #ofp_vendor_stats{ vendor = Vendor,
                                           data = Data }
                end,
    Body = #ofp_stats_reply{ type = Type, flags = Flags, body = ReplyBody },
    DMsg = #ofp_header{ type = stats_reply, xid = Xid, body = Body },
    { ok, DMsg, Rest }.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

decode_table_stats_list(Binary) -> decode_table_stats_list(Binary, []).

decode_table_stats_list(<<>>, List) -> lists:reverse(List);
decode_table_stats_list(<<StatsBin:?OFP_TABLE_STATS_SIZE/bytes, Rest/bytes>>, List) ->
    Stats = decode_table_stats(StatsBin),
    decode_table_stats_list(Rest, [Stats | List]).

decode_table_stats(<<TableId:8, _:3/bytes, Name:?OFP_MAX_TABLE_NAME_LEN/bytes,
                     Wildcards:32, Max:32, Active:32, LookUp:64, Matched:64>>) ->
    #ofp_table_stats{ table_id = get_id(table_id, TableId),
                      name = flex_msg_v1_utils:strip_string(Name),
                      wildcards = Wildcards, max_entries = Max,
                      active_count = Active, lookup_count = LookUp,
                      matched_count = Matched }.

decode_queue_stats_list(Binary) -> decode_queue_stats_list(Binary, []).

decode_queue_stats_list(<<>>, List) -> lists:reverse(List);
decode_queue_stats_list(<<StatsBin:?OFP_QUEUE_STATS_SIZE/bytes, Rest/bytes>>, List) ->
    Stats = decode_queue_stats(StatsBin),
    decode_queue_stats_list(Rest, [Stats | List]).

decode_queue_stats(<<PortNoInt:16, _:2/bytes, QueueId:32,
                     TxBytes:64, TxPackets:64, TxErrors:64>>) ->
    PortNo = get_id(port_no, PortNoInt),
    #ofp_queue_stats{ port_no = PortNo,
                      queue_id = QueueId,
                      tx_bytes = TxBytes,
                      tx_packets = TxPackets,
                      tx_errors = TxErrors }.

decode_port_stats_list(Binary) -> decode_port_stats_list(Binary, []).

decode_port_stats_list(<<>>, List) -> lists:reverse(List);
decode_port_stats_list(<<StatsBin:?OFP_PORT_STATS_SIZE/bytes, Rest/bytes>>, List) ->
    Stats = decode_port_stats(StatsBin),
    decode_port_stats_list(Rest, [Stats | List]).

decode_port_stats(<<PortNoInt:16, _:6/bytes, RxPackets:64, TxPackets:64,
                    RxBytes:64, TxBytes:64, RxDropped:64, TxDropped:64,
                    RxErrors:64, TxErrors:64, RxFrameErr:64, RxOverErr:64,
                    RxCrcErr:64, Collisions:64>>) ->
    PortNo = get_id(port_no, PortNoInt),
    #ofp_port_stats{ port_no = PortNo, rx_packets = RxPackets,
                     tx_packets = TxPackets, rx_bytes = RxBytes,
                     tx_bytes = TxBytes, rx_dropped = RxDropped,
                     tx_dropped = TxDropped, rx_errors = RxErrors,
                     tx_errors = TxErrors, rx_frame_err = RxFrameErr,
                     rx_over_err = RxOverErr, rx_crc_err = RxCrcErr,
                     collisions = Collisions }.

decode_flow_stats_list(Binary) -> decode_flow_stats_list(Binary, []).

decode_flow_stats_list(<<>>, List) -> lists:reverse(List);
decode_flow_stats_list(<<Length:16, _/bytes>> = Binary, List) ->
    <<StatsBin:Length/bytes, Rest/bytes>> = Binary,
    Stats = decode_flow_stats(StatsBin),
    decode_flow_stats_list(Rest, [Stats | List]).

decode_flow_stats(<<_Len:16, TableIdInt:8, _:8, MatchBin:?OFP_MATCH_SIZE/bytes,
                    DSec:32, DNSec:32, Priority:16, Idle:16, Hard:16,
                    _:6/bytes, Cookie:8/bytes, Packet:64, Byte:64,
                    ActionsBin/bytes>>) ->
    Actions = decode_actions(ActionsBin),
    Match = decode_match(MatchBin),
    TableId = get_id(table_id, TableIdInt),
    #ofp_flow_stats{ table_id = TableId, match = Match,
                     duration_sec = DSec, duration_nsec = DNSec,
                     priority = Priority, idle_timeout = Idle,
                     hard_timeout = Hard, cookie = Cookie,
                     packet_count = Packet, byte_count = Byte,
                     actions = Actions }.

decode_actions(Binary) -> decode_actions(Binary, []).

decode_actions(<<>>, Actions) -> lists:reverse(Actions);
decode_actions(<<TypeInt:16, Length:16, _/bytes>> = Binary, Actions) ->
    <<ActionBin:Length/bytes, Rest/bytes>> = Binary,
    ActionBody = decode_action(ActionBin),
    Actiontype = get_id(actions, TypeInt),
    Action = #ofp_action_header{ type = Actiontype, body = ActionBody },
    decode_actions(Rest, [Action | Actions]).

decode_action(<<?OFPAT_OUTPUT:16, 8:16, PortInt:16, MaxLen:16>>) ->
    Port = get_id(port_no, PortInt),
    #ofp_action_output{ port = Port, max_len = MaxLen };
decode_action(<<?OFPAT_SET_VLAN_VID:16, 8:16, VlanVID:16, _:16>>) ->
    #ofp_action_vlan_vid{ vlan_vid = VlanVID };
decode_action(<<?OFPAT_SET_VLAN_PCP:16, 8:16, VlanPcp:8, _:24>>) ->
    #ofp_action_vlan_pcp{ vlan_pcp = VlanPcp };
decode_action(<<?OFPAT_STRIP_VLAN:16, 8:16, _:4/bytes>>) ->
    #ofp_action_strip_vlan{};
decode_action(<<?OFPAT_SET_DL_SRC:16, 16:16, DlAddr:48/bits, _:48>>) ->
    #ofp_action_dl_addr{ dl_addr = DlAddr };
decode_action(<<?OFPAT_SET_DL_DST:16, 16:16, DlAddr:48/bits, _:48>>) ->
    #ofp_action_dl_addr{ dl_addr = DlAddr };
decode_action(<<?OFPAT_SET_NW_SRC:16, 8:16, NwAddr:32/bits>>) ->
    #ofp_action_nw_addr{ nw_addr = NwAddr };
decode_action(<<?OFPAT_SET_NW_DST:16, 8:16, NwAddr:32/bits>>) ->
    #ofp_action_nw_addr{ nw_addr = NwAddr };
decode_action(<<?OFPAT_SET_NW_TOS:16, 8:16, NwTos:8, _:24/bits>>) ->
    #ofp_action_nw_tos{ nw_tos = NwTos };
decode_action(<<?OFPAT_SET_TP_SRC:16, 8:16, TpPort:16, _:16/bits>>) ->
    #ofp_action_tp_port{ tp_port = TpPort };
decode_action(<<?OFPAT_SET_TP_DST:16, 8:16, TpPort:16, _:16/bits>>) ->
    #ofp_action_tp_port{ tp_port = TpPort };
decode_action(<<?OFPAT_ENQUEUE:16, 16:16, PortInt:16, _:48/bits, QueueID:32>>) ->
    Port = get_id(port_no, PortInt),
    #ofp_action_enqueue{ port = Port, queue_id = QueueID };
decode_action(<<?OFPAT_VENDOR:16, _Length:16, Vendor:32, Data/bytes>>) ->
    case Vendor of
        ?NX_VENDOR_ID ->
            #ofp_action_vendor{ vendor = nicira,
                                data = flex_msg_nx_decode:decode_action(Data) };
        _ ->
            #ofp_action_vendor{ vendor = Vendor, data = Data }
    end.

decode_match(Binary) ->
    <<Wildcards:32/integer, InPort:16/integer,
      DlSrc:6/binary, DlDst:6/binary, DlVlan:16/integer, DlVlanPcp:8/integer,
      _Pad1:1/bytes, DlType:16/integer, NwTos:8/integer, NwProto:8/integer, _Pad2:2/bytes,
      NwSrc:4/bytes, NwDst:4/bytes, TpSrc:16/integer, TpDst:16/integer>> = Binary,
    NwSrcMask = get_match_nw_masklen(Wildcards, nw_src),
    NwDstMask = get_match_nw_masklen(Wildcards, nw_dst),
    #ofp_match{ wildcards = Wildcards, in_port = InPort,
                dl_src = DlSrc, dl_dst = DlDst,
                dl_vlan = DlVlan, dl_vlan_pcp = DlVlanPcp,
                dl_type = DlType, nw_tos = NwTos,
                nw_proto = NwProto, nw_src = NwSrc, nw_src_mask = NwSrcMask,
                nw_dst = NwDst, nw_dst_mask = NwDstMask, tp_src = TpSrc,
                tp_dst = TpDst }.

get_match_nw_masklen(Wildcards, Which) ->
    MaskLen = case Which of
                  nw_src ->
                      (Wildcards band ?OFPFW_NW_SRC_MASK) bsr ?OFPFW_NW_SRC_SHIFT;
                  nw_dst ->
                      (Wildcards band ?OFPFW_NW_DST_MASK) bsr ?OFPFW_NW_DST_SHIFT
              end,
    case MaskLen > 32 of
        true  -> 0;
        false -> 32 - MaskLen
    end.

decode_packet_queues(Binary) -> decode_packet_queues(Binary, []).

decode_packet_queues(<<>>, Queues) -> lists:reverse(Queues);
decode_packet_queues(<<_QueueId:32, Length:16, _/bytes>> = Binary, Queues) ->
    <<PacketQueueBin:Length/bytes, Rest/bytes>> = Binary,
    Queue = decode_packet_queue(PacketQueueBin),
    decode_packet_queues(Rest, [Queue | Queues]).

decode_packet_queue(<<QueueId:32, _Length:16, _:16, PropsBin/bytes>>) ->
    Props = decode_queue_properties(PropsBin),
    #ofp_packet_queue{ queue_id = QueueId, properties = Props }.

decode_queue_properties(Binary) -> decode_queue_properties(Binary, []).

decode_queue_properties(<<>>, Props) -> lists:reverse(Props);
decode_queue_properties(<<_Prop:16, Length:16, _/bytes>> = Binary, Props) ->
    <<PropBin:Length/bytes, Rest/bytes>> = Binary,
    Prop = decode_queue_property(PropBin),
    decode_queue_properties(Rest, [Prop | Props]).

decode_queue_property(<<?OFPQT_NONE:16, _Len:16, _:4/bytes>>) ->
    #ofp_queue_prop_header{};
decode_queue_property(<<?OFPQT_MIN_RATE:16, _Len:16, _:4/bytes, Rate:16, _:6/bytes>>) ->
    #ofp_queue_prop_min_rate{ rate = Rate }.

decode_ports(Binary) -> decode_ports(Binary, []).

decode_ports(<<>>, Ports)   -> lists:reverse(Ports);
decode_ports(Binary, Ports) ->
    <<PortBin:?OFP_PHY_PORT_SIZE/bytes, Rest/bytes>> = Binary,
    Port = decode_port(PortBin),
    decode_ports(Rest, [Port | Ports]).

decode_port(<<PortInt:16, HwAddr:?OFP_ETH_ALEN/bytes, NameBin:?OFP_MAX_PORT_NAME_LEN/bytes,
              ConfigBin:32/bits, StateBin:32/bits, CurrBin:32/bits,
              AdvertizedBin:32/bits, SupportedBin:32/bits, PeerBin:32/bits>>) ->
    PortNo = get_id(port_no, PortInt),
    Name = flex_msg_v1_utils:strip_string(NameBin),
    Config = binary_to_flags(port_config, ConfigBin),
    State = binary_to_flags(port_state, StateBin),
    Curr = binary_to_flags(port_features, CurrBin),
    Advertized = binary_to_flags(port_features, AdvertizedBin),
    Supported = binary_to_flags(port_features, SupportedBin),
    Peer = binary_to_flags(port_features, PeerBin),
    #ofp_phy_port{ port_no = PortNo, hw_addr = HwAddr, name = Name,
                   config = Config, state = State, curr = Curr,
                   advertised = Advertized, supported = Supported, peer = Peer }.

binary_to_flags(Type, Binary) ->
    flex_msg_v1_utils:binary_to_flags(flex_msg_v1_enum, Type, Binary).

get_id(Enum, Value) ->
    flex_msg_v1_utils:get_enum_name(flex_msg_v1_enum, Enum, Value).
