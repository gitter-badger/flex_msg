-module(flex_msg_v1_encode).

-export([do/1]).

-include("ofp_v1.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec do(Message :: ofp_message()) -> binary().
do(#ofp_header{ type = hello, xid = Xid, body = #ofp_hello{} }) ->
    Length = ?OFP_HELLO_SIZE,
    <<?VERSION:8, ?OFPT_HELLO:8, Length:16, Xid:32>>;
do(#ofp_header{ type = error, xid = Xid,
                body = #ofp_error_msg{ type = Type, code = Code, data = Data } }) ->
    Length = ?OFP_ERROR_MSG_SIZE + byte_size(Data),
    TypeInt = flex_msg_v1_enum:to_int(error_type, Type),
    CodeInt = flex_msg_v1_enum:to_int(Type, Code),
    Error = <<TypeInt:16, CodeInt:16, Data/bytes>>,
    <<?VERSION:8, ?OFPT_ERROR:8, Length:16, Xid:32, Error/bytes>>;
do(#ofp_header{ type = echo_request, xid = Xid,
                body = #ofp_echo_request{ data = Data } }) ->
    Length = ?OFP_ECHO_REQUEST_SIZE + byte_size(Data),
    <<?VERSION:8, ?OFPT_ECHO_REQUEST:8, Length:16, Xid:32, Data/bytes>>;
do(#ofp_header{ type = echo_reply, xid = Xid,
                body = #ofp_echo_reply{ data = Data } }) ->
    Length = ?OFP_ECHO_REPLY_SIZE + byte_size(Data),
    <<?VERSION:8, ?OFPT_ECHO_REPLY:8, Length:16, Xid:32, Data/bytes>>;
do(#ofp_header{ type = vendor, xid = Xid,
                body = #ofp_vendor_header{ vendor = Vendor, data = Data } }) ->
    Length = ?OFP_VENDOR_HEADER_SIZE + byte_size(Data),
    Body = <<Vendor:32, Data/bytes>>,
    <<?VERSION:8, ?OFPT_VENDOR:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = features_request, xid = Xid,
                body = #ofp_features_request{} }) ->
    Length = ?OFP_FEATURES_REQUEST_SIZE,
    <<?VERSION:8, ?OFPT_FEATURES_REQUEST:8, Length:16, Xid:32>>;
do(#ofp_header{ type = features_reply, xid = Xid,
                body = #ofp_switch_features{ datapath_id = Dpid, n_buffers = NBuffers,
                                             n_tables = NTables, capabilities = Cap,
                                             actions = Actions, ports = Ports } }) ->
    PortsBin = encode_ports(Ports),
    CapaBin = flags_to_binary(capabilities, Cap, 4),
    ActionsBin = flags_to_binary(actions, Actions, 4),
    BodyBin = <<Dpid:64/bits, NBuffers:32, NTables:8, 0:24,
                CapaBin/bytes, ActionsBin/bytes, PortsBin/bytes>>,
    Length = ?OFP_SWITCH_FEATURES_SIZE + byte_size(PortsBin),
    <<?VERSION:8, ?OFPT_FEATURES_REPLY:8, Length:16, Xid:32, BodyBin/bytes>>;
do(#ofp_header{ type = get_config_request, xid = Xid,
                body = #ofp_get_config_request{} }) ->
    Length = ?OFP_GET_CONFIG_REQUEST_SIZE,
    <<?VERSION:8, ?OFPT_GET_CONFIG_REQUEST:8, Length:16, Xid:32>>;
do(#ofp_header{ type = get_config_reply, xid = Xid,
                body = #ofp_switch_config{ flags = Flags,
                                           miss_send_len = MissSendLen } }) ->
    Length = ?OFP_SWITCH_CONFIG_SIZE,
    FlagsBin = flags_to_binary(config_flags, Flags, 4),
    Body = <<FlagsBin:2/bytes, MissSendLen:16>>,
    <<?VERSION:8, ?OFPT_GET_CONFIG_REPLY:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = set_config, xid = Xid,
                body = #ofp_switch_config{ flags = Flags,
                                           miss_send_len = MissSendLen } }) ->
    Length = ?OFP_SWITCH_CONFIG_SIZE,
    FlagsBin = flags_to_binary(config_flags, Flags, 4),
    Body = <<FlagsBin:2/bytes, MissSendLen:16>>,
    <<?VERSION:8, ?OFPT_SET_CONFIG:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = packet_in, xid = Xid,
                body = #ofp_packet_in{ buffer_id = BufferId, in_port = PortNo,
                                       reason = Reason, data = Data } }) ->
    TotalLen = byte_size(Data),
    Length = ?OFP_PACKET_IN_SIZE + TotalLen - 2,
    ReasonInt = get_id(packet_in_reason, Reason),
    BufferIdInt = get_id(max_len, BufferId),
    PortInt = get_id(port_no, PortNo),
    Body = <<BufferIdInt:32, TotalLen:16, PortInt:16, ReasonInt:8, 0:8, Data/bytes>>,
    <<?VERSION:8, ?OFPT_PACKET_IN:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = flow_removed, xid = Xid,
                body = #ofp_flow_removed{ match = Match, cookie = Cookie,
                                          priority = Prority, reason = Reason,
                                          duration_sec = Dsec, duration_nsec = Dnsec,
                                          idle_timeout = Idle, packet_count = Packet,
                                          byte_count = Byte } }) ->
    Length = ?OFP_FLOW_REMOVED_SIZE,
    ReasonInt = get_id(flow_removed_reason, Reason),
    MatchBin = encode_match(Match),
    Body = <<MatchBin/bytes, Cookie/bytes, Prority:16, ReasonInt:8, 0:8,
             Dsec:32, Dnsec:32, Idle:16, 0:16, Packet:64, Byte:64>>,
    <<?VERSION:8, ?OFPT_FLOW_REMOVED:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = port_status, xid = Xid,
                body = #ofp_port_status{ reason = Reason, desc = PhyPort } }) ->
    ReasonInt = get_id(port_reason, Reason),
    PhyPortBin = encode_port(PhyPort),
    Length = ?OFP_PORT_STATUS_SIZE,
    Body = <<ReasonInt:8, 0:56, PhyPortBin/bytes>>,
    <<?VERSION:8, ?OFPT_PORT_STATUS:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = packet_out, xid = Xid,
                body = #ofp_packet_out{ buffer_id = BufferId, in_port = PortNo,
                                        actions = Actions, data = Data } }) ->
    PortInt = get_id(port_no, PortNo),
    BufferIdInt = get_id(max_len, BufferId),
    ActionsBin = encode_actions(Actions),
    ActionsLen = byte_size(ActionsBin),
    Length = ?OFP_PACKET_OUT_SIZE + ActionsLen + byte_size(Data),
    Body = <<BufferIdInt:32, PortInt:16, ActionsLen:16,
             ActionsBin:(ActionsLen * 8)/bits, Data/bytes>>,
    <<?VERSION:8, ?OFPT_PACKET_OUT:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = flow_mod, xid = Xid,
                body = #ofp_flow_mod{ match = Match, cookie = Cookie, command = Command,
                                      idle_timeout = Idle, hard_timeout = Hard,
                                      priority = Priority, buffer_id = BufferId,
                                      out_port = OutPortNo, flags = Flags,
                                      actions = Actions }}) ->
    OutPortInt = get_id(port_no, OutPortNo),
    CommandInt = get_id(flow_mod_command, Command),
    FlagsBin = flags_to_binary(flow_mod_flags, Flags, 2),
    BufferIdInt = get_id(max_len, BufferId),
    MatchBin = encode_match(Match),
    ActionsBin = encode_actions(Actions),
    ActionsLen = byte_size(ActionsBin),
    Length = ?OFP_FLOW_MOD_SIZE + ActionsLen,
    Body = <<MatchBin:?OFP_MATCH_SIZE/bytes, Cookie:8/bytes, CommandInt:16, Idle:16,
             Hard:16, Priority:16, BufferIdInt:32, OutPortInt:16, FlagsBin:16/bits,
             ActionsBin/bytes>>,
    <<?VERSION:8, ?OFPT_FLOW_MOD:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = port_mod, xid = Xid,
                body = #ofp_port_mod{ port_no = PortNo, hw_addr = HwAddr,
                                      config = Config, mask = Mask,
                                      advertised = Advertise }}) ->
    PortNoInt = get_id(port_no, PortNo),
    ConfigBin = flags_to_binary(port_config, Config, 4),
    MaskBin = flags_to_binary(port_config, Mask, 4),
    AdvertiseBin = flags_to_binary(port_features, Advertise, 4),
    Length = ?OFP_PORT_MOD_SIZE,
    Body = <<PortNoInt:16, HwAddr:?OFP_ETH_ALEN/bytes, ConfigBin:4/bytes,
             MaskBin:4/bytes, AdvertiseBin:4/bytes, 0:32>>,
    <<?VERSION:8, ?OFPT_PORT_MOD:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = barrier_request, xid = Xid, body = #ofp_barrier_request{} }) ->
    Length = ?OFP_BARRIER_REQUEST_SIZE,
    <<?VERSION:8, ?OFPT_BARRIER_REQUEST:8, Length:16, Xid:32>>;
do(#ofp_header{ type = barrier_reply, xid = Xid, body = #ofp_barrier_reply{} }) ->
    Length = ?OFP_BARRIER_REPLY_SIZE,
    <<?VERSION:8, ?OFPT_BARRIER_REPLY:8, Length:16, Xid:32>>;
do(#ofp_header{ type = queue_get_config_request, xid = Xid,
               body = #ofp_queue_get_config_request{ port_no = PortNo } }) ->
    PortNoInt = get_id(port_no, PortNo),
    Length = ?OFP_QUEUE_GET_CONFIG_REQUEST_SIZE,
    Body = <<PortNoInt:16, 0:16>>,
    <<?VERSION:8, ?OFPT_QUEUE_GET_CONFIG_REQUEST:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = queue_get_config_reply, xid = Xid,
                body = #ofp_queue_get_config_reply{ port_no = PortNo,
                                                    queues = Queues } }) ->
    PortNoInt = get_id(port_no, PortNo),
    QueuesBin = encode_packet_queues(Queues),
    Length = ?OFP_QUEUE_GET_CONFIG_REPLY_SIZE + byte_size(QueuesBin),
    Body = <<PortNoInt:16, 0:48, QueuesBin/bytes>>,
    <<?VERSION:8, ?OFPT_QUEUE_GET_CONFIG_REPLY:8, Length:16, Xid:32, Body/bytes>>;
do(#ofp_header{ type = stats_request, xid = Xid,
                body = #ofp_stats_request{ type = Type, flags = Flags,
                                           body = RequestBody } }) ->
    TypeInt = get_id(stats_types, Type),
    FlagsBin = flags_to_binary(stats_request_flags, Flags, 2),
    RequestBodyBin = case Type of
                         desc ->
                             <<>>;
                         flow ->
                             #ofp_flow_stats_request{ match = Match,
                                                      table_id = TableId,
                                                      out_port = OutPort } = RequestBody,
                             MatchBin = encode_match(Match),
                             TableIdInt = get_id(table_id, TableId),
                             OutPortInt = get_id(port_no, OutPort),
                             <<MatchBin:?OFP_MATCH_SIZE/bytes, TableIdInt:8,
                               0:8, OutPortInt:16>>;
                          aggregate ->
                             #ofp_aggregate_stats_request{ match = Match,
                                                      table_id = TableId,
                                                      out_port = OutPort } = RequestBody,
                             MatchBin = encode_match(Match),
                             TableIdInt = get_id(table_id, TableId),
                             OutPortInt = get_id(port_no, OutPort),
                             <<MatchBin:?OFP_MATCH_SIZE/bytes, TableIdInt:8,
                               0:8, OutPortInt:16>>;
                         table ->
                             <<>>;
                         port ->
                             #ofp_port_stats_request{ port_no = PortNo } = RequestBody,
                             PortNoInt = get_id(port_no, PortNo),
                             <<PortNoInt:16, 0:48>>;
                         queue ->
                             #ofp_queue_stats_request{ port_no = PortNo,
                                                       queue_id = QueueId } = RequestBody,
                             QueueIdInt = get_id(queue_id, QueueId),
                             PortNoInt = get_id(port_no, PortNo),
                             <<PortNoInt:16, 0:16, QueueIdInt:32>>;
                          vendor ->
                             #ofp_vendor_stats_request{ vendor = VendorInt,
                                                        data = Data } = RequestBody,
                             <<VendorInt:32, Data/bytes>>
                     end,
    BodyBin = <<TypeInt:16, FlagsBin:2/bytes, RequestBodyBin/binary>>,
    Length = ?OFP_HEADER_SIZE + byte_size(BodyBin),
    <<?VERSION:8, ?OFPT_STATS_REQUEST:8, Length:16, Xid:32, BodyBin/bytes>>;
do(#ofp_header{ type = stats_reply, xid = Xid,
                body = #ofp_stats_reply{ type = Type, flags = Flags,
                                         body = ReplyBody } }) ->
    TypeInt = get_id(stats_types, Type),
    FlagsBin = flags_to_binary(stats_reply_flags, Flags, 2),
    ReplyBodyBin = case Type of
                         desc ->
                           encode_stats_reply(ReplyBody);
                         flow ->
                           encode_stats_reply_list(ReplyBody);
                         aggregate ->
                           encode_stats_reply(ReplyBody);
                         table ->
                           encode_stats_reply_list(ReplyBody);
                         port ->
                           encode_stats_reply_list(ReplyBody);
                         queue ->
                           encode_stats_reply_list(ReplyBody);
                         vendor ->
                           encode_stats_reply(ReplyBody)
                     end,
    BodyBin = <<TypeInt:16, FlagsBin:2/bytes, ReplyBodyBin/binary>>,
    Length = ?OFP_HEADER_SIZE + byte_size(BodyBin),
    <<?VERSION:8, ?OFPT_STATS_REPLY:8, Length:16, Xid:32, BodyBin/bytes>>.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

encode_stats_reply_list(List) -> encode_stats_reply_list(List, <<>>).

encode_stats_reply_list([], Binary) -> Binary;
encode_stats_reply_list([Stats | Rest], Binary) ->
    StatsBin = encode_stats_reply(Stats),
    encode_stats_reply_list(Rest, <<Binary/bytes, StatsBin/bytes>>).

encode_stats_reply(#ofp_desc_stats{ mfr_desc = MFR, hw_desc = HW,
                                   sw_desc = SW, serial_num = SN,
                                   dp_desc = DP }) ->
    MFRBin = flex_msg_v1_utils:encode_string(MFR, ?DESC_STR_LEN),
    HWBin = flex_msg_v1_utils:encode_string(HW, ?DESC_STR_LEN),
    SWBin = flex_msg_v1_utils:encode_string(SW, ?DESC_STR_LEN),
    SNBin = flex_msg_v1_utils:encode_string(SN, ?SERIAL_NUM_LEN),
    DPBin = flex_msg_v1_utils:encode_string(DP, ?DESC_STR_LEN),
    <<MFRBin:?DESC_STR_LEN/bytes, HWBin:?DESC_STR_LEN/bytes,
      SWBin:?DESC_STR_LEN/bytes, SNBin:?SERIAL_NUM_LEN/bytes,
      DPBin:?DESC_STR_LEN/bytes>>;
encode_stats_reply(#ofp_flow_stats{ table_id = TableId, match = Match,
                                   duration_sec = DSec, duration_nsec = DNSec,
                                   priority = Priority, idle_timeout = Idle,
                                   hard_timeout = Hard, cookie = Cookie,
                                   packet_count = Packet, byte_count = Byte,
                                   actions = Actions }) ->
    MatchBin = encode_match(Match),
    TableIdInt = get_id(table_id, TableId),
    ActionsBin = encode_actions(Actions),
    Length = ?OFP_FLOW_STATS_SIZE + byte_size(ActionsBin),
    <<Length:16, TableIdInt:8, 0:8, MatchBin:?OFP_MATCH_SIZE/bytes,
      DSec:32, DNSec:32, Priority:16, Idle:16, Hard:16,
      0:48, Cookie:8/bytes, Packet:64, Byte:64, ActionsBin/bytes>>;
encode_stats_reply(#ofp_aggregate_stats{ packet_count = Packet,
                                         byte_count = Byte,
                                         flow_count = Flow }) ->
    <<Packet:64, Byte:64, Flow:32, 0:32>>;
encode_stats_reply(#ofp_table_stats{ table_id = TableId, name = Name,
                                     wildcards = Wildcards, max_entries = Max,
                                     active_count = Active, lookup_count = LookUp,
                                     matched_count = Matched }) ->
    TableIdInt = get_id(table_id, TableId),
    NameBin = flex_msg_v1_utils:encode_string(Name, ?OFP_MAX_TABLE_NAME_LEN),
    <<TableIdInt:8, 0:24, NameBin:?OFP_MAX_TABLE_NAME_LEN/bytes,
      Wildcards:32, Max:32, Active:32, LookUp:64, Matched:64>>;
encode_stats_reply(#ofp_port_stats{ port_no = PortNo, rx_packets = RxPackets,
                                    tx_packets = TxPackets, rx_bytes = RxBytes,
                                    tx_bytes = TxBytes, rx_dropped = RxDropped,
                                    tx_dropped = TxDropped, rx_errors = RxErrors,
                                    tx_errors = TxErrors, rx_frame_err = RxFrameErr,
                                    rx_over_err = RxOverErr, rx_crc_err = RxCrcErr,
                                    collisions = Collisions }) ->
     PortNoInt = get_id(port_no, PortNo),
    <<PortNoInt:16, 0:48, RxPackets:64, TxPackets:64,
      RxBytes:64, TxBytes:64, RxDropped:64, TxDropped:64,
      RxErrors:64, TxErrors:64, RxFrameErr:64, RxOverErr:64,
      RxCrcErr:64, Collisions:64>>;
encode_stats_reply(#ofp_queue_stats{ port_no = PortNo, queue_id = QueueId,
                                     tx_bytes = TxBytes, tx_packets = TxPackets,
                                     tx_errors = TxErrors }) ->
    PortNoInt = get_id(port_no, PortNo),
    <<PortNoInt:16, 0:16, QueueId:32, TxBytes:64, TxPackets:64, TxErrors:64>>;
encode_stats_reply(#ofp_vendor_stats{ vendor = Vendor, data = Data }) ->
    <<Vendor:32, Data/bytes>>.

encode_match(#ofp_match{ wildcards = Barewildcards } = Match1) ->
    case Barewildcards =:= ?OFPFW_ALL of
        true ->
            match_to_binary(Match1);
        _    ->
            Match2 = set_wildcards(Match1, in_port, 0, ?OFPFW_IN_PORT),
            Match3 = set_wildcards(Match2, dl_src, <<0,0,0,0,0,0>>, ?OFPFW_DL_SRC),
            Match4 = set_wildcards(Match3, dl_dst, <<0,0,0,0,0,0>>, ?OFPFW_DL_DST),
            Match5 = set_wildcards(Match4, dl_type, 0, ?OFPFW_DL_TYPE),
            Match6 = set_wildcards(Match5, dl_vlan, 0, ?OFPFW_DL_VLAN),
            Match7 = set_wildcards(Match6, dl_vlan_pcp, 0, ?OFPFW_DL_VLAN),
            Match8 = set_wildcards(Match7, nw_tos, 0, ?OFPFW_NW_TOS),
            Match9 = set_wildcards(Match8, nw_proto, 0, ?OFPFW_NW_PROTO),
            Match10 = set_wildcards(Match9, tp_src, 0, ?OFPFW_TP_SRC),
            Match11 = set_wildcards(Match10, tp_dst, 0, ?OFPFW_TP_DST),
            Match12 = set_wildcards(Match11, nw_src, <<0,0,0,0>>, ?OFPFW_NW_SRC_MASK),
            Match13 = set_wildcards(Match12, nw_dst, <<0,0,0,0>>, ?OFPFW_NW_DST_MASK),
            match_to_binary(Match13)
    end.

encode_packet_queues(Queues) -> encode_packet_queues(Queues, <<>>).

encode_packet_queues([], Binary) -> Binary;
encode_packet_queues([Queue | Rest], Binary) ->
    QueueBin = encode_packet_queue(Queue),
    encode_packet_queues(Rest, <<Binary/bytes, QueueBin/bytes>>).

encode_packet_queue(#ofp_packet_queue{ queue_id = QueueId, properties = Props }) ->
    PropsBin = encode_queue_properties(Props),
    Length = ?OFP_PACKET_QUEUE_SIZE + byte_size(PropsBin),
    <<QueueId:32, Length:16, 0:16, PropsBin/bytes>>.

encode_queue_properties(Props) -> encode_queue_properties(Props, <<>>).

encode_queue_properties([], Binary) -> Binary;
encode_queue_properties([Prop | Rest], Binary) ->
    PropBin = encode_queue_property(Prop),
    encode_queue_properties(Rest, <<Binary/bytes, PropBin/bytes>>).

encode_queue_property(#ofp_queue_prop_header{ property = none }) ->
    <<?OFPQT_NONE:16, 8:16, 0:32>>;
encode_queue_property(#ofp_queue_prop_min_rate{ property = min_rate,
                                                rate = Rate }) ->
    <<?OFPQT_MIN_RATE:16, 16:16, 0:32, Rate:16, 0:48>>.

encode_actions(Actions) -> encode_actions(Actions, <<>>).

encode_actions([], Binary) -> Binary;
encode_actions([Action | Rest], Binary) ->
    ActionBin = encode_action(Action),
    encode_actions(Rest, <<Binary/bytes, ActionBin/bytes>>).

encode_action(#ofp_action_header{ body = #ofp_action_output{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    PortNo = Body#ofp_action_output.port,
    MaxLen = Body#ofp_action_output.max_len,
    PortInt = get_id(port_no, PortNo),
    ActionType = get_id(actions, Type),
    <<ActionType:16, 8:16, PortInt:16, MaxLen:16>>;
encode_action(#ofp_action_header{ body = #ofp_action_vlan_vid{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    VlanVid = Body#ofp_action_vlan_vid.vlan_vid,
    ActionType = get_id(actions, Type),
    <<ActionType:16, 8:16, VlanVid:16, 0:16>>;
encode_action(#ofp_action_header{ body = #ofp_action_vlan_pcp{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    VlanPcp = Body#ofp_action_vlan_pcp.vlan_pcp,
    ActionType = get_id(actions, Type),
    <<ActionType:16, 8:16, VlanPcp:8, 0:24>>;
encode_action(#ofp_action_header{ body = #ofp_action_strip_vlan{} } = Action) ->
    Type = Action#ofp_action_header.type,
    ActionType = get_id(actions, Type),
    <<ActionType:16, 8:16, 0:32>>;
encode_action(#ofp_action_header{ body = #ofp_action_dl_addr{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    DlAddr = Body#ofp_action_dl_addr.dl_addr,
    ActionType = get_id(actions, Type),
    <<ActionType:16, 16:16, DlAddr:?OFP_ETH_ALEN/bytes, 0:48>>;
encode_action(#ofp_action_header{ body = #ofp_action_nw_addr{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    NwAddr = Body#ofp_action_nw_addr.nw_addr,
    ActionType = get_id(actions, Type),
    <<ActionType:16, 8:16, NwAddr:32/bits>>;
encode_action(#ofp_action_header{ body = #ofp_action_nw_tos{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    NwTos = Body#ofp_action_nw_tos.nw_tos,
    ActionType = get_id(actions, Type),
    <<ActionType:16, 8:16, NwTos:8, 0:24>>;
encode_action(#ofp_action_header{ body = #ofp_action_tp_port{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    TpPort = Body#ofp_action_tp_port.tp_port,
    ActionType = get_id(actions, Type),
    <<ActionType:16, 8:16, TpPort:16, 0:16>>;
encode_action(#ofp_action_header{ body = #ofp_action_enqueue{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    PortNo = Body#ofp_action_enqueue.port,
    QueueID = Body#ofp_action_enqueue.queue_id,
    PortInt = get_id(port_no, PortNo),
    ActionType = get_id(actions, Type),
    <<ActionType:16, 16:16, PortInt:16, 0:48, QueueID:32>>;
encode_action(#ofp_action_header{ body = #ofp_action_vendor{} } = Action) ->
    Type = Action#ofp_action_header.type,
    Body = Action#ofp_action_header.body,
    Vendor = Body#ofp_action_vendor.vendor,
    Data = Body#ofp_action_vendor.data,
    Vendorlength = byte_size(Data) + 8,
    ActionType = get_id(actions, Type),
    <<ActionType:16, Vendorlength:16, Vendor:32, Data/bytes>>.

encode_ports(Ports) -> encode_ports(Ports, <<>>).

encode_ports([], Binary) -> Binary;
encode_ports([Port | Rest], Binary) ->
    PortBin = encode_port(Port),
    encode_ports(Rest, <<Binary/bytes, PortBin/bytes>>).

encode_port(#ofp_phy_port{ port_no = PortNo, hw_addr = HwAddr, name = Name,
                           config = Config, state = State, curr = Curr,
                           advertised = Advertised, supported = Supported, peer = Peer }) ->
    PortInt = get_id(port_no, PortNo),
    NameBin = flex_msg_v1_utils:encode_string(Name, ?OFP_MAX_PORT_NAME_LEN),
    ConfigBin = flags_to_binary(port_config, Config, 4),
    StateBin = flags_to_binary(port_state, State, 4),
    CurrBin = flags_to_binary(port_features, Curr, 4),
    AdvertisedBin = flags_to_binary(port_features, Advertised, 4),
    SupportedBin = flags_to_binary(port_features, Supported, 4),
    PeerBin = flags_to_binary(port_features, Peer, 4),
    <<PortInt:16, HwAddr:?OFP_ETH_ALEN/bytes, NameBin:?OFP_MAX_PORT_NAME_LEN/bytes,
      ConfigBin:4/bytes, StateBin:4/bytes, CurrBin:4/bytes,
      AdvertisedBin:4/bytes, SupportedBin:4/bytes, PeerBin:4/bytes>>.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

set_wildcards(Match, Key, Default, OFPFW) ->
    case Key of
        in_port ->
            if
                Match#ofp_match.in_port =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.in_port =:= undefined ->
                    Match#ofp_match{ in_port = Default }
            end;
        dl_vlan ->
            if
                Match#ofp_match.dl_vlan =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.dl_vlan =:= undefined ->
                    Match#ofp_match{ dl_vlan = Default }
            end;
        dl_src ->
            if
                Match#ofp_match.dl_src =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.dl_src =:= undefined ->
                    Match#ofp_match{ dl_src = Default }
            end;
        dl_dst ->
            if
                Match#ofp_match.dl_dst =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.dl_dst =:= undefined ->
                    Match#ofp_match{ dl_dst = Default }
            end;
        dl_type ->
            if
                Match#ofp_match.dl_type =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.dl_type =:= undefined ->
                    Match#ofp_match{ dl_type = Default }
            end;
        nw_proto ->
            if
                Match#ofp_match.nw_proto =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.nw_proto =:= undefined ->
                    Match#ofp_match{ nw_proto = Default }
            end;
        tp_src ->
            if
                Match#ofp_match.tp_src =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.tp_src =:= undefined ->
                    Match#ofp_match{ tp_src = Default }
            end;
        tp_dst ->
            if
                Match#ofp_match.tp_dst =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.tp_dst=:= undefined ->
                    Match#ofp_match{ tp_dst = Default }
            end;
        nw_src ->
            if
                Match#ofp_match.nw_src =/= undefined ->
                    if
                        Match#ofp_match.nw_src_mask > 0 ->
                            Wildcards1 = Match#ofp_match.wildcards,
                            NwSrcMask = Match#ofp_match.nw_src_mask,
                            Wildcards2 = Wildcards1 band bnot OFPFW,
                            Wildcards3 = Wildcards2 bor ((32 - NwSrcMask) bsl ?OFPFW_NW_SRC_SHIFT),
                            Match#ofp_match{ wildcards = Wildcards3 };
                        Match#ofp_match.nw_src_mask =:= 0 ->
                            Match#ofp_match{ nw_src = Default, nw_src_mask = 0 }
                    end;
                Match#ofp_match.nw_src =:= undefined ->
                    Match#ofp_match{ nw_src = Default, nw_src_mask = 0 }
            end;
        nw_dst ->
            if
                Match#ofp_match.nw_dst =/= undefined ->
                    if
                        Match#ofp_match.nw_dst_mask > 0 ->
                            Wildcards1 = Match#ofp_match.wildcards,
                            NwDstMask = Match#ofp_match.nw_dst_mask,
                            Wildcards2 = Wildcards1 band bnot OFPFW,
                            Wildcards3 = Wildcards2 bor ((32 - NwDstMask) bsl ?OFPFW_NW_DST_SHIFT),
                            Match#ofp_match{ wildcards = Wildcards3 };
                        Match#ofp_match.nw_src_mask =:= 0 ->
                            Match#ofp_match{ nw_src = Default, nw_src_mask = 0 }
                    end;
                Match#ofp_match.nw_dst =:= undefined ->
                    Match#ofp_match{ nw_dst = <<0,0,0,0>>, nw_dst_mask = 0 }
            end;
        dl_vlan_pcp ->
            if
                Match#ofp_match.dl_vlan_pcp =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.dl_vlan_pcp =:= undefined ->
                    Match#ofp_match{ dl_vlan_pcp = Default }
            end;
        nw_tos ->
            if
                Match#ofp_match.nw_tos =/= undefined ->
                    get_wildcard(Match, OFPFW);
                Match#ofp_match.nw_tos =:= undefined ->
                    Match#ofp_match{ nw_tos = Default }
            end
        end.

get_wildcard(#ofp_match{ wildcards = Wildcards } = Match, OFPFW) ->
    Match#ofp_match{ wildcards = Wildcards band (bnot OFPFW) }.

match_to_binary(#ofp_match{ wildcards = Wildcards, in_port = InPort,
                            dl_src = Dlsrc, dl_dst = Dldst,
                            dl_vlan = Vlan, dl_vlan_pcp = PCP,
                            dl_type = DlType, nw_tos = TOS,
                            nw_proto = NwProto, nw_src = NwSrc,
                            nw_dst = NwDst, tp_src = TpSrc,
                            tp_dst = TpDst }) ->
    PortInt = get_id(port_no, InPort),
    <<Wildcards:32, PortInt:16, Dlsrc/bytes,
      Dldst/bytes, Vlan:16, PCP:8, 0:8, DlType:16,
      TOS:8, NwProto:8, 0:16, NwSrc/bytes, NwDst/bytes,
      TpSrc:16, TpDst:16>>.

-spec get_id(atom(), integer() | atom()) -> integer() | atom().
get_id(Enum, Value) ->
    flex_msg_v1_utils:get_enum_value(flex_msg_v1_enum, Enum, Value).

-spec flags_to_binary(atom(), [atom()], integer()) -> binary().
flags_to_binary(Type, Flags, Size) ->
    flex_msg_v1_utils:flags_to_binary(flex_msg_v1_enum, Type, Flags, Size).
