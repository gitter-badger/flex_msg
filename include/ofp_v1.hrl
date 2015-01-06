%% Protocol version
-define(VERSION, 1).

%%%-----------------------------------------------------------------------------
%%% Common Structures (5 2)
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Port Structures (5 2.1)
%%%-----------------------------------------------------------------------------

-define(OFP_PHY_PORT_SIZE, 48).
-define(OFP_MAX_PORT_NAME_LEN, 16).

-type ofp_port() :: max
                  | in_port
                  | table
                  | normal
                  | flood
                  | all
                  | controller
                  | local
                  | none
                  | integer().

-type ofp_port_config() :: port_down
                         | no_stp
                         | no_recv
                         | no_recv_stp
                         | no_flood
                         | no_fwd
                         | no_packet_in.

-type ofp_port_state() :: link_down
                        | stp_listen
                        | stp_learn
                        | stp_forward
                        | stp_block
                        | stp_mask.

-type ofp_port_feature() :: '10mb_hd'
                          | '10mb_fd'
                          | '100mb_hd'
                          | '100mb_fd'
                          | '1gb_hd'
                          | '1gb_fd'
                          | '10gb_fd'
                          | copper
                          | fibre
                          | autoneg
                          | pause
                          | pause_asym.

-record(ofp_phy_port, {
          port_no :: ofp_port(),
          hw_addr :: binary(),
          name :: binary(),
          config = [] :: [ofp_port_config()],
          state = [] :: [ofp_port_state()],
          curr = [] :: [ofp_port_feature()],
          advertised = [] :: [ofp_port_feature()],
          supported = [] :: [ofp_port_feature()],
          peer = [] :: [ofp_port_feature()],
          curr_speed = 0 :: integer(),
          max_speed = 0 :: integer() }).
-type ofp_phy_port() :: #ofp_phy_port{}.

%%%-----------------------------------------------------------------------------
%%% Queue Structures (5 2.2)
%%%-----------------------------------------------------------------------------

-define(OFP_PACKET_QUEUE_SIZE, 8).
-define(OFP_QUEUE_PROP_HEADER_SIZE, 8).
-define(OFP_QUEUE_PROP_MIN_RATE_SIZE, 16).

-define(OFPQT_NONE, 0).
-define(OFPQT_MIN_RATE, 1).

-type ofp_queue_properties() :: none
                              | min_rate.

-record(ofp_queue_prop_header, { property = none :: ofp_queue_properties() }).
-type ofp_queue_prop_header() :: #ofp_queue_prop_header{}.

-record(ofp_packet_queue, {
          queue_id :: integer(),
          properties :: ofp_queue_prop_header() | ofp_queue_prop_min_rate() }).
-type ofp_packet_queue() :: #ofp_packet_queue{}.

-record(ofp_queue_prop_min_rate, {
          property = min_rate :: ofp_queue_properties(),
          rate :: integer() }).
-type ofp_queue_prop_min_rate() :: #ofp_queue_prop_min_rate{}.

%%%-----------------------------------------------------------------------------
%%% Flow Match Structures (5 2.3)
%%%-----------------------------------------------------------------------------

-define(OFP_MATCH_SIZE, 40).
-define(OFP_ETH_ALEN, 6).

-define(OFPFW_IN_PORT,     16#00000001).
-define(OFPFW_DL_VLAN,     16#00000002).
-define(OFPFW_DL_SRC,      16#00000004).
-define(OFPFW_DL_DST,      16#00000008).
-define(OFPFW_DL_TYPE,     16#00000010).
-define(OFPFW_NW_PROTO,    16#00000020).
-define(OFPFW_TP_SRC,      16#00000040).
-define(OFPFW_TP_DST,      16#00000080).
-define(OFPFW_NW_SRC_SHIFT,          8).
-define(OFPFW_NW_SRC_BITS,           6).
-define(OFPFW_NW_SRC_MASK, 16#00003f00).
-define(OFPFW_NW_SRC_ALL,  16#00002000).
-define(OFPFW_NW_DST_SHIFT,         14).
-define(OFPFW_NW_DST_BITS,           6).
-define(OFPFW_NW_DST_MASK, 16#000fc000).
-define(OFPFW_NW_DST_ALL,  16#00080000).
-define(OFPFW_DL_VLAN_PCP, 16#00100000).
-define(OFPFW_NW_TOS,      16#00200000).
-define(OFPFW_ALL,         16#003fffff).

-type ofp_flow_wildcards() :: in_port
                            | dl_vlan
                            | dl_src
                            | dl_dst
                            | dl_type
                            | nw_proto
                            | tp_src
                            | tp_dst
                            | nw_src_mask
                            | nw_dst_mask
                            | dl_vlan_pcp
                            | nw_tos
                            | all.

-record(ofp_match, {
          wildcards = (?OFPFW_ALL band bnot(?OFPFW_NW_SRC_MASK bor ?OFPFW_NW_DST_MASK))
              bor ?OFPFW_NW_SRC_ALL bor ?OFPFW_NW_DST_ALL,
          in_port :: integer(),
          dl_src :: binary(),
          dl_dst :: binary(),
          dl_vlan :: integer(),
          dl_vlan_pcp :: integer(),
          dl_type :: integer(),
          nw_tos :: integer(),
          nw_proto :: integer(),
          nw_src :: binary(),
          nw_src_mask = 0 :: integer(),
          nw_dst :: binary(),
          nw_dst_mask = 0 :: integer(),
          tp_src :: integer(),
          tp_dst :: integer() }).
-type ofp_match() :: #ofp_match{}.

%%%-----------------------------------------------------------------------------
%%% Flow Action Structures (5 2.4)
%%%-----------------------------------------------------------------------------

-define(OFP_ACTION_HEADER_SIZE,   8).
-define(OFP_ACTION_OUTPUT_SIZE,   8).
-define(OFP_ACTION_ENQUEUE_SIZE,  16).
-define(OFP_ACTION_VLAN_VID_SIZE, 8).
-define(OFP_ACTION_VLAN_PCP_SIZE, 8).
-define(OFP_ACTION_DL_ADDR_SIZE,  16).
-define(OFP_ACTION_NW_ADDR_SIZE,  8).
-define(OFP_ACTION_NW_TOS_SIZE,   8).
-define(OFP_ACTION_TP_PORT_SIZE,  8).
-define(OFP_ACTION_VENDOR_SIZE,   8).

-define(OFPAT_OUTPUT,       0).
-define(OFPAT_SET_VLAN_VID, 1).
-define(OFPAT_SET_VLAN_PCP, 2).
-define(OFPAT_STRIP_VLAN,   3).
-define(OFPAT_SET_DL_SRC,   4).
-define(OFPAT_SET_DL_DST,   5).
-define(OFPAT_SET_NW_SRC,   6).
-define(OFPAT_SET_NW_DST,   7).
-define(OFPAT_SET_NW_TOS,   8).
-define(OFPAT_SET_TP_SRC,   9).
-define(OFPAT_SET_TP_DST,   10).
-define(OFPAT_ENQUEUE,      11).
-define(OFPAT_VENDOR,       16#ffff).

-type ofp_action_type() :: output
                         | set_vlan_vid
                         | set_vlan_pcp
                         | strip_vlan
                         | set_dl_src
                         | set_dl_dst
                         | set_nw_src
                         | set_nw_dst
                         | set_nw_tos
                         | set_tp_src
                         | set_tp_dst
                         | enqueue
                         | vendor.

-record(ofp_action_output, {
          port :: ofp_port(),
          max_len = 65535 :: integer() }).
-type ofp_action_output() :: #ofp_action_output{}.

-record(ofp_action_enqueue, {
          port :: ofp_port(),
          queue_id :: integer() }).
-type ofp_action_enqueue() :: #ofp_action_enqueue{}.

-record(ofp_action_vlan_vid, {
          vlan_vid :: integer() }).
-type ofp_action_vlan_vid() :: #ofp_action_vlan_vid{}.

-record(ofp_action_vlan_pcp, {
          vlan_pcp :: integer() }).
-type ofp_action_vlan_pcp() :: #ofp_action_vlan_pcp{}.

-record(ofp_action_strip_vlan, {}).
-type ofp_action_strip_vlan() :: #ofp_action_strip_vlan{}.

-record(ofp_action_dl_addr, {
          dl_addr :: binary() }).
-type ofp_action_dl_addr() :: #ofp_action_dl_addr{}.

-record(ofp_action_nw_addr, {
          nw_addr :: binary() }).
-type ofp_action_nw_addr() :: #ofp_action_nw_addr{}.

-record(ofp_action_nw_tos, {
          nw_tos :: integer() }).
-type ofp_action_nw_tos() :: #ofp_action_nw_tos{}.

-record(ofp_action_tp_port, {
          tp_port :: integer() }).
-type ofp_action_tp_port() :: #ofp_action_tp_port{}.

-record(ofp_action_vendor, {
          vendor :: integer(),
          data = <<>> :: binary() }).
-type ofp_action_vendor() :: #ofp_action_vendor{}.

-record(ofp_action_header, {
          type :: ofp_action_type(),
          body }).
-type ofp_action_header() :: #ofp_action_header{}.

%%%-----------------------------------------------------------------------------
%%% Controller-to-Switch Messages (5.3)
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Handshake (5 3.1)
%%%-----------------------------------------------------------------------------

-define(OFP_SWITCH_FEATURES_SIZE, 32).
-define(OFP_FEATURES_REQUEST_SIZE, 8).

-type ofp_capabilities() :: flow_stats
                          | table_stats
                          | port_stats
                          | stp
                          | reserved
                          | ip_reasm
                          | queue_stats
                          | arp_match_ip.

-record(ofp_switch_features, {
          datapath_id :: binary(),
          n_buffers :: integer(),
          n_tables :: integer(),
          capabilities = [] :: [ofp_capabilities()],
          actions = [] :: [ofp_action_type()],
          ports = [] :: [ofp_phy_port()] }).
-type ofp_switch_features() :: #ofp_switch_features{}.

-record(ofp_features_request, {}).
-type ofp_features_request() :: #ofp_features_request{}.

%%%-----------------------------------------------------------------------------
%%% Switch Configuration (5.3.2)
%%%-----------------------------------------------------------------------------

-define(OFP_SWITCH_CONFIG_SIZE, 12).
-define(OFP_GET_CONFIG_REQUEST_SIZE, 8).

-type ofp_config_flags() :: normal
                          | drop
                          | reasm
                          | mask.

-type ofp_buffer_id() :: integer()
                       | no_buffer.

-record(ofp_switch_config, {
          flags :: [ofp_config_flags()],
          miss_send_len :: integer() }).
-type ofp_switch_config() :: #ofp_switch_config{}.

-record(ofp_get_config_request, {}).
-type ofp_get_config_request() :: #ofp_get_config_request{}.

%%%-----------------------------------------------------------------------------
%%% Modify State Message (5.3.3)
%%%----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Modify Flow Entry Message
%%%-----------------------------------------------------------------------------

-define(OFP_FLOW_MOD_SIZE, 72).

-type ofp_flow_mod_command() :: add
                              | modify
                              | modify_strict
                              | delete
                              | delete_strict.

-type ofp_flow_mod_flags() :: send_flow_reasm
                            | check_overlap
                            | emerg.

-record(ofp_flow_mod, {
          match = #ofp_match{} :: ofp_match(),
          cookie = <<0:64>> :: binary(),
          command :: ofp_flow_mod_command(),
          idle_timeout = 0 :: integer(),
          hard_timeout = 0 :: integer(),
          priority = 65535 :: integer(),
          buffer_id = no_buffer :: ofp_buffer_id(),
          out_port = none :: ofp_port(),
          flags = [] :: [ofp_flow_mod_flags()],
          actions = [] :: [ofp_action_header()] }).
-type ofp_flow_mod() :: #ofp_flow_mod{}.

%%%-----------------------------------------------------------------------------
%%% Port Modification Message
%%%-----------------------------------------------------------------------------

-define(OFP_PORT_MOD_SIZE, 32).

-record(ofp_port_mod, {
          port_no :: ofp_port(),
          hw_addr :: binary(),
          config = [] :: [ofp_port_config()],
          mask = [] :: [ofp_port_config()],
          advertised = [] :: [ofp_port_feature()] }).
-type ofp_port_mod() :: #ofp_port_mod{}.

%%%-----------------------------------------------------------------------------
%%% Queue Configuration Message (5.3.4)
%%%-----------------------------------------------------------------------------

-define(OFP_QUEUE_GET_CONFIG_REQUEST_SIZE, 12).
-define(OFP_QUEUE_GET_CONFIG_REPLY_SIZE, 16).

-record(ofp_queue_get_config_request, { port_no :: ofp_port() }).
-type ofp_queue_get_config_request() :: #ofp_queue_get_config_request{}.

-record(ofp_queue_get_config_reply, {
          port_no :: ofp_port(),
          queues = [] :: [ofp_packet_queue()] }).
-type ofp_queue_get_config_reply() :: #ofp_queue_get_config_reply{}.

%%%-----------------------------------------------------------------------------
%%% Read State Message (5.3.5)
%%%-----------------------------------------------------------------------------

-define(OFP_STATS_REQUEST_SIZE,           12).
-define(OFP_STATS_REPLY_SIZE,             12).
-define(OFP_DESC_STATS_SIZE,            1056).
-define(OFP_FLOW_STATS_REQUEST_SIZE,      44).
-define(OFP_FLOW_STATS_SIZE,              88).
-define(OFP_AGGREGATE_STATS_REQUEST_SIZE, 44).
-define(OFP_AGGREGATE_STATS_SIZE,         24).
-define(OFP_TABLE_STATS_SIZE,             64).
-define(OFP_PORT_STATS_REQUEST_SIZE,       8).
-define(OFP_PORT_STATS_SIZE,             104).
-define(OFP_QUEUE_STATS_REQUEST_SIZE,      8).
-define(OFP_QUEUE_STATS_SIZE,             32).
-define(OFP_VENDOR_STATS_SIZE,             8).

-define(OFPST_DESC, 0).
-define(OFPST_FLOW, 1).
-define(OFPST_AGGREGATE, 2).
-define(OFPST_TABLE, 3).
-define(OFPST_PORT, 4).
-define(OFPST_QUEUE, 5).
-define(OFPST_VENDOR, 16#ffff).

-type ofp_stats_types() :: desc
                         | flow
                         | aggregate
                         | table
                         | port
                         | queue
                         | vendor.

-type ofp_table_id() :: all
                      | emergency
                      | integer().

-type ofp_stats_request_flag() :: more.
-type ofp_stats_reply_flag() :: more.

-record(ofp_stats_request, {
          type :: ofp_stats_types(),
          flags = [] :: [ofp_stats_request_flag()],
          body }).
-type ofp_stats_request() :: #ofp_stats_request{}.

-record(ofp_stats_reply, {
          type :: ofp_stats_types(),
          flags = [] :: [ofp_stats_reply_flag()],
          body }).
-type ofp_stats_reply() :: #ofp_stats_reply{}.

-record(ofp_desc_stats_request, {}).
-type ofp_desc_stats_request() :: #ofp_desc_stats_request{}.

-define(DESC_STR_LEN, 256).
-define(SERIAL_NUM_LEN, 32).

-record(ofp_desc_stats, {
          mfr_desc = <<>>,
          hw_desc = <<>>,
          sw_desc = <<>>,
          serial_num = <<>>,
          dp_desc = <<>> }).
-type ofp_desc_stats() :: #ofp_desc_stats{}.

-record(ofp_flow_stats_request, {
          match :: ofp_match(),
          table_id = all :: ofp_table_id(),
          out_port = none :: ofp_port() }).
-type ofp_flow_stats_request() :: #ofp_flow_stats_request{}.

-record(ofp_flow_stats, {
          table_id :: ofp_table_id(),
          match :: ofp_match(),
          duration_sec :: integer(),
          duration_nsec :: integer(),
          priority :: integer(),
          idle_timeout :: integer(),
          hard_timeout :: integer(),
          cookie :: binary(),
          packet_count :: integer(),
          byte_count :: integer(),
          actions = [] :: ofp_action_header() }).
-type ofp_flow_stats() :: #ofp_flow_stats{}.

-record(ofp_aggregate_stats_request, {
          match :: ofp_match(),
          table_id = all :: ofp_table_id(),
          out_port = none :: ofp_port() }).
-type ofp_aggregate_stats_request() :: #ofp_aggregate_stats_request{}.

-record(ofp_aggregate_stats, {
          packet_count = 0 :: integer(),
          byte_count = 0 :: integer(),
          flow_count = 0 :: integer() }).
-type ofp_aggregate_stats() :: #ofp_aggregate_stats{}.

-record(ofp_table_stats_request, {}).
-type ofp_table_stats_request() :: #ofp_table_stats_request{}.

-define(OFP_MAX_TABLE_NAME_LEN, 32).

-record(ofp_table_stats, {
          table_id :: ofp_table_id(),
          name = <<>> ,
          wildcards = 0 :: integer(),
          max_entries = 0 :: integer(),
          active_count = 0 :: integer(),
          lookup_count = 0 :: integer(),
          matched_count = 0 :: integer() }).
-type ofp_table_stats() :: #ofp_table_stats{}.

-record(ofp_port_stats_request, { port_no = none :: ofp_port() }).
-type ofp_port_stats_request() :: #ofp_port_stats_request{}.

-record(ofp_port_stats, {
          port_no :: ofp_port(),
          rx_packets = 0:: integer(),
          tx_packets = 0:: integer(),
          rx_bytes = 0 :: integer(),
          tx_bytes = 0 :: integer(),
          rx_dropped = 0 :: integer(),
          tx_dropped = 0 :: integer(),
          rx_errors = 0 :: integer(),
          tx_errors = 0 :: integer(),
          rx_frame_err = 0 :: integer(),
          rx_over_err = 0 :: integer(),
          rx_crc_err = 0 :: integer(),
          collisions = 0 :: integer() }).
-type ofp_port_stats() :: #ofp_port_stats{}.

-record(ofp_queue_stats_request, {
          port_no = all :: ofp_port(),
          queue_id = all }).
-type ofp_queue_stats_request() :: #ofp_queue_stats_request{}.

-record(ofp_queue_stats, {
          port_no :: ofp_port(),
          queue_id :: integer(),
          tx_bytes = 0 :: integer(),
          tx_packets = 0 :: integer(),
          tx_errors = 0 :: integer() }).
-type ofp_queue_stats() :: #ofp_queue_stats{}.

-record(ofp_vendor_stats_request, { vendor :: integer(),
                                    data = <<>> :: binary() }).
-type ofp_vendor_stats_request() :: #ofp_vendor_stats_request{}.

-record(ofp_vendor_stats, { vendor :: integer(),
                            data = <<>> :: binary() }).
-type ofp_vendor_stats() :: #ofp_vendor_stats{}.

%%%-----------------------------------------------------------------------------
%%% Send Packet Message (5.3.6)
%%%-----------------------------------------------------------------------------

-define(OFP_PACKET_OUT_SIZE, 16).

-record(ofp_packet_out, {
          buffer_id = no_buffer :: ofp_buffer_id(),
          in_port = none :: ofp_port(),
          actions = [] :: [ofp_action_header()],
          data = <<>> }).
-type ofp_packet_out() :: #ofp_packet_out{}.

%%%-----------------------------------------------------------------------------
%%% Barrier Message (5.3.7)
%%%-----------------------------------------------------------------------------

-define(OFP_BARRIER_REQUEST_SIZE, 8).
-define(OFP_BARRIER_REPLY_SIZE, 8).

-record(ofp_barrier_request, { header :: ofp_header() }).
-type ofp_barrier_request() :: #ofp_barrier_request{}.

-record(ofp_barrier_reply, { header :: ofp_header() }).
-type ofp_barrier_reply() :: #ofp_barrier_reply{}.

%%%-----------------------------------------------------------------------------
%%% Asynchronous Messages (5.4)
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Packet-In Message (5.4.1)
%%%-----------------------------------------------------------------------------

-define(OFP_PACKET_IN_SIZE, 20).

-type ofp_packet_in_reason() :: no_match
                              | action.

-record(ofp_packet_in, {
          buffer_id :: ofp_buffer_id(),
          total_len :: integer(),
          in_port :: ofp_port(),
          reason :: ofp_packet_in_reason(),
          data :: binary() }).
-type ofp_packet_in() :: #ofp_packet_in{}.

%%%-----------------------------------------------------------------------------
%%% Flow Removed Message (5.4.2)
%%%-----------------------------------------------------------------------------

-define(OFP_FLOW_REMOVED_SIZE, 88).

-type ofp_flow_removed_reason() :: idle_timeout
                                 | hard_timeout
                                 | delete.

-record(ofp_flow_removed, {
          match :: ofp_match(),
          cookie :: binary(),
          priority :: integer(),
          reason :: ofp_flow_removed_reason(),
          duration_sec :: integer(),
          duration_nsec :: integer(),
          idle_timeout :: integer(),
          packet_count :: integer(),
          byte_count :: integer() }).
-type ofp_flow_removed() :: #ofp_flow_removed{}.

%%%-----------------------------------------------------------------------------
%%% Port Status Message (5.4.3)
%%%-----------------------------------------------------------------------------

-define(OFP_PORT_STATUS_SIZE, 64).

-type ofp_port_reason() :: add
                         | delete
                         | modify.

-record(ofp_port_status, {
          reason :: ofp_port_reason(),
          desc :: ofp_phy_port() }).
-type ofp_port_status() :: #ofp_port_status{}.

%%%-----------------------------------------------------------------------------
%%% Error Message (5.4.4)
%%%-----------------------------------------------------------------------------

-define(OFP_ERROR_MSG_SIZE, 12).

-type ofp_error_type() :: hello_failed
                        | bad_request
                        | bad_action
                        | flow_mod_failed
                        | port_mod_failed
                        | queue_op_failed.

-type ofp_hello_failed_code() :: incompatible
                               | eperm.

-type ofp_bad_request_code() :: bad_version
                              | bad_type
                              | bad_stat
                              | bad_vendor
                              | bad_subtype
                              | eperm
                              | bad_len
                              | buffer_empty
                              | buffer_unknown.

-type ofp_bad_action_code() :: bad_type
                             | bad_len
                             | bad_vendor
                             | bad_vendor_type
                             | bad_out_port
                             | bad_argument
                             | eperm
                             | too_many
                             | bad_queue.

-type ofp_flow_mod_failed_code() :: all_tables_full
                                  | overlap
                                  | eperm
                                  | bad_emerg_timeout
                                  | bad_command
                                  | unsupported.

-type ofp_port_mod_failed_code() :: bad_port
                                  | bad_hw_addr.

-type ofp_queue_op_failed_code() :: bad_port
                                  | bad_queue
                                  | eperm.

-type ofp_error_code() :: ofp_hello_failed_code()
                        | ofp_bad_request_code()
                        | ofp_bad_action_code()
                        | ofp_flow_mod_failed_code()
                        | ofp_port_mod_failed_code()
                        | ofp_queue_op_failed_code().

-record(ofp_error_msg, {
          type :: ofp_error_type(),
          code :: ofp_error_code(),
          data = <<>> :: binary() }).
-type ofp_error_msg() :: #ofp_error_msg{}.

%%%-----------------------------------------------------------------------------
%%% Symmetric Messages (5.5)
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Hello (5.5.1)
%%%-----------------------------------------------------------------------------

-define(OFP_HELLO_SIZE, 8).

-record(ofp_hello, {}).
-type ofp_hello() :: #ofp_hello{}.

%%%-----------------------------------------------------------------------------
%%% Echo Request (5.5.1)
%%%-----------------------------------------------------------------------------

-define(OFP_ECHO_REQUEST_SIZE, 8).

-record(ofp_echo_request, { data = <<>> :: binary() }).
-type ofp_echo_request() :: #ofp_echo_request{}.

%%%-----------------------------------------------------------------------------
%%% Echo Reply (5.5.2)
%%%-----------------------------------------------------------------------------

-define(OFP_ECHO_REPLY_SIZE, 8).

-record(ofp_echo_reply, { data = <<>> :: binary() }).
-type ofp_echo_reply() :: #ofp_echo_reply{}.

%%%-----------------------------------------------------------------------------
%%% Vendor (5.5.3)
%%%-----------------------------------------------------------------------------

-define(OFP_VENDOR_HEADER_SIZE, 12).

-record(ofp_vendor_header, {
          vendor :: integer(),
          data = <<>> :: binary() }).
-type ofp_vendor_header() :: #ofp_vendor_header{}.

%%%-----------------------------------------------------------------------------
%%% OpenFlow Header(5.1)
%%%-----------------------------------------------------------------------------

-define(OFP_HEADER_SIZE, 8).

-define(OFPT_HELLO, 0).
-define(OFPT_ERROR, 1).
-define(OFPT_ECHO_REQUEST, 2).
-define(OFPT_ECHO_REPLY, 3).
-define(OFPT_VENDOR, 4).
-define(OFPT_FEATURES_REQUEST, 5).
-define(OFPT_FEATURES_REPLY, 6).
-define(OFPT_GET_CONFIG_REQUEST, 7).
-define(OFPT_GET_CONFIG_REPLY, 8).
-define(OFPT_SET_CONFIG, 9).
-define(OFPT_PACKET_IN, 10).
-define(OFPT_FLOW_REMOVED, 11).
-define(OFPT_PORT_STATUS, 12).
-define(OFPT_PACKET_OUT, 13).
-define(OFPT_FLOW_MOD, 14).
-define(OFPT_PORT_MOD, 15).
-define(OFPT_STATS_REQUEST, 16).
-define(OFPT_STATS_REPLY, 17).
-define(OFPT_BARRIER_REQUEST, 18).
-define(OFPT_BARRIER_REPLY, 19).
-define(OFPT_QUEUE_GET_CONFIG_REQUEST, 20).
-define(OFPT_QUEUE_GET_CONFIG_REPLY, 21).

-type ofp_message() :: ofp_hello()
                     | ofp_error_msg()
                     | ofp_echo_request()
                     | ofp_echo_reply()
                     | ofp_vendor_header()
                     | ofp_switch_features()
                     | ofp_switch_config()
                     | ofp_packet_in()
                     | ofp_flow_removed()
                     | ofp_port_status()
                     | ofp_packet_out()
                     | ofp_flow_mod()
                     | ofp_port_mod()
                     | ofp_stats_request()
                     | ofp_stats_reply()
                     | ofp_barrier_request()
                     | ofp_barrier_reply()
                     | ofp_queue_get_config_request()
                     | ofp_queue_get_config_reply().

-type ofp_type() :: hello
                  | error
                  | echo_request
                  | echo_reply
                  | vendor
                  | features_request
                  | features_reply
                  | get_config_request
                  | get_config_reply
                  | set_config
                  | packet_in
                  | flow_removed
                  | port_status
                  | packet_out
                  | flow_mod
                  | port_mod
                  | stats_request
                  | stats_reply
                  | barrier_request
                  | barrier_reply
                  | queue_get_config_request
                  | queue_get_config_reply.

-record(ofp_header, {
          version = ?VERSION :: integer(),
          type :: ofp_type(),
          xid = 0 :: integer(),
          body :: ofp_message() }).
-type ofp_header() :: #ofp_header{}.
