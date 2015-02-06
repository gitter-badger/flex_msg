%% Protocol version
-define(VERSION, 6).

%%%-----------------------------------------------------------------------------
%%% Protocol Basic Format(7.1)
%%%-----------------------------------------------------------------------------

-define(OFP_HELLO_SIZE, 8).

-type ofp_hello_element() :: { versionbitmap, [integer()] }.

-record(ofp_hello, { version = ?VERSION :: integer(),
                     xid = 0 :: integer(),
                     elements = [] :: [ofp_hello_element()] }).
-type ofp_hello() :: #ofp_hello{}.

%%%-----------------------------------------------------------------------------
%%% OpenFlow Header (7.1.1)
%%%-----------------------------------------------------------------------------

% immutable messages.
-define(OFPT_HELLO, 0).
-define(OFPT_ERROR, 1).
-define(OFPT_ECHO_REQUEST, 2).
-define(OFPT_ECHO_REPLY, 3).
-define(OFPT_EXPERIMENTER, 4).

% switch configuration messages.
-define(OFPT_FEATURES_REQUEST, 5).
-define(OFPT_FEATURES_REPLY, 6).
-define(OFPT_GET_CONFIG_REQUEST, 7).
-define(OFPT_GET_CONFIG_REPLY, 8).
-define(OFPT_SET_CONFIG, 9).

% asynchronous messages.
-define(OFPT_PACKET_IN, 10).
-define(OFPT_FLOW_REMOVED, 11).
-define(OFPT_PORT_STATUS, 12).

% controller command messages.
-define(OFPT_PACKET_OUT, 13).
-define(OFPT_FLOW_MOD, 14).
-define(OFPT_GROUP_MOD, 15).
-define(OFPT_PORT_MOD, 16).
-define(OFPT_TABLE_MOD, 17).

% multipart messages.
-define(OFPT_MULTIPART_REQUEST, 18).
-define(OFPT_MULTIPART_REPLY, 19).

% barrier messages.
-define(OFPT_BARRIER_REQUEST, 20).
-define(OFPT_BARRIER_REPLY, 21).

% controller role change request messages
-define(OFPT_ROLE_REQUEST, 22).
-define(OFPT_ROLE_REPLY, 23).

% asynchronous message configuration
-define(OFPT_GET_ASYNC_REQUEST, 26).
-define(OFPT_GET_ASYNC_REPLY, 27).
-define(OFPT_SET_ASYNC_REPLY, 28).

% meters and rate limiters configuration messages
-define(OFPT_METER_MOD, 29).

% controller role change event messages
-define(OFPT_ROLE_STATUS, 30).

% asynchronous messages
-define(OFPT_TABLE_STATUS, 31).

% request forwarding by the switch
-define(OFPT_REQUESTFORWORD, 32).

% bundle operations (multiple messages as a single operation)
-define(OFPT_BUNDLE_CONTROL, 33).
-define(OFPT_BUNDLE_ADD_MESSAGE, 34).

% controller status async message
-define(OFPT_CONTROLLER_STATUS, 35).

%%%-----------------------------------------------------------------------------
%%% Common Structures(7.2)
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Port Structures(7.2.1)
%%%-----------------------------------------------------------------------------

-define(OFPP_MAX, 16#ffffff00).
-define(OFPP_UNSET, 16#fffffff7).
-define(OFPP_IN_PORT, 16#fffffff8).
-define(OFPP_TABLE, 16#fffffff9).
-define(OFPP_NORMAL, 16#fffffffa).
-define(OFPP_FLOOD, 16#fffffffb).
-define(OFPP_ALL, 16#fffffffc).
-define(OFPP_CONTROLLER, 16#fffffffd).
-define(OFPP_LOCAL, 16#fffffffe).
-define(OFPP_ANY, 16#ffffffff).

-type ofp_port_no() :: max
                     | unset
                     | in_port
                     | table
                     | normal
                     | flood
                     | all
                     | controller
                     | local
                     | any
                     | integer().

%%%-----------------------------------------------------------------------------
%%% Port Description Structures(7.2.1.1)
%%%-----------------------------------------------------------------------------

-define(OFP_ETH_ALEN, 6).
-define(OFP_MAX_PORT_NAME_LEN, 16). % null-terminated

-type ofp_port_config() :: port_down
                         | no_recv
                         | no_fwd
                         | no_packet_in.

-type ofp_port_state() :: link_down
                        | blocked
                        | live.

-record(ofp_port, { port_no :: ofp_port_no(),
                    hw_addr :: binary(),
                    name    :: binary(),
                    config  :: [ofp_port_config()],
                    state   :: [ofp_port_state()] }).

-type ofp_port() :: #ofp_port{}.

%%%-----------------------------------------------------------------------------
%%% Port Description Properties(7.2.1.2)
%%%-----------------------------------------------------------------------------

-define(OFPPDPT_ETHERNET, 0).
-define(OFPPDPT_OPTICAL, 1).
-define(OFPPDPT_PIPELINE_INPUT, 2).
-define(OFPPDPT_PIPELINE_OUTPUT, 3).
-define(OFPPDPT_RECIRCULATE, 4).
-define(OFPPDPT_EXPERIMENTER, 16#ffff).

-type ofp_port_desc_prop_type() :: ethernet
                                 | optical
                                 | pipeline_input
                                 | pipeline_output
                                 | recircurate
                                 | experimeter.

-record(ofp_port_desc_prop_header, { type :: ofp_port_desc_prop_type() }).
-type ofp_port_desc_prop_header() :: #ofp_port_desc_prop_header{}.

-type ofp_port_features() :: '10mb_hd'
                           | '10mb_fd'
                           | '100mb_hd'
                           | '100mb_fd'
                           | '1gb_hd'
                           | '1gb_fd'
                           | '10gb_fd'
                           | '40gb_fd'
                           | '100gb_fd'
                           | '1tb_fd'
                           | other
                           | copper
                           | fibre
                           | autoneg
                           | pause
                           | pause_asym.

-record(ofp_port_desc_prop_ethernet, { curr       :: [ofp_port_features()],
                                       advertised :: [ofp_port_features()],
                                       supported  :: [ofp_port_features()] }).
-type ofp_port_desc_prop_ethernet() :: #ofp_port_desc_prop_ethernet{}.

-type ofp_optical_port_features() :: rx_tune
                                   | tx_tune
                                   | tx_pwr
                                   | use_freq.

-record(ofp_port_desc_prop_optical, { supported :: [ofp_optical_port_features()],
                                      tx_min_freq_lmda  :: integer(),
                                      tx_max_freq_lmda  :: integer(),
                                      tx_grid_freq_lmda :: integer(),
                                      rx_min_freq_lmda  :: integer(),
                                      rx_max_freq_lmda  :: integer(),
                                      rx_grid_freq_lmda :: integer(),
                                      tx_pwr_min        :: integer(),
                                      tx_pwr_max        :: integer() }).
-type ofp_port_desc_prop_optical() :: #ofp_port_desc_prop_optical{}.

%%%-----------------------------------------------------------------------------
%%% Header Type Structure(7.2.2)
%%%-----------------------------------------------------------------------------

-type ofp_header_type_namespaces() :: onf
                                    | ethertype
                                    | ip_proto
                                    | udp_tcp_port
                                    | ipv4_option.

-type ofp_header_type_onf() :: ethernet
                             | no_header
                             | oxm_experimenter.

-type ofp_header_type_ethertype() :: ipv4_header
                                   | vlan_tag
                                   | ipv6_header
                                   | mpls_shim_header
                                   | pbb_itag.

-type ofp_header_type_ipv4() :: basic_tcp_header
                              | udp_header.

-type ofp_header_type_udp_tcp_port() :: vxlan_header.

-record(ofp_header_type, { name_space :: ofp_header_type_namespaces(),
                           ns_type :: ofp_header_type_onf()
                                    | ofp_header_type_ethertype()
                                    | ofp_header_type_ipv4()
                                    | ofp_header_type_udp_tcp_port() }).
-type ofp_header_type() :: #ofp_header_type{}.

%%%-----------------------------------------------------------------------------
%%% Flow matching structures(7.2.3)
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Flow match headers(7.2.3.1)
%%%-----------------------------------------------------------------------------

-type ofp_match_type() :: standard
                        | oxm.

-record(ofp_match, { type  :: ofp_match_type(),
                     oxm_fields = [] }).

%%%-----------------------------------------------------------------------------
%%% Flow match field structures(7.2.3.2)
%%%-----------------------------------------------------------------------------

-type oxm_class() :: nxm0
                   | nxm1
                   | openflow_basic
                   | packet_regs
                   | experimenter.

-type oxm_field_type() :: in_port
                        | in_phy_port
                        | metadata
                        | eth_dst
                        | eth_src
                        | eth_type
                        | vlan_vid
                        | vlan_pcp
                        | ip_dscp
                        | ip_ecn
                        | ip_proto
                        | ipv4_src
                        | ipv4_dst
                        | tcp_src
                        | tcp_dst
                        | udp_src
                        | udp_dst
                        | sctp_src
                        | sctp_dst
                        | icmpv4_type
                        | icmpv4_code
                        | arp_op
                        | arp_spa
                        | arp_tpa
                        | arp_sha
                        | arp_tha
                        | ipv6_src
                        | ipv6_dst
                        | ipv6_flabel
                        | icmpv6_type
                        | icmpv6_code
                        | ipv6_nd_target
                        | ipv6_nd_sll
                        | ipv6_nd_tll
                        | mpls_label
                        | mpls_tc
                        | mpls_bos
                        | pbb_isid
                        | tunnel_id
                        | ipv6_exthdr
                        | pbb_uca
                        | tcp_flags
                        | actset_output
                        | packet_type.

-record(oxm_field, { class            :: oxm_class(),
                     field            :: oxm_field_type(),
                     has_mask = false :: boolean(),
                     value,
                     mask }).
-type oxm_field() :: #oxm_field{}.

-type ofp_vlan_id() :: present
                     | none.

-type ofp_ipv6exthdr_flags() :: nonext
                              | esp
                              | auth
                              | dest
                              | frag
                              | router
                              | hop
                              | unrep
                              | unreq.

%%%-----------------------------------------------------------------------------
%%% Experimenter Flow Match Fields(7.2.3.12)
%%%-----------------------------------------------------------------------------

-record(ofp_oxm_experimenter_header, { class = experimenter :: experimenter,
                                       experimenter = <<>> :: binary() }).
-type ofp_oxm_experimenter_header() :: #ofp_oxm_experimenter_header{}.

%%%-----------------------------------------------------------------------------
%%% Flow Stats Structures(7.2.4)
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------
%%% Flow Stats Header(7.2.4.1)
%%%-----------------------------------------------------------------------------

-record(ofp_stats, { oxs_fields = [] }).
-type ofp_stats() :: #ofp_stats{}.

%%%-----------------------------------------------------------------------------
%%% Flow Stat Field Structures(7.2.4.2)
%%%-----------------------------------------------------------------------------

-type ofp_oxs_class() :: openflow_basic
                       | experimenter.

-record(oxs_field, { class :: ofp_oxs_class(),
                     field }).
-type oxs_field() :: #oxs_field{}.

%%%-----------------------------------------------------------------------------
%%% Flow Stat Fields(7.2.4.3)
%%%-----------------------------------------------------------------------------

-type oxs_ofb_stat_fields() :: duration
                             | idle_time
                             | flow_count
                             | packet_count
                             | byte_count.

%%%-----------------------------------------------------------------------------
%%% Experimenter Flow Stat Fields(7.2.4.4)
%%%-----------------------------------------------------------------------------

-record(ofp_oxs_experimenter_header, { class = experimenter,
                                       experimenter :: integer() }).
-type ofp_oxs_experimenter_header() :: #ofp_oxs_experimenter_header{}.

%%%-----------------------------------------------------------------------------
%%% Flow Instruction Structures(7.2.5)
%%%-----------------------------------------------------------------------------

-type ofp_instruction_type() :: goto_table
                              | write_metadata
                              | write_actions
                              | apply_actions
                              | clear_actions
                              | deprecated
                              | stat_trigger
                              | experimenter.

-record(ofp_instruction_goto_table, { table_id }).
-type ofp_instruction_goto_table() :: #ofp_instruction_goto_table{}.

-record(ofp_instruction_write_metadata, { metadata :: binary(),
                                          metadata_mask :: binary() }).
-type ofp_instruction_write_metadata() :: #ofp_instruction_write_metadata{}.

-record(ofp_instruction_actions, { actions = [] }).
-type ofp_instruction_actions() :: #ofp_instruction_actions{}.

-type ofp_stat_trigger_flags() :: periodic
                                | only_first.

-record(ofp_instruction_stat_trigger, { flags = []      :: [ofp_stat_trigger_flags()],
                                        thresholds = [] :: [ofp_stats()] }).
-type ofp_instruction_stat_trigger() :: #ofp_instruction_stat_trigger{}.


%%%-----------------------------------------------------------------------------
%%% Action Structures(7.2.6)
%%%-----------------------------------------------------------------------------

-type ofp_action_type() :: output
                         | copy_ttl_out
                         | copy_ttl_in
                         | set_mpls_ttl
                         | dec_mpls_ttl
                         | push_vlan
                         | pop_vlan
                         | push_mpls
                         | pop_mpls
                         | set_queue
                         | group
                         | set_nw_ttl
                         | dec_nw_ttl
                         | set_field
                         | push_pbb
                         | pop_pbb
                         | copy_field
                         | meter
                         | experimenter.

%%%-----------------------------------------------------------------------------
%%% Output Action Structures(7.2.6.1)
%%%-----------------------------------------------------------------------------

-type ofp_controller_max_len() :: max
                                | no_buffer
                                | integer().

-record(ofp_action_output, { port          :: ofp_port_no(),
                             max_len = 128 :: ofp_controller_max_len() }).
-type ofp_action_output() :: #ofp_action_output{}.

%%%-----------------------------------------------------------------------------
%%% Group Action Structures(7.2.6.2)
%%%-----------------------------------------------------------------------------

-record(ofp_action_group, { group_id :: integer() }).
-type ofp_action_group() :: #ofp_action_group{}.

%%%-----------------------------------------------------------------------------
%%% Set-Queue Action Structure(7.2.6.3)
%%%-----------------------------------------------------------------------------

-record(ofp_action_set_queue, { queue_id :: integer()}).
-type ofp_action_set_queue() :: #ofp_action_set_queue{}.

%%%-----------------------------------------------------------------------------
%%% Meter Action Structure(7.2.6.4)
%%%-----------------------------------------------------------------------------

-record(ofp_action_meter, { meter_id :: integer() }).
-type ofp_action_meter() :: #ofp_action_meter{}.

%%%-----------------------------------------------------------------------------
%%% TTL Action Structure(7.2.6.5)
%%%-----------------------------------------------------------------------------

-record(ofp_action_mpls_ttl, { mpls_ttl :: integer() }).
-type ofp_action_mpls_ttl() :: #ofp_action_mpls_ttl{}.

-record(ofp_action_generic, { type :: copy_ttl_out
                                    | copy_ttl_in
                                    | dec_mpls_ttl
                                    | dec_nw_ttl
                                    | pop_vlan
                                    | pop_pbb }).

-record(ofp_action_nw_ttl, { nw_ttl :: integer() }).
-type ofp_action_nw_ttl() :: #ofp_action_nw_ttl{}.

-record(ofp_action_push, { ethertype :: integer() }).
