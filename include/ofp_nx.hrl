%% Nicira Extention

%%%-----------------------------------------------------------------------------
%%% Nicira Extention Commons
%%%-----------------------------------------------------------------------------

-define(NX_VENDOR_ID, 16#00002320).

-define(NXT_ROLE_REQUEST, 10).
-define(NXT_ROLE_REPLY, 11).
-define(NXT_SET_FLOW_FORMAT, 12).
-define(NXT_FLOW_MOD, 13).
-define(NXT_FLOW_REMOVED, 14).
-define(NXT_FLOW_MOD_TABLE_ID, 15).
-define(NXT_SET_PACKET_IN_FORMAT, 16).
-define(NXT_PACKET_IN, 17).
-define(NXT_FLOW_AGE, 18).
-define(NXT_SET_ASYNC_CONFIG, 19).
-define(NXT_SET_CONTROLLER_ID, 20).
-define(NXT_FLOW_MONITOR_CANCEL, 21).
-define(NXT_FLOW_MONITOR_PAUSED, 22).
-define(NXT_FLOW_MONITOR_RESUMED, 23).

%%%-----------------------------------------------------------------------------
%%%  Message
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Common Structures
%%%-----------------------------------------------------------------------------

-define(NICIRA_HEADER_SIZE, 16).

-record(nicira_header, {
          sub_type,
          body }).
-type nicira_header() :: #nicira_header{}.

-define(MATCH_SIZE, 8).

-record(nx_match, { fields = [] }).
-type nx_match() :: #nx_match{}.

%% -------------------------------
%% OpenFlow 1.0-compatible fields.
%% -------------------------------

-define(NXM_OF_IN_PORT, 0).
-define(NXM_OF_ETH_DST, 1).
-define(NXM_OF_ETH_SRC, 2).
-define(NXM_OF_ETH_TYPE, 3).
-define(NXM_OF_VLAN_TCI, 4).
-define(NXM_OF_IP_TOS, 5).
-define(NXM_OF_IP_PROTO, 6).
-define(NXM_OF_IP_SRC, 7).
-define(NXM_OF_IP_DST, 8).
-define(NXM_OF_TCP_SRC, 9).
-define(NXM_OF_TCP_DST, 10).
-define(NXM_OF_UDP_SRC, 11).
-define(NXM_OF_UDP_DST, 12).
-define(NXM_OF_ICMP_TYPE, 13).
-define(NXM_OF_ICMP_CODE, 14).
-define(NXM_OF_ARP_OP, 15).
-define(NXM_OF_ARP_SPA, 16).
-define(NXM_OF_ARP_TPA, 17).

%% ------------------------
%% Nicira match extensions
%% ------------------------

-define(NXM_NX_MAX_REGS, 16).
-define(NXM_NX_REG0, 0).
-define(NXM_NX_REG1, 1).
-define(NXM_NX_REG2, 2).
-define(NXM_NX_REG3, 3).
-define(NXM_NX_REG4, 4).
-define(NXM_NX_REG5, 5).
-define(NXM_NX_REG6, 6).
-define(NXM_NX_REG7, 7).

-define(NXM_NX_TUN_ID, 16).
-define(NXM_NX_ARP_SHA, 17).
-define(NXM_NX_ARP_THA, 18).
-define(NXM_NX_IPV6_SRC, 19).
-define(NXM_NX_IPV6_DST, 20).
-define(NXM_NX_ICMPV6_TYPE, 21).
-define(NXM_NX_ICMPV6_CODE, 22).
-define(NXM_NX_ND_TARGET, 23).
-define(NXM_NX_ND_SLL, 24).
-define(NXM_NX_ND_TLL, 25).
-define(NXM_NX_IP_FRAG, 26).
-define(NXM_NX_IPV6_LABEL, 27).
-define(NXM_NX_IP_ECN, 28).
-define(NXM_NX_IP_TTL, 29).
-define(NXM_NX_COOKIE, 30).
-define(NXM_NX_TUN_IPV4_SRC, 31).
-define(NXM_NX_TUN_IPV4_DST, 32).
-define(NXM_NX_PKT_MARK, 33).
-define(NXM_NX_TCP_FLAGS, 34).
-define(NXM_NX_DP_HASH, 35).
-define(NXM_NX_RECIRC_ID, 36).

-record(nxm_field_header, {
          vendor :: atom(),
          field :: atom(),
          has_mask = false :: boolean() }).
-type nxm_field_header() :: #nxm_field_header{}.

-record(oxm_field, {
          vendor :: atom(),
          field :: atom(),
          has_mask = false :: boolean(),
          value,
          mask }).
-type oxm_field() :: #oxm_field{}.

-define(NXAST_RESUBMIT, 1).
-define(NXAST_RESUBMIT_TABLE, 14).
-record(nx_action_resubmit, {
          subtype :: resubmit | resubmit_table,
          in_port = in_port,
          table_id = 16#ff }).
-type nx_action_resubmit() :: #nx_action_resubmit{}.

-define(NX_FLOW_MOD_SPEC_HEADER_SIZE, 2).
-record(nx_flow_mod_spec_header, { action, n_bits }).
-type nx_flow_mod_spec_header() :: #nx_flow_mod_spec_header{}.

-record(learn_match_field, { src, dst }).
-type learn_match_field() :: #learn_match_field{}.

-record(learn_immediate_field, { value, dst }).
-type learn_immediate_field() :: #learn_immediate_field{}.

-record(learn_load_field, { src, dst }).
-type learn_load_field() :: #learn_load_field{}.

-record(learn_load_immediate_field, { value, dst }).
-type learn_load_immediate_field() :: #learn_load_immediate_field{}.

-record(learn_output_action, { port }).
-type learn_output_action() :: #learn_output_action{}.

-define(NXAST_LEARN, 16).
-record(nx_action_learn, {
          idle_timeout = 0,
          hard_timeout = 0,
          priority = 65535,
          cookie = <<0:64>>,
          flags = [],
          table_id = 255,
          fin_idle_timeout = 0,
          fin_hard_timeout = 0,
          flow_mod_spec = [] }).
-type nx_action_learn() :: #nx_action_learn{}.

-define(NXAST_NOTE, 8).
-record(nx_action_note, {
          note = <<>> :: binary() }).
-type nx_action_note() :: #nx_action_note{}.

-define(NXAST_SET_TUNNEL, 2).
-record(nx_action_set_tunnel, { tun_id = 0 :: integer() }).
-type nx_action_set_tunnel() :: #nx_action_set_tunnel{}.

-define(NXAST_SET_TUNNEL64, 9).
-record(nx_action_set_tunnel64, { tun_id = 0 :: integer() }).
-type nx_action_set_tunnel64() :: #nx_action_set_tunnel64{}.

%%%-----------------------------------------------------------------------------
%%% Read State Message
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Modify Flow Entry Message
%%%-----------------------------------------------------------------------------

-record(nx_flow_mod_table_id, {
          set :: boolean() }).
-type nx_flow_mod_table_id() :: #nx_flow_mod_table_id{}.

%%%-----------------------------------------------------------------------------
%%% Packet-In Message
%%%-----------------------------------------------------------------------------

-define(NXPIF_OPENFLOW10, 0).
-define(NXPIF_NXM, 1).

-type nx_packet_in_format() :: openflow10
                             | nxm.

-record(nx_set_packet_in_format, {
          format :: atom() }).
-type nx_set_packet_in_format() :: #nx_set_packet_in_format{}.

%%%-----------------------------------------------------------------------------
%%% Role Message
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Switch Configuration Message
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Flow Modification Message
%%%-----------------------------------------------------------------------------

-define(NXFF_OPENFLOW10, 0).
-define(NXFF_NXM, 1).

-define(NXM0, 0). % compatible with openflow 1.0
-define(NXM1, 1). % nicira ext match.

-record(nx_set_flow_format, { format :: atom() }).
-type nx_set_flow_format() :: #nx_set_flow_format{}.

-record(nx_flow_mod, {
          cookie = <<0:64>> :: binary(),
          command :: atom(),
          table_id = 0 :: integer(),
          idle_timeout = 0 :: integer(),
          hard_timeout = 0 :: integer(),
          priority = 65535 :: integer(),
          buffer_id = no_buffer :: atom(),
          out_port = none :: atom(),
          flags = [] :: list(),
          match = [] :: list(),
          actions = [] :: list() }).
-type nx_flow_mod() :: #nx_flow_mod{}.
