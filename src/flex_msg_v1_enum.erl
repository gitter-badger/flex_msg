-module(flex_msg_v1_enum).

%%------------------------------------------------------------------------------
%% Common Structures
%%------------------------------------------------------------------------------

%% Port Structures -------------------------------------------------------------

-enum({ port_config, [port_down,
                      no_stp,
                      no_recv,
                      no_recv_stp,
                      no_flood,
                      no_fwd,
                      no_packet_in] }).

-enum({ port_state, [stp_listen,
                     link_down,
                     { stp_learn,   8 },
                     { stp_forward, 9 },
                     { stp_block, 768 }] }).

-enum({ port_no, [{ max,        16#ff00 },
                  { in_port,    16#fff8 },
                  { table,      16#fff9 },
                  { normal,     16#fffa },
                  { flood,      16#fffb },
                  { all,        16#fffc },
                  { controller, 16#fffd },
                  { local,      16#fffe },
                  { none,       16#ffff }] }).

-enum({ port_features, ['10mb_hd',
                        '10mb_fd',
                        '100mb_hd',
                        '100mb_fd',
                        '1gb_hd',
                        '1gb_fd',
                        '10gb_fd',
                        copper,
                        fibre,
                        autoneg,
                        pause,
                        pause_asym] }).

%% Error Message ---------------------------------------------------------------

-enum({ error_type, [hello_failed,
                     bad_request,
                     bad_action,
                     flow_mod_failed,
                     port_mod_failed,
                     queue_op_failed] }).

-enum({ hello_failed,[incompatible,
                      eperm] }).

-enum({ bad_request, [bad_version,
                      bad_type,
                      bad_stat,
                      bad_vendor,
                      bad_subtype,
                      eperm,
                      bad_len,
                      buffer_empty,
                      buffer_unknown] }).

-enum({ bad_action, [bad_type,
                     bad_len,
                     bad_vendor,
                     bad_vendor_type,
                     bad_out_port,
                     bad_argument,
                     eperm,
                     too_many,
                     bad_queue] }).

-enum({ flow_mod_failed, [all_tables_full,
                          overlap,
                          eperm,
                          bad_emerg_timeout,
                          bad_command,
                          unsupported] }).

-enum({ port_mod_failed, [bad_port,
                          bad_hw_addr] }).

-enum({ queue_op_failed,[bad_port,
                         bad_queue,
                         eperm] }).

%%------------------------------------------------------------------------------
%% Controller-to-Switch Messages
%%------------------------------------------------------------------------------
%% Handshake -------------------------------------------------------------------

-enum({ capabilities, [flow_stats,
                       table_stats,
                       port_stats,
                       stp,
                       reserved,
                       ip_reasm,
                       queue_stats,
                       arp_match_ip] }).

-enum({ config_flags, [normal,
                       drop,
                       reasm,
                       mask] }).

%%%-----------------------------------------------------------------------------
%%% Flow Action Structures (5 2.4)
%%%-----------------------------------------------------------------------------

-enum({ actions, [output,
                  set_vlan_vid,
                  set_vlan_pcp,
                  strip_vlan,
                  set_dl_src,
                  set_dl_dst,
                  set_nw_src,
                  set_nw_dst,
                  set_nw_tos,
                  set_tp_src,
                  set_tp_dst,
                  enqueue,
                  { vendor, 16#ffff }] }).

-enum({ max_len, [{ no_buffer, 16#ffffffff }] }).

%%------------------------------------------------------------------------------
%% Asynchronous Messages
%%------------------------------------------------------------------------------
%% Packet-In Message -----------------------------------------------------------

-enum({ packet_in_reason, [no_match,
                           action ] }).

%% Flow Removed Message --------------------------------------------------------

-enum({ flow_removed_reason, [idle_timeout,
                              hard_timeout,
                              delete] }).

%% Port Status Message ---------------------------------------------------------

-enum({ port_reason, [add,
                      delete,
                      modify] }).
%% Modify State Messages -------------------------------------------------------

-enum({flow_mod_command, [add,
                          modify,
                          modify_strict,
                          delete,
                          delete_strict]}).

-enum({flow_mod_flags, [send_flow_rem,
                        check_overlap,
                        emerg]}).

%% Stats Messages -------------------------------------------------------------

-enum({ stats_types, [desc,
                      flow,
                      aggregate,
                      table,
                      port,
                      queue,
                      { vendor, 16#ffff } ] }).

-enum({ stats_request_flags, [more] }).
-enum({ stats_reply_flags, [more] }).

-enum({ table_id, [{ emergency, 16#fe },
                   { all, 16#ff }] }).

-enum({ queue_id, [{ all, 16#ffffffff }] }).
