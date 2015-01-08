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
