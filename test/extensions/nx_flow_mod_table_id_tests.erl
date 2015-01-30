-module(nx_flow_mod_table_id_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

nx_flow_mod_table_id_test_() ->
    [{ "enable nx_flow_mod_table_id with xid 123",
       fun enable_nx_flow_mod_table_id_xid/0 },
     { "disable nx_flow_mod_table_id with xid 123",
       fun disable_nx_flow_mod_table_id_xid/0 }].

enable_nx_flow_mod_table_id_xid() ->
    NXData = #nicira_header{ sub_type = flow_mod_table_id,
                             body = #nx_flow_mod_table_id{ set = true }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

disable_nx_flow_mod_table_id_xid() ->
    NXData = #nicira_header{ sub_type = flow_mod_table_id,
                             body = #nx_flow_mod_table_id{ set = false }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
