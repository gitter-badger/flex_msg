-module(nx_set_flow_format_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

nx_set_flow_format_test_() ->
    [{ "nx set_flow_format with nxm and xid 123", fun set_flow_format_with_nxm/0 },
     { "nx set_flow_format with openflow10 and xid 123", fun set_flow_format_with_openflow10/0 }].

set_flow_format_with_nxm() ->
    NXData = #nicira_header{ sub_type = set_flow_format,
                             body = #nx_set_flow_format{ format = nxm }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

set_flow_format_with_openflow10() ->
    NXData = #nicira_header{ sub_type = set_flow_format,
                             body = #nx_set_flow_format{ format = openflow10 }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
