-module(nx_set_packet_in_format_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

nx_set_packet_in_format_test_() ->
    [{ "nx_set_packet_in_format with xid 123 and nxm",
       fun nx_set_packet_in_format_xid_nxm/0 },
     { "nx_set_packet_in_format with xid 123 and openflow10",
       fun nx_set_packet_in_format_xid_ofp10/0 }].

nx_set_packet_in_format_xid_nxm() ->
    NXData = #nicira_header{ sub_type = set_packet_in_format,
                             body = #nx_set_packet_in_format{ format = nxm }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_set_packet_in_format_xid_ofp10() ->
    NXData = #nicira_header{ sub_type = set_packet_in_format,
                             body = #nx_set_packet_in_format{ format = openflow10 }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
