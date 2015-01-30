-module(nx_role_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

nx_role_reply_test_() ->
    [{ "nx role reply with role other and xid 123", fun nx_role_reply_with_other/0 },
     { "nx role reply with role master and xid 123", fun nx_role_reply_with_master/0 },
     { "nx role reply with role slave and xid 123", fun nx_role_reply_with_slave/0 }].

nx_role_reply_with_other() ->
    NXData = #nicira_header{ sub_type = role_reply,
                             body = #nx_role{ role = other }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_role_reply_with_master() ->
    NXData = #nicira_header{ sub_type = role_reply,
                             body = #nx_role{ role = master }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

nx_role_reply_with_slave() ->
    NXData = #nicira_header{ sub_type = role_reply,
                             body = #nx_role{ role = slave }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
