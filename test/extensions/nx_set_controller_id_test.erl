-module(nx_set_controller_id_test).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

nx_set_controller_id_test_() ->
    [{ "nx set controller id with xid 123", fun set_controller_id_with_xid/0 }].

set_controller_id_with_xid() ->
    NXData = #nicira_header{ sub_type = set_controller_id,
                             body = #nx_controller_id{ id = 1 }},
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
