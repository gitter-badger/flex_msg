-module(get_config_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

get_config_request_test_() ->
    [{ "get_config_request with not xid", fun request/0 },
     { "get_config_request with xid 123", fun request_with_xid/0 }].

request() ->
    GetConfigRequest = #ofp_get_config_request{},
    Msg = #ofp_header{ type = get_config_request,
                       body = GetConfigRequest },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

request_with_xid() ->
    GetConfigRequest = #ofp_get_config_request{},
    Msg = #ofp_header{ type = get_config_request,
                       xid = 123,
                       body = GetConfigRequest },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
