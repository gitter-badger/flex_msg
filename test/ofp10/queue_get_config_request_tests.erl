-module(queue_get_config_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

queue_get_config_request_test_() ->
    [{ "queue_get_config_request with xid 123", fun queue_get_config_request_with_xid/0 }].

queue_get_config_request_with_xid() ->
    Body = #ofp_queue_get_config_request{ port_no = 1 },
    Msg = #ofp_header{ type = queue_get_config_request,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
