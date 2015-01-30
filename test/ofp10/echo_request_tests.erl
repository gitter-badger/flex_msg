-module(echo_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

echo_request_test_() ->
    [{ "echo request with not xid", fun echo_request/0 },
     { "echo request with xid 123", fun echo_request_xid/0 },
     { "echo request with data hoge", fun echo_request_data/0 },
     { "echo requset with xid 123 and data hoge", fun echo_request_data_xid/0 }].

echo_request() ->
    Echo = #ofp_echo_request{},
    Msg = #ofp_header{ type = echo_request,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

echo_request_xid() ->
    Echo = #ofp_echo_request{},
    Msg = #ofp_header{ type = echo_request,
                       xid = 123,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

echo_request_data() ->
    Echo = #ofp_echo_request{ data = <<"hoge">> },
    Msg = #ofp_header{ type = echo_request,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

echo_request_data_xid() ->
    Echo = #ofp_echo_request{ data = <<"hoge">> },
    Msg = #ofp_header{ type = echo_request,
                       xid = 123,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
