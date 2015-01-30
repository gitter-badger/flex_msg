-module(echo_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

echo_reply_test_() ->
    [{ "echo reply with not xid", fun echo_reply/0 },
     { "echo reply with xid 123", fun echo_reply_xid/0 },
     { "echo reply with data hoge", fun echo_reply_data/0 },
     { "echo requset with xid 123 and data hoge", fun echo_reply_data_xid/0 }].

echo_reply() ->
    Echo = #ofp_echo_reply{},
    Msg = #ofp_header{ type = echo_reply,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

echo_reply_xid() ->
    Echo = #ofp_echo_reply{},
    Msg = #ofp_header{ type = echo_reply,
                       xid = 123,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

echo_reply_data() ->
    Echo = #ofp_echo_reply{ data = <<"hoge">> },
    Msg = #ofp_header{ type = echo_reply,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

echo_reply_data_xid() ->
    Echo = #ofp_echo_reply{ data = <<"hoge">> },
    Msg = #ofp_header{ type = echo_reply,
                       xid = 123,
                       body = Echo },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
