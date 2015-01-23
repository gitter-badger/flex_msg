-module(features_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

features_request_test_() ->
    [{ "features_request with xid 123", fun features_request_xid/0 },
     { "features_request with not xid", fun features_request/0 }].

features_request() ->
    Msg = #ofp_header{ version = 1,
                       type = features_request,
                       body = #ofp_features_request{} },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

features_request_xid() ->
    Msg = #ofp_header{ version = 1,
                       type = features_request,
                       xid = 123,
                       body = #ofp_features_request{} },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
