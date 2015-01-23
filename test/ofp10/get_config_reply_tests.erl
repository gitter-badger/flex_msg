-module(get_config_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

get_config_reply_test_() ->
    [{ "get config reply with no options", fun reply/0 },
     { "get config reply with flags options", fun reply_with_flags/0 },
     { "get config reply with flags and miss send len options",
       fun reply_with_flags_miss_send_len/0 },
     { "get config reply with xid 123, flags and miss send len options",
       fun reply_with_flags_miss_send_len_xid/0 }].

reply() ->
    GetConfigReply = #ofp_switch_config{},
    Msg = #ofp_header{ type = get_config_reply,
                       body = GetConfigReply },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

reply_with_flags() ->
    GetConfigReply = #ofp_switch_config{ flags = [normal, drop,
                                                  reasm, mask] },
    Msg = #ofp_header{ type = get_config_reply,
                       body = GetConfigReply },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

reply_with_flags_miss_send_len() ->
    GetConfigReply = #ofp_switch_config{ flags = [normal, drop,
                                                  reasm, mask],
                                         miss_send_len = 256 },
    Msg = #ofp_header{ type = get_config_reply,
                       body = GetConfigReply },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

reply_with_flags_miss_send_len_xid() ->
    GetConfigReply = #ofp_switch_config{ flags = [normal, drop,
                                                  reasm, mask],
                                         miss_send_len = 256 },
    Msg = #ofp_header{ type = get_config_reply,
                       xid = 123,
                       body = GetConfigReply },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
