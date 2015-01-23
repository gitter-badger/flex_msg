-module(set_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

set_config_test_() ->
    [{ "set config with no options", fun set_config/0 },
     { "set config with flags options", fun set_config_with_flags/0 },
     { "set config with flags and miss send len options",
       fun set_config_with_flags_miss_send_len/0 },
     { "set config with xid 123, flags and miss send len options",
       fun set_config_with_flags_miss_send_len_xid/0 }].

set_config() ->
    SetConfig = #ofp_switch_config{},
    Msg = #ofp_header{ type = set_config,
                       body = SetConfig },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

set_config_with_flags() ->
    SetConfig = #ofp_switch_config{ flags = [normal, drop,
                                             reasm, mask] },
    Msg = #ofp_header{ type = set_config,
                       body = SetConfig },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

set_config_with_flags_miss_send_len() ->
    SetConfig = #ofp_switch_config{ flags = [normal, drop,
                                             reasm, mask],
                                    miss_send_len = 256 },
    Msg = #ofp_header{ type = set_config,
                       body = SetConfig },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

set_config_with_flags_miss_send_len_xid() ->
    SetConfig = #ofp_switch_config{ flags = [normal, drop,
                                             reasm, mask],
                                    miss_send_len = 256 },
    Msg = #ofp_header{ type = set_config,
                       xid = 123,
                       body = SetConfig },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
