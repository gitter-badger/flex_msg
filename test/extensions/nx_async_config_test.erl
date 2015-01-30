-module(nx_async_config_test).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").
-include("ofp_nx.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

nx_async_config_test_() ->
    [{ "async config with xid 123", fun nx_async_config_with_xid/0 }].

nx_async_config_with_xid() ->
    NXData = #nicira_header{ sub_type = set_async_config,
                             body = #nx_async_config{ packet_in_mask = [no_match,
                                                                        action],
                                                      port_status_mask = [add,
                                                                          delete,
                                                                          modify],
                                                      flow_removed_mask = [idle_timeout,
                                                                           hard_timeout,
                                                                           delete] } },
    Body = #ofp_vendor_header{ vendor = nicira,
                               data = NXData },
    Msg = #ofp_header{ type = vendor,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    io:format("msg: ~p~n, ~p~n", [?DECODE(Bin), erlang:get_stacktrace()]),
    {ok, EMsg, _ } = ?DECODE(Bin),
    io:format("msg: ~p~n, ~p~n", [EMsg, erlang:get_stacktrace()]),
    Msg = EMsg.
