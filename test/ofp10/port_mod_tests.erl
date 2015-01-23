-module(port_mod_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

port_mod_test_() ->
    [{"port_mod with config all flags", fun port_mod/0 }].

port_mod() ->
    Body = #ofp_port_mod{ port_no = 1,
                          hw_addr = <<17,34,51,68,85,102>>,
                          config = [port_down,
                                    no_stp,
                                    no_recv,
                                    no_recv_stp,
                                    no_flood,
                                    no_fwd,
                                    no_packet_in],
                          mask = [port_down],
                          advertised = [] },
    Msg = #ofp_header{ type = port_mod,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
