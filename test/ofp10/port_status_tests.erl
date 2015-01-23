-module(port_status_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

port_status_test_() ->
    [{ "port_mod with port_no 1 reason add", fun port_phy1_add/0 },
     { "port_mod with port_no 1 reason delete", fun port_phy1_delete/0 },
     { "port_mod with port_no 1 reason modify", fun port_phy1_modify/0 },
     { "port_mod with port_no max", fun port_max/0 },
     { "port_mod with port_no in_port", fun port_in_port/0 },
     { "port_mod with port_no table", fun port_table/0 },
     { "port_mod with port_no normal", fun port_normal/0 },
     { "port_mod with port_no flood", fun port_flood/0 },
     { "port_mod with port_no all", fun port_all/0 },
     { "port_mod with port_no controller", fun port_controller/0 },
     { "port_mod with port_no local", fun port_local/0 },
     { "port_mod with port_no none", fun port_none/0 }].

port_phy1_add() ->
    Port = #ofp_phy_port{ port_no = 1,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = add,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_phy1_delete() ->
    Port = #ofp_phy_port{ port_no = 1,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_phy1_modify() ->
    Port = #ofp_phy_port{ port_no = 1,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = modify,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_max() ->
    Port = #ofp_phy_port{ port_no = max,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_in_port() ->
    Port = #ofp_phy_port{ port_no = in_port,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_table() ->
    Port = #ofp_phy_port{ port_no = table,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_normal() ->
    Port = #ofp_phy_port{ port_no = normal,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_flood() ->
    Port = #ofp_phy_port{ port_no = flood,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_all() ->
    Port = #ofp_phy_port{ port_no = all,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_controller() ->
    Port = #ofp_phy_port{ port_no = controller,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_local() ->
    Port = #ofp_phy_port{ port_no = local,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_none() ->
    Port = #ofp_phy_port{ port_no = none,
                          hw_addr = <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06>>,
                          name = <<"foo">>,
                          config = [no_flood], state = [stp_forward],
                          curr = ['10mb_hd'], advertised = ['1gb_fd'],
                          supported = [autoneg], peer = [pause_asym]},
    Body = #ofp_port_status{ reason = delete,
                             desc = Port },
    Msg = #ofp_header{ type = port_status,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
