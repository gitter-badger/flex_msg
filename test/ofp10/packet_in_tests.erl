-module(packet_in_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

packet_in_test_() ->
    [{ "packet_in with reason no match and xid 123", fun no_match_xid/0 },
     { "packet_in with reason no match", fun no_match/0 },
     { "packet_in with reason action", fun action/0 }].

no_match_xid() ->
    Frame = <<255, 255, 255, 255, 255, 255, 172, 93, 16,
              49, 55, 121, 8, 6, 0, 1, 8, 0, 6, 4, 0, 1,
              172, 93, 16, 49, 55, 121, 192, 168, 2, 254,
              255, 255, 255, 255, 255, 255, 192, 168, 2,
              5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    PacketIn = #ofp_packet_in{ buffer_id = 16#ffffff00,
                               in_port = 1,
                               total_len = 60,
                               reason = no_match,
                               data = Frame },
    Msg = #ofp_header{ type = packet_in,
                       xid = 123,
                       body = PacketIn },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

no_match() ->
    Frame = <<255, 255, 255, 255, 255, 255, 172, 93, 16,
              49, 55, 121, 8, 6, 0, 1, 8, 0, 6, 4, 0, 1,
              172, 93, 16, 49, 55, 121, 192, 168, 2, 254,
              255, 255, 255, 255, 255, 255, 192, 168, 2,
              5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    PacketIn = #ofp_packet_in{ buffer_id = 16#ffffff00,
                               in_port = 1,
                               total_len = 60,
                               reason = no_match,
                               data = Frame },
    Msg = #ofp_header{ type = packet_in,
                       body = PacketIn },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

action() ->
    Frame = <<255, 255, 255, 255, 255, 255, 172, 93, 16,
              49, 55, 121, 8, 6, 0, 1, 8, 0, 6, 4, 0, 1,
              172, 93, 16, 49, 55, 121, 192, 168, 2, 254,
              255, 255, 255, 255, 255, 255, 192, 168, 2,
              5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    PacketIn = #ofp_packet_in{ buffer_id = 16#ffffff00,
                               in_port = 1,
                               total_len = 60,
                               reason = action,
                               data = Frame },
    Msg = #ofp_header{ type = packet_in,
                       body = PacketIn },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
