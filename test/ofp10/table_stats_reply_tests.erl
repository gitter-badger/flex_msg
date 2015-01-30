-module(table_stats_reply_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

table_stats_reply_test_() ->
    [{ "table_stats_reply with xid 123", fun table_stats_reply_with_xid/0 }].

table_stats_reply_with_xid() ->
    TableStats = #ofp_table_stats{ table_id = 0, name = <<"classifier">>,
                                   wildcards = 4194303, max_entries = 1000000 },
    Body = #ofp_stats_reply{ type = table, flags = [], body = [TableStats] },
    Msg = #ofp_header{ type = stats_reply,
                       xid = 123,
                       body = Body },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
