-module(vendor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

vendor_test_() ->
    [{ "vendor with vendor 0 and data blank", fun vendor/0 },
     { "vendor with xid 123, vendor 0 and data blank", fun vendor_xid/0 },
     { "vendor with xid 123, vendor 2320 and data nx flow mod table id", fun vendor_nx_table_id_xid/0 }].

vendor() ->
    Vendor = #ofp_vendor_header{ vendor = 0, data = <<>> },
    Msg = #ofp_header{ type = vendor, body = Vendor },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

vendor_xid() ->
    Vendor = #ofp_vendor_header{ vendor = 0, data = <<>> },
    Msg = #ofp_header{ type = vendor, xid = 123, body = Vendor },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

vendor_nx_table_id_xid() ->
    Vendor = #ofp_vendor_header{ vendor = 2320,
                                 data = <<16#01, 16#04, 16#00, 16#18,
                                          16#00, 16#00, 16#00, 16#07,
                                          16#00, 16#00, 16#23, 16#20,
                                          16#00, 16#00, 16#00, 16#0f,
                                          16#01, 16#00, 16#00, 16#00,
                                          16#00, 16#00, 16#00, 16#00>> },
    Msg = #ofp_header{ type = vendor, xid = 123, body = Vendor },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
