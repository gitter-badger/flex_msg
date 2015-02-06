-module(flex_msg_v6_encode).

-export([do/1]).

-include("ofp_v6.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

do(#ofp_hello{ xid = Xid, elements = Elem }) ->
    BodyBin = hello_elements(Elem, []),
    Length = ?OFP_HELLO_SIZE + byte_size(BodyBin),
    <<?VERSION:8, ?OFPT_HELLO:8, Length:16, Xid:32, BodyBin/bytes>>.

hello_elements([], Acc) -> list_to_binary(Acc);
hello_elements([H | Rest], Acc) -> hello_elements(Rest, [hello_element(H)|Acc]).

hello_element({ versionbitmap, Versions }) ->
    BitmapBin = bitmap(Versions),
    TypeInt = hello_elem(versionbitmap),
    SizeInt = 4 + byte_size(BitmapBin),
    <<TypeInt:16, SizeInt:16, BitmapBin/bytes>>;
hello_element(_) ->
    <<>>.

bitmap(List) ->
    Size = lists:max(List) div 32,
    bitmap(List, Size, 0).

bitmap([], Size, Acc) ->
    Bytes = (Size + 1) * 32,
    <<Acc:Bytes>>;
bitmap([H | Rest], Size, Acc) ->
    Index = (Size - H div 32) * 32 + H rem 32,
    bitmap(Rest, Size, Acc bor (1 bsl Index)).

hello_elem(versionbitmap) -> 1.
