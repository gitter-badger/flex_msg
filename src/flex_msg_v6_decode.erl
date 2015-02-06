-module(flex_msg_v6_decode).

-export([do/1]).

-include("ofp_v6.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

do(<<?VERSION:8, ?OFPT_HELLO:8, _Length:16, Xid:32, BodyBin/bytes>>) ->
    Body = hello_elements(BodyBin, []),
    #ofp_hello{ xid = Xid, elements = Body }.

hello_elements(<<>>, Acc) -> Acc;
hello_elements(Binary, Acc) ->
    <<TypeInt:16, SizeInt:16, Rest/bytes>> = Binary,
    Type = hello_elem(TypeInt),
    BitmapBinLength = SizeInt - 4,
    <<BitmapBin:BitmapBinLength/bytes, Rest2/bytes>> = Rest,
    Acc2 = case Type of
               versionbitmap ->
                   [{versionbitmap, bitmap(BitmapBin)}|Acc];
               _ ->
                   Acc
           end,
    hello_elements(Rest2, Acc2).

bitmap(Binary) -> bitmap(Binary, 0, []).

bitmap(<<>>, _, Acc) -> Acc;
bitmap(<<Int:32, Rest/bytes>>, Base, Acc) ->
    Acc2 = bitmap(Int, 0, Base, Acc),
    bitmap(Rest, Base + 32, Acc2).

bitmap(_, Index, _, Acc) when Index >= 32 -> Acc;
bitmap(Int, Index, Base, Acc)  when Int band (1 bsl Index) == 0 ->
    bitmap(Int, Index + 1, Base, Acc);
bitmap(Int, Index, Base, Acc) ->
    bitmap(Int, Index + 1, Base, [Base + Index|Acc]).

hello_elem(1) -> versionbitmap;
hello_elem(TypeInt) -> TypeInt.
