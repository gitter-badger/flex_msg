-module(flex_msg_v1_utils).

-export([binary_to_flags/3,
         flags_to_binary/4,
         get_enum_name/3,
         get_enum_value/3,
         strip_string/1,
         encode_string/2]).

%%%-----------------------------------------------------------------------------
%%% Helper functions
%%%-----------------------------------------------------------------------------

-spec get_enum_name(atom(), atom(), integer() | atom()) -> integer() | atom().
get_enum_name(EnumMod, Enum, Int) when is_integer(Int) ->
    %% TODO: Check if it's not larger than max
    try
        EnumMod:to_atom(Enum, Int)
    catch
        throw:bad_enum ->
            Int
    end;
get_enum_name(_, _, Atom) when is_atom(Atom)->
    Atom.

-spec get_enum_value(atom(), atom(), integer() | atom()) -> integer() | atom().
get_enum_value(EnumMod, Enum, Atom) when is_atom(Atom) ->
    try
        EnumMod:to_int(Enum, Atom)
    catch
        throw:bad_enum ->
            Atom
    end;
get_enum_value(_, _, Int) when is_integer(Int) ->
    Int.

-spec binary_to_flags(atom(), atom(), binary()) -> [atom()].
binary_to_flags(EnumMod, Type, Binary) ->
    BitSize = bit_size(Binary),
    <<Integer:BitSize>> = Binary,
    binary_to_flags(EnumMod, Type, Integer, BitSize - 1, []).

-spec flags_to_binary(atom(), atom(), [atom()], integer()) -> binary().
flags_to_binary(EnumMod, Type, Flags, Size) ->
    flags_to_binary(EnumMod, Type, Flags, <<0:(Size*8)>>, Size*8).

-spec strip_string(binary()) -> binary().
strip_string(Binary) ->
    strip_string(Binary, size(Binary) - 1).

-spec strip_string(binary(), integer()) -> binary().
strip_string(Binary, Byte) when Byte >= 0 ->
    case binary:at(Binary, Byte) of
        0 -> strip_string(Binary, Byte - 1);
        _ -> binary:part(Binary, 0, Byte + 1)
    end;
strip_string(_, _) ->
    <<>>.

-spec encode_string(string() | binary(), integer()) -> binary().
encode_string(String, Length) when is_list(String) ->
    encode_string(list_to_binary(String), Length);
encode_string(Binary, Length) when byte_size(Binary) >= Length - 1 ->
    Null = <<0:8>>,
    <<Binary:(Length - 1)/bytes, Null/bytes>>;
encode_string(Binary, Length) ->
    PaddingLength = (Length - byte_size(Binary)) * 8,
    Padding = <<0:PaddingLength>>,
    <<Binary/bytes, Padding/bytes>>.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec binary_to_flags(atom(), atom(), integer(), integer(), [atom()]) ->
                             [atom()].
binary_to_flags(EnumMod, Type, Integer, Bit, Flags) when Bit >= 0 ->
    case 0 /= (Integer band (1 bsl Bit)) of
        true ->
            Flag = EnumMod:to_atom(Type, Bit),
            binary_to_flags(EnumMod, Type, Integer, Bit - 1, [Flag | Flags]);
        false ->
            binary_to_flags(EnumMod, Type, Integer, Bit - 1, Flags)
    end;
binary_to_flags(_, _, _, _, Flags) ->
    Flags.

-spec flags_to_binary(atom(), atom(), [atom()], binary(), integer()) -> binary().
flags_to_binary(_, _, [], Binary, _) ->
    Binary;
flags_to_binary(EnumMod, Type, [Flag | Rest], Binary, BitSize) ->
    <<Binary2:BitSize>> = Binary,
    Bit = EnumMod:to_int(Type, Flag),
    NewBinary = (Binary2 bor (1 bsl Bit)),
    flags_to_binary(EnumMod, Type, Rest, <<NewBinary:BitSize>>, BitSize).
