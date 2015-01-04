-module(flex_msg).

-include("flex_msg.hrl").

-export([parse/2]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

parse(Parser, Binary) ->
    case parse(Binary, Parser, []) of
         {ok, NewParser, Messages} ->
            {ok, NewParser, lists:reverse(Messages)};
        {error,Exception} ->
            {error,Exception}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

parse(Binary, #ofp_parser{module = Module, stack = Stack} = Parser, Messages) ->
    NewBinary = <<Stack/binary, Binary/binary>>,
    case Module:decode(NewBinary) of
        {error, binary_too_small} ->
            {ok, Parser#ofp_parser{stack = NewBinary}, Messages};
        {error,Exception} ->
            {error,Exception};
        {ok, Message, Leftovers} ->
            parse(Leftovers, Parser#ofp_parser{stack = <<>>},  [Message | Messages])
    end.
