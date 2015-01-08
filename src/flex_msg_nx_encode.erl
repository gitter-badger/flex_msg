-module(flex_msg_nx_encode).

-export([do/1]).

-include("ofp_nx.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

do(#nicira_header{ sub_type = flow_mod_table_id,
                   body = #nx_flow_mod_table_id{ set = Set }}) ->
    SetInt = case Set of
                 true  -> 1;
                 false -> 0
             end,
    <<?NXT_FLOW_MOD_TABLE_ID:32, SetInt:8, 0:56>>.
