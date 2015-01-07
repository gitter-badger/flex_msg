-module(flex_msg_nx_decode).

-export([do/1]).

-include("ofp_nx.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

do(<<?NXT_FLOW_MOD_TABLE_ID:32, SetInt:8, _:7/bytes>>) ->
    Set = case SetInt of
             0 -> false;
             _ -> true
         end,
    #nicira_header{ sub_type = flow_mod_table_id,
                    body = #nx_flow_mod_table_id{ set = Set }}.
