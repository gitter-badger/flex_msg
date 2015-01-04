-module(flex_msg_v1_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).

hello_test() ->
    Msg = ?MODNAME:hello(),
    ?assertEqual(messages(hello), Msg).

openflow_error_test() ->
    Msg = ?MODNAME:openflow_error(hello_failed, incompatible, <<>>),
    ?assertEqual(messages(openflow_error), Msg).

echo_request_test() ->
    Msg = ?MODNAME:echo_request(<<>>),
    ?assertEqual(messages(echo_request), Msg).

echo_reply_test() ->
    Msg = ?MODNAME:echo_reply(<<>>),
    ?assertEqual(messages(echo_reply), Msg).

barrier_test() ->
    Msg = ?MODNAME:barrier(),
    ?assertEqual(messages(barrier), Msg).

vendor_test() ->
    Msg = ?MODNAME:vendor(1234, <<"nazo">>),
    ?assertEqual(messages(vendor), Msg).

get_features_test() ->
    Msg = ?MODNAME:get_features(),
    ?assertEqual(messages(get_features), Msg).

get_config_test() ->
    Msg = ?MODNAME:get_config(),
    ?assertEqual(messages(get_config), Msg).

set_config_test() ->
    Msg = ?MODNAME:set_config([], 128),
    ?assertEqual(messages(set_config), Msg).

set_port_up_test() ->
    Msg = ?MODNAME:set_port_up(1, <<0,0,0,0,0,1>>),
    ?assertEqual(messages(set_port_up), Msg).

set_port_down_test() ->
    Msg = ?MODNAME:set_port_down(1, <<0,0,0,0,0,1>>),
    ?assertEqual(messages(set_port_down), Msg).

set_port_packet_in_test() ->
    Msg = ?MODNAME:set_port_packet_in(1, <<0,0,0,0,0,1>>),
    ?assertEqual(messages(set_port_packet_in), Msg).

set_port_no_packet_in_test() ->
    Msg = ?MODNAME:set_port_no_packet_in(1, <<0,0,0,0,0,1>>),
    ?assertEqual(messages(set_port_no_packet_in), Msg).

set_port_modes_test() ->
    Msg = ?MODNAME:set_port_modes(1, <<0,0,0,0,0,1>>, ['100mb_fd', '10gb_fd']),
    ?assertEqual(messages(set_port_modes), Msg).

get_queue_config_test() ->
    Msg = ?MODNAME:get_queue_config(1),
    ?assertEqual(messages(get_queue_config), Msg).

get_switch_description_test() ->
    Msg = ?MODNAME:get_switch_description(),
    ?assertEqual(messages(get_switch_description), Msg).

get_flow_stats_test() ->
    Matches = [{ in_port, 1 },
               { dl_src, <<0,0,0,0,0,10>> },
               { dl_dst, <<0,0,0,0,0,20>> },
               { dl_vlan, 0 },
               { dl_vlan_pcp, 0 },
               { dl_type, 16#800 },
               { nw_tos, 0 },
               { nw_proto, 1 },
               { nw_src, <<10,0,0,0>>, 8 },
               { nw_dst, <<20,0,0,0>>, 8 },
               { tp_src, 8 },
               { tp_dst, 0 }],
    Msg = ?MODNAME:get_flow_stats(Matches, 0),
    ?assertEqual(messages(get_flow_stats), Msg).

get_aggregate_stats_test() ->
    Matches = [{ in_port, 1 },
               { dl_src, <<0,0,0,0,0,10>> },
               { dl_dst, <<0,0,0,0,0,20>> },
               { dl_vlan, 0 },
               { dl_vlan_pcp, 0 },
               { dl_type, 16#800 },
               { nw_tos, 0 },
               { nw_proto, 1 },
               { nw_src, <<10,0,0,0>>, 8 },
               { nw_dst, <<20,0,0,0>>, 8 },
               { tp_src, 8 },
               { tp_dst, 0 }],
    Msg = ?MODNAME:get_aggregate_stats(Matches, 0),
    ?assertEqual(messages(get_aggregate_stats), Msg).

get_table_stats_test() ->
    Msg = ?MODNAME:get_table_stats(),
    ?assertEqual(messages(get_table_stats), Msg).

get_port_stats_all_test() ->
    Msg = ?MODNAME:get_port_stats_all(),
    ?assertEqual(messages(get_port_stats_all), Msg).

get_port_stats_test() ->
    Msg = ?MODNAME:get_port_stats(1),
    ?assertEqual(messages(get_port_stats), Msg).

send_packet_test() ->
    Data = <<0, 38, 130, 235, 234, 209, 0, 22, 157, 29, 156,
             196, 8, 0, 69, 0, 0, 50, 0, 0, 0, 0, 128, 1, 18,
             121, 192, 168, 83, 3, 192, 168, 83, 254, 8, 0,
             246, 255, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
    Actions = [{ set_vlan_vid, 10 },
               { set_vlan_priority, 0 },
               { strip_vlan_header },
               { set_eth_src_addr, <<0,0,0,0,0,1>> },
               { set_eth_dst_addr, <<0,0,0,0,0,2>> },
               { set_ip_src_addr, <<10,0,0,1>> },
               { set_ip_dst_addr, <<20,0,0,2>> },
               { set_ip_tos, 16#b8 },
               { set_transport_src_port, 8 },
               { set_transport_dst_port, 0 },
               { enqueue, 1, 1 },
               { vendor_action, <<"nazo">> },
               { send_out_port, 1, 128 }],
    Msg = ?MODNAME:send_packet(Data, Actions),
    io:format("Msg: ~w~n", [Msg]),
    ?assertEqual(messages(send_packet), Msg).

add_flow_test() ->
    Actions = [{ set_vlan_vid, 10 },
               { set_vlan_priority, 0 },
               { strip_vlan_header },
               { set_eth_src_addr, <<0,0,0,0,0,1>> },
               { set_eth_dst_addr, <<0,0,0,0,0,2>> },
               { set_ip_src_addr, <<10,0,0,1>> },
               { set_ip_dst_addr, <<20,0,0,2>> },
               { set_ip_tos, 16#b8 },
               { set_transport_src_port, 8 },
               { set_transport_dst_port, 0 },
               { enqueue, 1, 1 },
               { vendor_action, <<"nazo">> },
               { send_out_port, 1, 128 }],
    Matches = [{ in_port, 1 },
               { dl_src, <<0,0,0,0,0,10>> },
               { dl_dst, <<0,0,0,0,0,20>> },
               { dl_vlan, 0 },
               { dl_vlan_pcp, 0 },
               { dl_type, 16#800 },
               { nw_tos, 0 },
               { nw_proto, 1 },
               { nw_src, <<10,0,0,0>>, 8 },
               { nw_dst, <<20,0,0,0>>, 8 },
               { tp_src, 8 },
               { tp_dst, 0 }],
    Msg = ?MODNAME:add_flow(<<0:64>>, 16#ffff, 0, 0, Matches, Actions),
    io:format("Msg: ~w~n", [Msg]),
    ?assertEqual(messages(add_flow), Msg).

modify_flow_test() ->
    Actions = [{ set_vlan_vid, 10 },
               { set_vlan_priority, 0 },
               { strip_vlan_header },
               { set_eth_src_addr, <<0,0,0,0,0,1>> },
               { set_eth_dst_addr, <<0,0,0,0,0,2>> },
               { set_ip_src_addr, <<10,0,0,1>> },
               { set_ip_dst_addr, <<20,0,0,2>> },
               { set_ip_tos, 16#b8 },
               { set_transport_src_port, 8 },
               { set_transport_dst_port, 0 },
               { enqueue, 1, 1 },
               { vendor_action, <<"nazo">> },
               { send_out_port, 1, 128 }],
    Matches = [{ in_port, 1 },
               { dl_src, <<0,0,0,0,0,10>> },
               { dl_dst, <<0,0,0,0,0,20>> },
               { dl_vlan, 0 },
               { dl_vlan_pcp, 0 },
               { dl_type, 16#800 },
               { nw_tos, 0 },
               { nw_proto, 1 },
               { nw_src, <<10,0,0,0>>, 8 },
               { nw_dst, <<20,0,0,0>>, 8 },
               { tp_src, 8 },
               { tp_dst, 0 }],
    Msg = ?MODNAME:modify_flow(<<0:64>>, 16#ffff, 0, 0, Matches, Actions),
    io:format("Msg: ~w~n", [Msg]),
    ?assertEqual(messages(modify_flow), Msg).

delete_flow_test() ->
    Matches = [{ in_port, 1 },
               { dl_src, <<0,0,0,0,0,10>> },
               { dl_dst, <<0,0,0,0,0,20>> },
               { dl_vlan, 0 },
               { dl_vlan_pcp, 0 },
               { dl_type, 16#800 },
               { nw_tos, 0 },
               { nw_proto, 1 },
               { nw_src, <<10,0,0,0>>, 8 },
               { nw_dst, <<20,0,0,0>>, 8 },
               { tp_src, 8 },
               { tp_dst, 0 }],
    Msg = ?MODNAME:delete_flow(<<0:64>>, Matches, none),
    io:format("Msg: ~w~n", [Msg]),
    ?assertEqual(messages(delete_flow), Msg).

messages(Type) ->
    case Type of
        hello ->
            #ofp_hello{};
        openflow_error ->
            #ofp_error_msg{ type = hello_failed, code = incompatible, data = <<>> };
        echo_request ->
            #ofp_echo_request{ data = <<>> };
        echo_reply ->
            #ofp_echo_reply{ data = <<>> };
        barrier ->
            #ofp_barrier_request{};
        vendor ->
            #ofp_vendor_header{ vendor = 1234, data = <<"nazo">> };
        get_features ->
            #ofp_features_request{};
        get_config ->
            #ofp_get_config_request{};
        set_config ->
            #ofp_switch_config{ flags = [], miss_send_len = 128 };
        set_port_up ->
            #ofp_port_mod{ port_no = 1,
                           hw_addr = <<0,0,0,0,0,1>>,
                           config = [],
                           mask = [port_down] };
        set_port_down ->
            #ofp_port_mod{ port_no = 1,
                           hw_addr = <<0,0,0,0,0,1>>,
                           config = [port_down],
                           mask = [port_down] };
        set_port_packet_in ->
            #ofp_port_mod{ port_no = 1,
                           hw_addr = <<0,0,0,0,0,1>>,
                           config = [],
                           mask = [no_packet_in] };
        set_port_no_packet_in ->
            #ofp_port_mod{ port_no = 1,
                           hw_addr = <<0,0,0,0,0,1>>,
                           config = [no_packet_in],
                           mask = [no_packet_in] };
        set_port_modes ->
            #ofp_port_mod{ port_no = 1,
                           hw_addr = <<0,0,0,0,0,1>>,
                           config = [],
                           mask = [],
                           advertised = ['100mb_fd', '10gb_fd']};
        get_queue_config ->
            #ofp_queue_get_config_request{ port_no = 1 };
        get_switch_description ->
            #ofp_stats_request{ type = desc, flags = [],
                                body = #ofp_desc_stats_request{} };
        get_flow_stats ->
            Matches = #ofp_match{ in_port = 1,
                                  dl_src = <<0,0,0,0,0,10>>,
                                  dl_dst = <<0,0,0,0,0,20>>,
                                  dl_vlan_pcp = 0,
                                  dl_vlan = 0,
                                  dl_type = 16#800,
                                  nw_tos = 0,
                                  nw_proto = 1,
                                  nw_src = <<10,0,0,0>>,
                                  nw_src_mask = 8,
                                  nw_dst = <<20,0,0,0>>,
                                  nw_dst_mask = 8,
                                  tp_src = 8,
                                  tp_dst = 0 },
            Body = #ofp_flow_stats_request{ match = Matches, table_id = 0 },
            #ofp_stats_request{ type = flow, body = Body };
        get_aggregate_stats ->
            Matches = #ofp_match{ in_port = 1,
                                  dl_src = <<0,0,0,0,0,10>>,
                                  dl_dst = <<0,0,0,0,0,20>>,
                                  dl_vlan_pcp = 0,
                                  dl_vlan = 0,
                                  dl_type = 16#800,
                                  nw_tos = 0,
                                  nw_proto = 1,
                                  nw_src = <<10,0,0,0>>,
                                  nw_src_mask = 8,
                                  nw_dst = <<20,0,0,0>>,
                                  nw_dst_mask = 8,
                                  tp_src = 8,
                                  tp_dst = 0 },
            Body = #ofp_aggregate_stats_request{ match = Matches, table_id = 0 },
            #ofp_stats_request{ type = aggregate, body = Body };
        get_table_stats ->
            #ofp_stats_request{ type = table, body = #ofp_table_stats_request{} };
        get_port_stats_all ->
             #ofp_stats_request{ type = port,
                                 body = #ofp_port_stats_request{ port_no = none }};
        get_port_stats ->
            #ofp_stats_request{ type = port,
                                body = #ofp_port_stats_request{ port_no = 1 }};
        send_packet ->
            Data = <<0, 38, 130, 235, 234, 209, 0, 22, 157, 29, 156,
                     196, 8, 0, 69, 0, 0, 50, 0, 0, 0, 0, 128, 1, 18,
                     121, 192, 168, 83, 3, 192, 168, 83, 254, 8, 0,
                     246, 255, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
            Actions = [#ofp_action_header{ type = set_vlan_vid,
                                           body = #ofp_action_vlan_vid{ vlan_vid = 10 }},
                       #ofp_action_header{ type = set_vlan_pcp,
                                           body = #ofp_action_vlan_pcp{ vlan_pcp = 0 }},
                       #ofp_action_header{ type = strip_vlan,
                                           body = #ofp_action_strip_vlan{} },
                       #ofp_action_header{ type = set_dl_src,
                                           body = #ofp_action_dl_addr{
                                                     dl_addr = <<0,0,0,0,0,1>> }},
                       #ofp_action_header{ type = set_dl_dst,
                                           body = #ofp_action_dl_addr{
                                                     dl_addr = <<0,0,0,0,0,2>> }},
                       #ofp_action_header{ type = set_nw_src,
                                           body = #ofp_action_nw_addr{
                                                     nw_addr = <<10,0,0,1>> }},
                       #ofp_action_header{ type = set_nw_dst,
                                           body = #ofp_action_nw_addr{
                                                     nw_addr = <<20,0,0,2>> }},
                       #ofp_action_header{ type = set_nw_tos,
                                           body = #ofp_action_nw_tos{ nw_tos = 16#b8 }},
                       #ofp_action_header{ type = set_tp_src,
                                           body = #ofp_action_tp_port{ tp_port = 8 }},
                       #ofp_action_header{ type = set_tp_dst,
                                           body = #ofp_action_tp_port{ tp_port = 0 }},
                       #ofp_action_header{ type = enqueue,
                                           body = #ofp_action_enqueue{ port = 1,
                                                                       queue_id = 1}},
                       #ofp_action_header{ type = vendor,
                                           body = #ofp_action_vendor{
                                                     vendor = <<"nazo">> }},
                      #ofp_action_header{ type = output,
                                          body = #ofp_action_output{ port = 1,
                                                                     max_len = 128 }}] ,
            #ofp_packet_out{ buffer_id = no_buffer,
                             in_port = none,
                             actions = Actions,
                             data = Data };
        add_flow ->
            Actions = [#ofp_action_header{ type = set_vlan_vid,
                                           body = #ofp_action_vlan_vid{ vlan_vid = 10 }},
                       #ofp_action_header{ type = set_vlan_pcp,
                                           body = #ofp_action_vlan_pcp{ vlan_pcp = 0 }},
                       #ofp_action_header{ type = strip_vlan,
                                           body = #ofp_action_strip_vlan{} },
                       #ofp_action_header{ type = set_dl_src,
                                           body = #ofp_action_dl_addr{
                                                     dl_addr = <<0,0,0,0,0,1>> }},
                       #ofp_action_header{ type = set_dl_dst,
                                           body = #ofp_action_dl_addr{
                                                     dl_addr = <<0,0,0,0,0,2>> }},
                       #ofp_action_header{ type = set_nw_src,
                                           body = #ofp_action_nw_addr{
                                                     nw_addr = <<10,0,0,1>> }},
                       #ofp_action_header{ type = set_nw_dst,
                                           body = #ofp_action_nw_addr{
                                                     nw_addr = <<20,0,0,2>> }},
                       #ofp_action_header{ type = set_nw_tos,
                                           body = #ofp_action_nw_tos{ nw_tos = 16#b8 }},
                       #ofp_action_header{ type = set_tp_src,
                                           body = #ofp_action_tp_port{ tp_port = 8 }},
                       #ofp_action_header{ type = set_tp_dst,
                                           body = #ofp_action_tp_port{ tp_port = 0 }},
                       #ofp_action_header{ type = enqueue,
                                           body = #ofp_action_enqueue{ port = 1,
                                                                       queue_id = 1}},
                       #ofp_action_header{ type = vendor,
                                           body = #ofp_action_vendor{
                                                     vendor = <<"nazo">> }},
                      #ofp_action_header{ type = output,
                                          body = #ofp_action_output{ port = 1,
                                                                     max_len = 128 }}] ,
            #ofp_flow_mod{ match = #ofp_match{ in_port = 1,
                                               dl_src = <<0,0,0,0,0,10>>,
                                               dl_dst = <<0,0,0,0,0,20>>,
                                               dl_vlan_pcp = 0,
                                               dl_vlan = 0,
                                               dl_type = 16#800,
                                               nw_tos = 0,
                                               nw_proto = 1,
                                               nw_src = <<10,0,0,0>>,
                                               nw_src_mask = 8,
                                               nw_dst = <<20,0,0,0>>,
                                               nw_dst_mask = 8,
                                               tp_src = 8,
                                               tp_dst = 0 },
                           cookie = <<0:64>>,
                           priority = 16#ffff,
                           idle_timeout = 0,
                           hard_timeout = 0,
                           command = add,
                           flags = [send_flow_rem],
                           actions = Actions };
        modify_flow ->
            Actions = [#ofp_action_header{ type = set_vlan_vid,
                                           body = #ofp_action_vlan_vid{ vlan_vid = 10 }},
                       #ofp_action_header{ type = set_vlan_pcp,
                                           body = #ofp_action_vlan_pcp{ vlan_pcp = 0 }},
                       #ofp_action_header{ type = strip_vlan,
                                           body = #ofp_action_strip_vlan{} },
                       #ofp_action_header{ type = set_dl_src,
                                           body = #ofp_action_dl_addr{
                                                     dl_addr = <<0,0,0,0,0,1>> }},
                       #ofp_action_header{ type = set_dl_dst,
                                           body = #ofp_action_dl_addr{
                                                     dl_addr = <<0,0,0,0,0,2>> }},
                       #ofp_action_header{ type = set_nw_src,
                                           body = #ofp_action_nw_addr{
                                                     nw_addr = <<10,0,0,1>> }},
                       #ofp_action_header{ type = set_nw_dst,
                                           body = #ofp_action_nw_addr{
                                                     nw_addr = <<20,0,0,2>> }},
                       #ofp_action_header{ type = set_nw_tos,
                                           body = #ofp_action_nw_tos{ nw_tos = 16#b8 }},
                       #ofp_action_header{ type = set_tp_src,
                                           body = #ofp_action_tp_port{ tp_port = 8 }},
                       #ofp_action_header{ type = set_tp_dst,
                                           body = #ofp_action_tp_port{ tp_port = 0 }},
                       #ofp_action_header{ type = enqueue,
                                           body = #ofp_action_enqueue{ port = 1,
                                                                       queue_id = 1}},
                       #ofp_action_header{ type = vendor,
                                           body = #ofp_action_vendor{
                                                     vendor = <<"nazo">> }},
                      #ofp_action_header{ type = output,
                                          body = #ofp_action_output{ port = 1,
                                                                     max_len = 128 }}] ,
            #ofp_flow_mod{ match = #ofp_match{ in_port = 1,
                                               dl_src = <<0,0,0,0,0,10>>,
                                               dl_dst = <<0,0,0,0,0,20>>,
                                               dl_vlan_pcp = 0,
                                               dl_vlan = 0,
                                               dl_type = 16#800,
                                               nw_tos = 0,
                                               nw_proto = 1,
                                               nw_src = <<10,0,0,0>>,
                                               nw_src_mask = 8,
                                               nw_dst = <<20,0,0,0>>,
                                               nw_dst_mask = 8,
                                               tp_src = 8,
                                               tp_dst = 0 },
                           cookie = <<0:64>>,
                           priority = 16#ffff,
                           idle_timeout = 0,
                           hard_timeout = 0,
                           command = modify,
                           flags = [send_flow_rem],
                           actions = Actions };
        delete_flow ->
            #ofp_flow_mod{ match = #ofp_match{ in_port = 1,
                                               dl_src = <<0,0,0,0,0,10>>,
                                               dl_dst = <<0,0,0,0,0,20>>,
                                               dl_vlan_pcp = 0,
                                               dl_vlan = 0,
                                               dl_type = 16#800,
                                               nw_tos = 0,
                                               nw_proto = 1,
                                               nw_src = <<10,0,0,0>>,
                                               nw_src_mask = 8,
                                               nw_dst = <<20,0,0,0>>,
                                               nw_dst_mask = 8,
                                               tp_src = 8,
                                               tp_dst = 0 },
                           cookie = <<0:64>>,
                           command = delete,
                           flags = [],
                           out_port = none }
    end.
