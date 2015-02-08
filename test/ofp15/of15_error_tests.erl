-module(of15_error_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v6.hrl").

-define(MODNAME, flex_msg_v6).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

error_test_() ->
    [{ "hello failed incompatible error", fun hello_failed_incompatible/0 },
     { "hello failed eperm error", fun hello_failed_eperm/0 },

     % request tests
     { "bad request with bad verson code", fun bad_request_bad_version/0 },
     { "bad request with bad type code", fun bad_request_bad_type/0 },
     { "bad request with bad multipart code", fun bad_request_bad_multipart/0 },
     { "bad request with bad experimenter code", fun bad_request_bad_experimenter/0 },
     { "bad request with bad exp type code", fun bad_request_bad_exp_type/0 },
     { "bad request with buffer empty code", fun bad_request_buffer_empty/0 },
     { "bad request with buffer unknown code", fun bad_request_buffer_unknown/0 },
     { "bad request with bad table id code", fun bad_request_bad_table_id/0 },
     { "bad request with is slave code", fun bad_request_is_slave/0 },
     { "bad request with bad_port code", fun bad_request_bad_port/0 },
     { "bad request with bad packet code", fun bad_request_bad_packet/0 },
     { "bad request with multipart bufffer overflow code",
       fun bad_request_multipart_buffer_overflow/0 },
     { "bad request with multipart request timeout",
       fun bad_request_multipart_request_timeout/0 },
     { "bad request with multipart reply timeout code",
       fun bad_request_multipart_reply_timeout/0 },
     { "bad request with multipart bad sched code",
       fun bad_request_multipart_bad_sched/0 },
     { "bad request with pipeline fields only code",
       fun bad_request_pipeline_fields_only/0 },
     { "bad request with unknown code", fun bad_request_unknown/0 },

     % action tests
     { "bad action with bad_type code", fun bad_action_bad_type/0 },
     { "bad action with bad_len  code", fun bad_action_bad_len/0 },
     { "bad action with bad_experimenter  code", fun bad_action_bad_experimenter/0 },
     { "bad action with bad_exp_type  code", fun bad_action_bad_exp_type/0 },
     { "bad action with bad_out_port  code", fun bad_action_bad_out_port/0 },
     { "bad action with bad_argument  code", fun bad_action_bad_argument/0 },
     { "bad action with eperm  code", fun bad_action_eperm/0 },
     { "bad action with too_many  code", fun bad_action_too_many/0 },
     { "bad action with bad_queue  code", fun bad_action_bad_queue/0 },
     { "bad action with bad_out_group  code", fun bad_action_bad_out_group/0 },
     { "bad action with match_inconsistent  code", fun bad_action_match_inconsistent/0 },
     { "bad action with unsupported_order  code", fun bad_action_unsupported_order/0 },
     { "bad action with bad_tag  code", fun bad_action_bad_tag/0 },
     { "bad action with bad_set_type  code", fun bad_action_bad_set_type/0 },
     { "bad action with bad_set_len  code", fun bad_action_bad_set_len/0 },
     { "bad action with bad_set_argument code", fun bad_action_bad_set_argument/0 },
     { "bad action with bad_set_mask code", fun bad_action_bad_set_mask/0 },

     % instruction tests
     { "bad instruction with unknown_inst code",
       fun bad_instruction_unknown_inst/0 },
     { "bad instruction with unsup_inst code",
       fun bad_instruction_unsup_inst/0 },
     { "bad instruction with bad_table_id code",
       fun bad_instruction_bad_table_id/0 },
     { "bad instruction with unsup_metadata code",
       fun bad_instruction_unsup_metadata/0 },
     { "bad instruction with unsup_metadata_mask code",
       fun bad_instruction_unsup_metadata_mask/0 },
     { "bad instruction with bad_experimenter code",
       fun bad_instruction_bad_experimenter/0 },
     { "bad instruction with bad_exp_type code",
       fun bad_instruction_bad_exp_type/0 },
     { "bad instruction with bad_len code",
       fun bad_instruction_bad_len/0 },
     { "bad instruction with eperm code",
       fun bad_instruction_eperm/0 },
     { "bad instruction with dup_inst code",
       fun bad_instruction_dup_inst/0 },

     % match tests
     { "bad match with bad_type code",
       fun bad_match_bad_type/0 },
     { "bad match with bad_len code",
       fun bad_match_bad_len/0 },
     { "bad match with bad_tag code",
       fun bad_match_bad_tag/0 },
     { "bad match with bad_dl_addr_mask code",
       fun bad_match_bad_dl_addr_mask/0 },
     { "bad match with bad_nw_addr_mask code",
       fun bad_match_bad_nw_addr_mask/0 },
     { "bad match with bad_wildcards code",
       fun bad_match_bad_wildcards/0 },
     { "bad match with bad_field code",
       fun bad_match_bad_field/0 },
     { "bad match with bad_value code",
       fun bad_match_bad_value/0 },
     { "bad match with bad_mask code",
       fun bad_match_bad_mask/0 },
     { "bad match with bad_prereq code",
       fun bad_match_bad_prereq/0 },
     { "bad match with dup_field code",
       fun bad_match_dup_field/0 },
     { "bad match with eperm code",
       fun bad_match_eperm/0 },

     % flow_mod tests
     { "flow_mod failed with unknown code",
       fun flow_mod_failed_unknown/0 },
     { "flow_mod failed with table_full code",
       fun flow_mod_failed_table_full/0 },
     { "flow_mod failed with bad_table_id code",
       fun flow_mod_failed_bad_table_id/0 },
     { "flow_mod failed with overlap code",
       fun flow_mod_failed_overlap/0 },
     { "flow_mod failed with eperm code",
       fun flow_mod_failed_eperm/0 },
     { "flow_mod failed with bad_timeout code",
       fun flow_mod_failed_bad_timeout/0 },
     { "flow_mod failed with bad_command code",
       fun flow_mod_failed_bad_command/0 },
     { "flow_mod failed with bad_flags code",
       fun flow_mod_failed_bad_flags/0 },
     { "flow_mod failed with cant_sync code",
       fun flow_mod_failed_cant_sync/0 },
     { "flow_mod failed with bad_priority code",
       fun flow_mod_failed_bad_priority/0 },
     { "flow_mod failed with is_sync code",
       fun flow_mod_failed_is_sync/0 },

     % group mod tests
     { "group_mod failed with group_exists code",
       fun group_mod_failed_group_exists/0 },
     { "group_mod failed with invaild_group code",
       fun group_mod_failed_invaild_group/0 },
     { "group_mod failed with weight_unsupported code",
       fun group_mod_failed_weight_unsupported/0 },
     { "group_mod failed with out_of_groups code",
       fun group_mod_failed_out_of_groups/0 },
     { "group_mod failed with out_of_buckets code",
       fun group_mod_failed_out_of_buckets/0 },
     { "group_mod failed with chaining_unsupported code",
       fun group_mod_failed_chaining_unsupported/0 },
     { "group_mod failed with watch_unsupported code",
       fun group_mod_failed_watch_unsupported/0 },
     { "group_mod failed with loop code",
       fun group_mod_failed_loop/0 },
     { "group_mod failed with unknown_group code",
       fun group_mod_failed_unknown_group/0 },
     { "group_mod failed with chained_group code",
       fun group_mod_failed_chained_group/0 },
     { "group_mod failed with bad_type code",
       fun group_mod_failed_bad_type/0 },
     { "group_mod failed with bad_command code",
       fun group_mod_failed_bad_command/0 },
     { "group_mod failed with bad_bucket code",
       fun group_mod_failed_bad_bucket/0 },
     { "group_mod failed with bad_watch code",
       fun group_mod_failed_bad_watch/0 },
     { "group_mod failed with eperm code",
       fun group_mod_failed_eperm/0 },
     { "group_mod failed with unknown_bucket code",
       fun group_mod_failed_unknown_bucket/0 },
     { "group_mod failed with bucket_exists code",
       fun group_mod_failed_bucket_exists/0 },

     % port mod tests
     { "port_mod failed with bad_port code",
       fun port_mod_failed_bad_port/0 },
     { "port_mod failed with bad_hw_addr code",
       fun port_mod_failed_bad_hw_addr/0 },
     { "port_mod failed with bad_config code",
       fun port_mod_failed_bad_config/0 },
     { "port_mod failed with bad_adverise code",
       fun port_mod_failed_bad_adverise/0 },
     { "port_mod failed with eperm code",
       fun port_mod_failed_eperm/0 },

     % table_mod tests
     { "table_mod failed with bad_table code",
       fun table_mod_failed_bad_table/0 },
     { "table_mod failed with bad_config code",
       fun table_mod_failed_bad_config/0 },
     { "table_mod failed with eperm code",
       fun table_mod_failed_eperm/0 },

     % queue op tests
     { "queue_op failed with bad_port code",
       fun queue_op_failed_bad_port/0 },
     { "queue_op failed with bad_queue code",
       fun queue_op_failed_bad_queue/0 },
     { "queue_op failed with eperm code",
       fun queue_op_failed_eperm/0 },

     % switch config tests
     { "switch_config failed with bad_flags code",
       fun switch_config_failed_bad_flags/0 },
     { "switch_config failed with bad_len code",
       fun switch_config_failed_bad_len/0 },
     { "switch_config failed with eperm code",
       fun switch_config_failed_eperm/0 },

     % role request tests
     { "role_request failed with stale code",
       fun role_request_failed_stale/0 },
     { "role_request failed with unsup code",
       fun role_request_failed_unsup/0 },
     { "role_request failed with bad_role code",
       fun role_request_failed_bad_role/0 },
     { "role_request failed with id_unsup code",
       fun role_request_failed_id_unsup/0 },
     { "role_request failed with id_in_use code",
       fun role_request_failed_id_in_use/0 },

     % meter_mod tests
     { "meter_mod failed with unknown code",
       fun meter_mod_failed_unknown/0 },
     { "meter_mod failed with meter_exists code",
       fun meter_mod_failed_meter_exists/0 },
     { "meter_mod failed with invaild_meter code",
       fun meter_mod_failed_invaild_meter/0 },
     { "meter_mod failed with unknown_meter code",
       fun meter_mod_failed_unknown_meter/0 },
     { "meter_mod failed with bad_command code",
       fun meter_mod_failed_bad_command/0 },
     { "meter_mod failed with bad_flags code",
       fun meter_mod_failed_bad_flags/0 },
     { "meter_mod failed with bad_rate code",
       fun meter_mod_failed_bad_rate/0 },
     { "meter_mod failed with bad_burst code",
       fun meter_mod_failed_bad_burst/0 },
     { "meter_mod failed with bad_band code",
       fun meter_mod_failed_bad_band/0 },
     { "meter_mod failed with bad_band_value code",
       fun meter_mod_failed_bad_band_value/0 },
     { "meter_mod failed with out_of_meters code",
       fun meter_mod_failed_out_of_meters/0 },
     { "meter_mod failed with out_of_bands code",
       fun meter_mod_failed_out_of_bands/0 },

     % table features
     { "table_features failed with bad_table code",
       fun table_features_failed_bad_table/0 },
     { "table_features failed with bad_metadata code",
       fun table_features_failed_bad_metadata/0 },
     { "table_features failed with eperm code",
       fun table_features_failed_eperm/0 },
     { "table_features failed with bad_capa code",
       fun table_features_failed_bad_capa/0 },
     { "table_features failed with bad_max_ent code",
       fun table_features_failed_bad_max_ent/0 },
     { "table_features failed with bad_features code",
       fun table_features_failed_bad_features/0 },
     { "table_features failed with bad_command code",
       fun table_features_failed_bad_command/0 },
     { "table_features failed with too_many code",
       fun table_features_failed_too_many/0 },

     % bad property tests
     { "bad_property with bad_type code",
       fun bad_property_bad_type/0 },
     { "bad_property with bad_len code",
       fun bad_property_bad_len/0 },
     { "bad_property with bad_value code",
       fun bad_property_bad_value/0 },
     { "bad_property with too_many code",
       fun bad_property_too_many/0 },
     { "bad_property with dup_type code",
       fun bad_property_dup_type/0 },
     { "bad_property with bad_experimenter code",
       fun bad_property_bad_experimenter/0 },
     { "bad_property with bad_exp_type code",
       fun bad_property_bad_exp_type/0 },
     { "bad_property with bad_exp_value code",
       fun bad_property_bad_exp_value/0 },
     { "bad_property with eperm code",
       fun bad_property_eperm/0 },

     % async config tests
     { "async_config_failed with invalid code",
       fun async_config_failed_invalid/0 },
     { "async_config_failed with unsupported code",
       fun async_config_failed_unsupported/0 },
     { "async_config_failed with eperm code",
       fun async_config_failed_eperm/0 },

     % flow_monitor tests
     { "flow_monitor_failed with unknown code",
       fun flow_monitor_failed_unknown/0 },
     { "flow_monitor_failed with monitor_exists code",
       fun flow_monitor_failed_monitor_exists/0 },
     { "flow_monitor_failed with invalid_monitor code",
       fun flow_monitor_failed_invalid_monitor/0 },
     { "flow_monitor_failed with unknown_monitor code",
       fun flow_monitor_failed_unknown_monitor/0 },
     { "flow_monitor_failed with bad_command code",
       fun flow_monitor_failed_bad_command/0 },
     { "flow_monitor_failed with bad_flags code",
       fun flow_monitor_failed_bad_flags/0 },
     { "flow_monitor_failed with bad_table_id code",
       fun flow_monitor_failed_bad_table_id/0 },
     { "flow_monitor_failed with bad_out code",
       fun flow_monitor_failed_bad_out/0 },

     % bundle tests
     { "bundle_failed with unknown code",
       fun bundle_failed_unknown/0 },
     { "bundle_failed with eperm code",
       fun bundle_failed_eperm/0 },
     { "bundle_failed with bad_id code",
       fun bundle_failed_bad_id/0 },
     { "bundle_failed with bundle_exists code",
       fun bundle_failed_bundle_exists/0 },
     { "bundle_failed with bundle_closed code",
       fun bundle_failed_bundle_closed/0 },
     { "bundle_failed with out_of_bundles code",
       fun bundle_failed_out_of_bundles/0 },
     { "bundle_failed with bad_type code",
       fun bundle_failed_bad_type/0 },
     { "bundle_failed with bad_flags code",
       fun bundle_failed_bad_flags/0 },
     { "bundle_failed with msg_bad_len code",
       fun bundle_failed_msg_bad_len/0 },
     { "bundle_failed with msg_bad_xid code",
       fun bundle_failed_msg_bad_xid/0 },
     { "bundle_failed with msg_unsup code",
       fun bundle_failed_msg_unsup/0 },
     { "bundle_failed with msg_conflict code",
       fun bundle_failed_msg_conflict/0 },
     { "bundle_failed with msg_too_many code",
       fun bundle_failed_msg_too_many/0 },
     { "bundle_failed with msg_failed code",
       fun bundle_failed_msg_failed/0 },
     { "bundle_failed with timeout code",
       fun bundle_failed_timeout/0 },
     { "bundle_failed with bundle_in_progress code",
       fun bundle_failed_bundle_in_progress/0 },
     { "bundle_failed with sched_not_supported code",
       fun bundle_failed_sched_not_supported/0 },
     { "bundle_failed with sched_future code",
       fun bundle_failed_sched_future/0 },
     { "bundle_failed with sched_past code",
       fun bundle_failed_sched_past/0 }].

hello_failed_incompatible() ->
    Msg = #ofp_error{ type = hello_failed,
                      code = incompatible,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

hello_failed_eperm() ->
    Msg = #ofp_error{ type = hello_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_version() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_version,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_type() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_multipart() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_multipart,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_experimenter() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_experimenter,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_exp_type() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_exp_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_buffer_empty() ->
    Msg = #ofp_error{ type = bad_request,
                      code = buffer_empty,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_buffer_unknown() ->
    Msg = #ofp_error{ type = bad_request,
                      code = buffer_unknown,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_table_id() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_table_id,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_is_slave() ->
    Msg = #ofp_error{ type = bad_request,
                      code = is_slave,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_port() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_port,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_packet() ->
    Msg = #ofp_error{ type = bad_request,
                      code = bad_packet,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_multipart_buffer_overflow() ->
    Msg = #ofp_error{ type = bad_request,
                      code = multipart_buffer_overflow,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_multipart_request_timeout() ->
    Msg = #ofp_error{ type = bad_request,
                      code = multipart_request_timeout,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_multipart_reply_timeout() ->
    Msg = #ofp_error{ type = bad_request,
                      code = multipart_reply_timeout,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_multipart_bad_sched() ->
    Msg = #ofp_error{ type = bad_request,
                      code = multipart_bad_sched,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_pipeline_fields_only() ->
    Msg = #ofp_error{ type = bad_request,
                      code = pipeline_fields_only,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_request_unknown() ->
    Msg = #ofp_error{ type = bad_request,
                      code = unknown,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_type() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_len() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_len,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_experimenter() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_experimenter,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_exp_type() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_exp_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_out_port() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_out_port,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_argument() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_argument,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_eperm() ->
    Msg = #ofp_error{ type = bad_action,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_too_many() ->
    Msg = #ofp_error{ type = bad_action,
                      code = too_many,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_queue() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_queue,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_out_group() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_out_group,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_match_inconsistent() ->
    Msg = #ofp_error{ type = bad_action,
                      code = match_inconsistent,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_unsupported_order() ->
    Msg = #ofp_error{ type = bad_action,
                      code = unsupported_order,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_tag() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_tag,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_set_type() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_set_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_set_len() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_set_len,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_set_argument() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_set_argument,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_set_mask() ->
    Msg = #ofp_error{ type = bad_action,
                      code = bad_set_mask,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_unknown_inst() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = unknown_inst,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_unsup_inst() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = unsup_inst,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_bad_table_id() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = bad_table_id,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_unsup_metadata() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = unsup_metadata,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_unsup_metadata_mask() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = unsup_metadata_mask,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_bad_experimenter() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = bad_experimenter,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_bad_exp_type() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = bad_exp_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_bad_len() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = bad_len,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_eperm() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_instruction_dup_inst() ->
    Msg = #ofp_error{ type = bad_instruction,
                      code = dup_inst,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_type() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_len() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_len,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_tag() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_tag,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_dl_addr_mask() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_dl_addr_mask,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_nw_addr_mask() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_nw_addr_mask,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_wildcards() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_wildcards,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_field() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_field,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_value() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_value,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_mask() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_mask,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_bad_prereq() ->
    Msg = #ofp_error{ type = bad_match,
                      code = bad_prereq,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_dup_field() ->
    Msg = #ofp_error{ type = bad_match,
                      code = dup_field,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_match_eperm() ->
    Msg = #ofp_error{ type = bad_match,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_unknown() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = unknown,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_table_full() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = table_full,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_bad_table_id() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = bad_table_id,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_overlap() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = overlap,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_eperm() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_bad_timeout() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = bad_timeout,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_bad_command() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = bad_command,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_bad_flags() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = bad_flags,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_cant_sync() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = cant_sync,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_bad_priority() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = bad_priority,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_is_sync() ->
    Msg = #ofp_error{ type = flow_mod_failed,
                      code = is_sync,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_group_exists() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = group_exists,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_invaild_group() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = invaild_group,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_weight_unsupported() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = weight_unsupported,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_out_of_groups() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = out_of_groups,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_out_of_buckets() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = out_of_buckets,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_chaining_unsupported() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = chaining_unsupported,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_watch_unsupported() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = watch_unsupported,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_loop() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = loop,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_unknown_group() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = unknown_group,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_chained_group() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = chained_group,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_bad_type() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = bad_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_bad_command() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = bad_command,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_bad_bucket() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = bad_bucket,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_bad_watch() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = bad_watch,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_eperm() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_unknown_bucket() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = unknown_bucket,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

group_mod_failed_bucket_exists() ->
    Msg = #ofp_error{ type = group_mod_failed,
                      code = bucket_exists,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

port_mod_failed_bad_port() ->
    Msg = #ofp_error{ type = port_mod_failed,
                      code = bad_port,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

port_mod_failed_bad_hw_addr() ->
    Msg = #ofp_error{ type = port_mod_failed,
                      code = bad_hw_addr,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

port_mod_failed_bad_config() ->
    Msg = #ofp_error{ type = port_mod_failed,
                      code = bad_config,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

port_mod_failed_bad_adverise() ->
    Msg = #ofp_error{ type = port_mod_failed,
                      code = bad_adverise,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

port_mod_failed_eperm() ->
    Msg = #ofp_error{ type = port_mod_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_mod_failed_bad_table() ->
    Msg = #ofp_error{ type = table_mod_failed,
                      code = bad_table,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_mod_failed_bad_config() ->
    Msg = #ofp_error{ type = table_mod_failed,
                      code = bad_config,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_mod_failed_eperm() ->
    Msg = #ofp_error{ type = table_mod_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

queue_op_failed_bad_port() ->
    Msg = #ofp_error{ type = queue_op_failed,
                      code = bad_port,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

queue_op_failed_bad_queue() ->
    Msg = #ofp_error{ type = queue_op_failed,
                      code = bad_queue,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

queue_op_failed_eperm() ->
    Msg = #ofp_error{ type = queue_op_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

switch_config_failed_bad_flags() ->
    Msg = #ofp_error{ type = switch_config_failed,
                      code = bad_flags,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

switch_config_failed_bad_len() ->
    Msg = #ofp_error{ type = switch_config_failed,
                      code = bad_len,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

switch_config_failed_eperm() ->
    Msg = #ofp_error{ type = switch_config_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

role_request_failed_stale() ->
    Msg = #ofp_error{ type = role_request_failed,
                      code = stale,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

role_request_failed_unsup() ->
    Msg = #ofp_error{ type = role_request_failed,
                      code = unsup,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

role_request_failed_bad_role() ->
    Msg = #ofp_error{ type = role_request_failed,
                      code = bad_role,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

role_request_failed_id_unsup() ->
    Msg = #ofp_error{ type = role_request_failed,
                      code = id_unsup,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

role_request_failed_id_in_use() ->
    Msg = #ofp_error{ type = role_request_failed,
                      code = id_in_use,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_unknown() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = unknown,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_meter_exists() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = meter_exists,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_invaild_meter() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = invaild_meter,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_unknown_meter() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = unknown_meter,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_bad_command() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = bad_command,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_bad_flags() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = bad_flags,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_bad_rate() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = bad_rate,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_bad_burst() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = bad_burst,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_bad_band() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = bad_band,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_bad_band_value() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = bad_band_value,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_out_of_meters() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = out_of_meters,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

meter_mod_failed_out_of_bands() ->
    Msg = #ofp_error{ type = meter_mod_failed,
                      code = out_of_bands,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_bad_table() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = bad_table,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_bad_metadata() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = bad_metadata,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_eperm() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_bad_capa() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = bad_capa,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_bad_max_ent() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = bad_max_ent,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_bad_features() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = bad_features,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_bad_command() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = bad_command,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

table_features_failed_too_many() ->
    Msg = #ofp_error{ type = table_features_failed,
                      code = too_many,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_bad_type() ->
    Msg = #ofp_error{ type = bad_property,
                      code = bad_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_bad_len() ->
    Msg = #ofp_error{ type = bad_property,
                      code = bad_len,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_bad_value() ->
    Msg = #ofp_error{ type = bad_property,
                      code = bad_value,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_too_many() ->
    Msg = #ofp_error{ type = bad_property,
                      code = too_many,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_dup_type() ->
    Msg = #ofp_error{ type = bad_property,
                      code = dup_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_bad_experimenter() ->
    Msg = #ofp_error{ type = bad_property,
                      code = bad_experimenter,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_bad_exp_type() ->
    Msg = #ofp_error{ type = bad_property,
                      code = bad_exp_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_bad_exp_value() ->
    Msg = #ofp_error{ type = bad_property,
                      code = bad_exp_value,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bad_property_eperm() ->
    Msg = #ofp_error{ type = bad_property,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

async_config_failed_invalid() ->
    Msg = #ofp_error{ type = async_config_failed,
                      code = invalid,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

async_config_failed_unsupported() ->
    Msg = #ofp_error{ type = async_config_failed,
                      code = unsupported,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

async_config_failed_eperm() ->
    Msg = #ofp_error{ type = async_config_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_unknown() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = unknown,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_monitor_exists() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = monitor_exists,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_invalid_monitor() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = invalid_monitor,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_unknown_monitor() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = unknown_monitor,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_bad_command() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = bad_command,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_bad_flags() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = bad_flags,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_bad_table_id() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = bad_table_id,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

flow_monitor_failed_bad_out() ->
    Msg = #ofp_error{ type = flow_monitor_failed,
                      code = bad_out,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_unknown() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = unknown,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_eperm() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = eperm,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_bad_id() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = bad_id,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_bundle_exists() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = bundle_exists,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_bundle_closed() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = bundle_closed,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_out_of_bundles() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = out_of_bundles,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_bad_type() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = bad_type,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_bad_flags() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = bad_flags,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_msg_bad_len() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = msg_bad_len,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_msg_bad_xid() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = msg_bad_xid,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_msg_unsup() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = msg_unsup,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_msg_conflict() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = msg_conflict,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_msg_too_many() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = msg_too_many,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_msg_failed() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = msg_failed,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_timeout() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = timeout,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_bundle_in_progress() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = bundle_in_progress,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_sched_not_supported() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = sched_not_supported,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_sched_future() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = sched_future,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.

bundle_failed_sched_past() ->
    Msg = #ofp_error{ type = bundle_failed,
                      code = sched_past,
                      data = <<"test error">> },
    Bin = ?ENCODE(Msg),
    EMsg = ?DECODE(Bin),
    Msg = EMsg.
