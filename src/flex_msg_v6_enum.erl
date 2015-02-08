-module(flex_msg_v6_enum).

%%------------------------------------------------------------------------------
%% Error Messages
%%------------------------------------------------------------------------------

-enum({ error_type, [{ hello_failed, 0 },
                     { bad_request, 1 },
                     { bad_action, 2 },
                     { bad_instruction, 3 },
                     { bad_match, 4 },
                     { flow_mod_failed, 5 },
                     { group_mod_failed, 6 },
                     { port_mod_failed, 7 },
                     { table_mod_failed, 8 },
                     { queue_op_failed, 9 },
                     { switch_config_failed, 10 },
                     { role_request_failed, 11 },
                     { meter_mod_failed, 12 },
                     { table_features_failed, 13 },
                     { bad_property, 14 },
                     { async_config_failed, 15 },
                     { flow_monitor_failed, 16 },
                     { bundle_failed, 17 },
                     { experimenter, 16#ffff }] }).

-enum({ hello_failed, [{ incompatible, 0 },
                       { eperm, 1 }] }).

-enum({ bad_request, [{ bad_version, 0 },
                      { bad_type, 1 },
                      { bad_multipart, 2 },
                      { bad_experimenter, 3 },
                      { bad_exp_type, 4 },
                      { buffer_empty, 5 },
                      { buffer_unknown, 6 },
                      { bad_table_id, 7 },
                      { is_slave, 8 },
                      { bad_port, 9 },
                      { bad_packet, 10 },
                      { multipart_buffer_overflow, 11 },
                      { multipart_request_timeout, 12 },
                      { multipart_reply_timeout, 13 },
                      { multipart_bad_sched, 14 },
                      { pipeline_fields_only, 15 },
                      { unknown, 16 }]}).

-enum({ bad_action, [{ bad_type, 0 },
                     { bad_len, 1 },
                     { bad_experimenter, 3 },
                     { bad_exp_type, 4 },
                     { bad_out_port, 5 },
                     { bad_argument, 6 },
                     { eperm, 7 },
                     { too_many, 8 },
                     { bad_queue, 9 },
                     { bad_out_group, 10 },
                     { match_inconsistent, 11 },
                     { unsupported_order, 12 },
                     { bad_tag, 13 },
                     { bad_set_type, 14 },
                     { bad_set_len, 15 },
                     { bad_set_argument, 16 },
                     { bad_set_mask, 17 }] }).

-enum({ bad_instruction, [{ unknown_inst, 0 },
                          { unsup_inst, 1 },
                          { bad_table_id, 2 },
                          { unsup_metadata, 3 },
                          { unsup_metadata_mask, 4 },
                          { bad_experimenter, 5 },
                          { bad_exp_type, 6 },
                          { bad_len, 7 },
                          { eperm, 8 },
                          { dup_inst, 9 }] }).

-enum({ bad_match, [{ bad_type, 0 },
                    { bad_len, 1 },
                    { bad_tag, 2 },
                    { bad_dl_addr_mask, 3 },
                    { bad_nw_addr_mask, 4 },
                    { bad_wildcards, 5 },
                    { bad_field, 6 },
                    { bad_value, 7 },
                    { bad_mask, 8 },
                    { bad_prereq, 9 },
                    { dup_field, 10 },
                    { eperm, 11 }] }).

-enum({ flow_mod_failed, [{ unknown, 0 },
                          { table_full, 1 },
                          { bad_table_id, 2 },
                          { overlap, 3 },
                          { eperm, 4 },
                          { bad_timeout, 5 },
                          { bad_command, 6 },
                          { bad_flags, 7 },
                          { cant_sync, 8 },
                          { bad_priority, 9 },
                          { is_sync, 10 }] }).

-enum({ group_mod_failed, [{ group_exists, 0 },
                           { invaild_group, 1 },
                           { weight_unsupported, 2 },
                           { out_of_groups, 3 },
                           { out_of_buckets, 4 },
                           { chaining_unsupported, 5 },
                           { watch_unsupported, 6 },
                           { loop, 7 },
                           { unknown_group, 8 },
                           { chained_group, 9 },
                           { bad_type, 10 },
                           { bad_command, 11 },
                           { bad_bucket, 12 },
                           { bad_watch, 13 },
                           { eperm, 14 },
                           { unknown_bucket, 15 },
                           { bucket_exists, 16 }] }).

-enum({ port_mod_failed, [{ bad_port, 0 },
                          { bad_hw_addr, 1 },
                          { bad_config, 2 },
                          { bad_adverise, 3 },
                          { eperm, 4 }] }).

-enum({ table_mod_failed, [{ bad_table, 0 },
                           { bad_config, 1 },
                           { eperm, 2 }] }).

-enum({ queue_op_failed, [{ bad_port, 0 },
                          { bad_queue, 1 },
                          { eperm, 2 }] }).

-enum({ switch_config_failed, [{ bad_flags, 0 },
                               { bad_len, 1 },
                               { eperm, 2 }] }).

-enum({ role_request_failed, [{ stale, 0 },
                              { unsup, 1 },
                              { bad_role, 2 },
                              { id_unsup, 3 },
                              { id_in_use, 4 }] }).

-enum({ meter_mod_failed, [{ unknown, 0 },
                           { meter_exists, 1 },
                           { invaild_meter, 2 },
                           { unknown_meter, 3 },
                           { bad_command, 4 },
                           { bad_flags, 5 },
                           { bad_rate, 6 },
                           { bad_burst, 7 },
                           { bad_band, 8 },
                           { bad_band_value, 9 },
                           { out_of_meters, 10 },
                           { out_of_bands, 11 }] }).

-enum({ table_features_failed, [{ bad_table, 0 },
                                { bad_metadata, 1 },
                                { eperm, 5 },
                                { bad_capa, 6 },
                                { bad_max_ent, 7 },
                                { bad_features, 8 },
                                { bad_command, 9 },
                                { too_many, 10 }] }).

-enum({ bad_property, [{ bad_type, 0 },
                       { bad_len, 1 },
                       { bad_value, 2 },
                       { too_many, 3 },
                       { dup_type, 4 },
                       { bad_experimenter, 5 },
                       { bad_exp_type, 6 },
                       { bad_exp_value, 7 },
                       { eperm, 8 }] }).

-enum({ async_config_failed, [{ invalid, 0 },
                              { unsupported, 1 },
                              { eperm, 2 }] }).

-enum({ flow_monitor_failed, [{ unknown, 0 },
                              { monitor_exists, 1 },
                              { invalid_monitor, 2 },
                              { unknown_monitor, 3 },
                              { bad_command, 4 },
                              { bad_flags, 5 },
                              { bad_table_id, 6 },
                              { bad_out, 7 }] }).

-enum({ bundle_failed, [{ unknown, 0 },
                        { eperm, 1 },
                        { bad_id, 2 },
                        { bundle_exists, 3 },
                        { bundle_closed, 4 },
                        { out_of_bundles, 5 },
                        { bad_type, 6 },
                        { bad_flags, 7 },
                        { msg_bad_len, 8 },
                        { msg_bad_xid, 9 },
                        { msg_unsup, 10 },
                        { msg_conflict, 11 },
                        { msg_too_many, 12 },
                        { msg_failed, 13 },
                        { timeout, 14 },
                        { bundle_in_progress, 15 },
                        { sched_not_supported, 16 },
                        { sched_future, 17 },
                        { sched_past, 18 }] }).
