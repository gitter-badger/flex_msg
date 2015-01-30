-module(error_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ofp_v1.hrl").

-define(MODNAME, flex_msg_v1).
-define(DECODE(Bin), ?MODNAME:decode(Bin)).
-define(ENCODE(Msg), ?MODNAME:encode(Msg)).

error_test_() ->
    [{ "hello failed incompatible error", fun hello_failed_incompatible/0 },
     { "hello failed eperm error", fun hello_failed_eperm/0 },
     { "bad request bad version error", fun bad_request_bad_version/0 },
     { "bad request bad type error", fun bad_request_bad_type/0 },
     { "bad request bad stat error", fun bad_request_bad_stat/0 },
     { "bad request bad vendor error", fun bad_request_bad_vendor/0 },
     { "bad request bad subtype error", fun bad_request_bad_subtype/0 },
     { "bad request eperm error", fun bad_request_eperm/0 },
     { "bad request bad len error", fun bad_request_bad_len/0 },
     { "bad request buffer empty error", fun bad_request_buffer_empty/0 },
     { "bad request buffer unknown error", fun bad_request_buffer_unknown/0 },
     { "bad action bad type error", fun bad_action_bad_type/0 },
     { "bad action bad len error", fun bad_action_bad_len/0 },
     { "bad action bad vendor error", fun bad_action_bad_vendor/0 },
     { "bad action bad vendor type error", fun bad_action_bad_vendor_type/0 },
     { "bad action bad out port error", fun bad_action_bad_out_port/0 },
     { "bad action bad argument error", fun bad_action_bad_argument/0 },
     { "bad action eperm error", fun bad_action_eperm/0 },
     { "bad action too many error", fun bad_action_too_many/0 },
     { "bad action bad queue error", fun bad_action_bad_queue/0 },
     { "flow mod failed all tables full error", fun flow_mod_failed_all_tables_full/0 },
     { "flow mod failed overlap error", fun flow_mod_failed_overlap/0 },
     { "flow mod failed eperm error", fun flow_mod_failed_eperm/0 },
     { "flow mod failed bad timeout error", fun flow_mod_failed_bad_emerg_timeout/0 },
     { "flow mod failed bad command error", fun flow_mod_failed_bad_command/0 },
     { "flow mod failed unsupported error", fun flow_mod_failed_unsupported/0 },
     { "port mod failed bad port error", fun port_mod_failed_bad_port/0 },
     { "port mod failed bad hw address error", fun port_mod_failed_bad_hw_addr/0 },
     { "queue op faild bad port error", fun queue_op_failed_bad_port/0 },
     { "queue op faild bad queue error", fun queue_op_failed_bad_queue/0 },
     { "queue op faild eperm error", fun queue_op_failed_eperm/0 }].

hello_failed_incompatible() ->
    Error = #ofp_error_msg{ type = hello_failed,
                            code = incompatible },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

hello_failed_eperm() ->
    Error = #ofp_error_msg{ type = hello_failed,
                            code = eperm },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_version() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = bad_version },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_type() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = bad_type },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_stat() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = bad_stat },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_vendor() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = bad_vendor },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_subtype() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = bad_subtype },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_eperm() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = eperm },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_bad_len() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = bad_len },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_buffer_empty() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = buffer_empty },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_request_buffer_unknown() ->
    Error = #ofp_error_msg{ type = bad_request,
                            code = buffer_unknown },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_type() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = bad_type },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_len() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = bad_len },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_vendor() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = bad_vendor },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_vendor_type() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = bad_vendor_type },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_out_port() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = bad_out_port },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_argument() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = bad_argument },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_eperm() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = eperm },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_too_many() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = too_many },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

bad_action_bad_queue() ->
    Error = #ofp_error_msg{ type = bad_action,
                            code = bad_queue },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_all_tables_full() ->
    Error = #ofp_error_msg{ type = flow_mod_failed,
                            code = all_tables_full },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_overlap() ->
    Error = #ofp_error_msg{ type = flow_mod_failed,
                            code = overlap },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_eperm() ->
    Error = #ofp_error_msg{ type = flow_mod_failed,
                            code = eperm },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_bad_emerg_timeout() ->
    Error = #ofp_error_msg{ type = flow_mod_failed,
                            code = bad_emerg_timeout },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_bad_command() ->
    Error = #ofp_error_msg{ type = flow_mod_failed,
                            code = bad_command },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

flow_mod_failed_unsupported() ->
    Error = #ofp_error_msg{ type = flow_mod_failed,
                            code = unsupported },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_mod_failed_bad_port() ->
    Error = #ofp_error_msg{ type = port_mod_failed,
                            code = bad_port },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

port_mod_failed_bad_hw_addr() ->
    Error = #ofp_error_msg{ type = port_mod_failed,
                            code = bad_hw_addr },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

queue_op_failed_bad_port() ->
    Error = #ofp_error_msg{ type = queue_op_failed,
                            code = bad_port },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

queue_op_failed_bad_queue() ->
    Error = #ofp_error_msg{ type = queue_op_failed,
                            code = bad_queue },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.

queue_op_failed_eperm() ->
    Error = #ofp_error_msg{ type = queue_op_failed,
                            code = eperm },
    Msg = #ofp_header{ type = error, body = Error },
    Bin = ?ENCODE(Msg),
    {ok, EMsg, _ } = ?DECODE(Bin),
    Msg = EMsg.
