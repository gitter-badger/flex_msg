-define(MOD(Version), case Version of
                          1 -> flex_msg_v1;
                          _ -> unsupported
                      end).

%% Parser ----------------------------------------------------------------------
-record(ofp_parser, {
          version :: integer(),
          module :: atom(),
          stack = <<>> :: binary() }).
-type ofp_parser() :: #ofp_parser{}.
