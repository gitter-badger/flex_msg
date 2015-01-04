flex_msg [![Build Status](https://travis-ci.org/shun159/flex_msg.svg?branch=master)](https://travis-ci.org/shun159/flex_msg)
========

Erlang OpenFlow Packet Library.

## Features
  Support for [OpenFlow Protocol 1.0][ofp1]

## Examples

### Hello
  ````erlang
  Msg = flex_msg_v1:prepend_of_header(Xid, flex_msg_v1:hello()),
  flex_msg_v1:encode(Msg).
  ````

### OpenFlow Error
  ````erlang
  Body = flex_msg_v1:openflow_error(hello_failed, incompatible, <<>>),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Echo
  ````erlang
  % echo request
  Request = flex_msg_v1:echo_request(<<>>),
  RequestMsg = flex_msg_v1:prepend_of_header(Xid, Request),
  flex_msg_v1:encode(RequestMsg).

  Reply = flex_msg_v1:echo_reply(<<>>),
  ReplyMsg = flex_msg_v1:prepend_of_header(Xid, Reply),
  flex_msg_v1:encode(ReplyMsg).
  ````

### Barrier Request
  ````erlang
  Body = flex_msg_v1:barrier(),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Vendor
  ````erlang
  Body = flex_msg_v1:vendor(1234, <<"nazo">>),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Get Features
  ````erlang
  Body = flex_msg_v1:get_features(),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Get Config
  ````erlang
  Body = flex_msg_v1:get_config(),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Set Config
  ````erlang
  Body = flex_msg_v1:set_config([], 128),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````
### Set Port Up
  ````erlang
  Body = flex_msg_v1:set_port_up(1, <<0,0,0,0,0,1>>),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Set Port Down
  ````erlang
  Body = flex_msg_v1:set_port_down(1, <<0,0,0,0,0,1>>),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Set Port Not PacketIn
  ````erlang
  Body = flex_msg_v1:set_port_no_packet_in(1, <<0,0,0,0,0,1>>),
  Msg = flex_msg_v1:prepend_of_header(Xid, Body),
  flex_msg_v1:encode(Msg).
  ````

### Send Packet
  ````erlang
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
    Body = flex_msg_v1:send_packet(Data, Actions),
    Msg = flex_msg_v1:prepend_of_header(Xid, Body),
    flex_msg_v1:encode(Msg).
  ````
### Flow Mod Add
  ````erlang
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
    Body = flex_msg_v1:add_flow(<<0:64>>, 16#ffff, 0, 0, Matches, Actions),
    Msg = flex_msg_v1:prepend_of_header(Xid, Body),
    flex_msg_v1:encode(Msg).

  ````

[ofp1]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.0.0.pdf
