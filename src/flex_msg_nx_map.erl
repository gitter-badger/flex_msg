-module(flex_msg_nx_map).

-export([nxm_field_bit_length/1]).

nxm_field_bit_length(in_port) -> 16;
nxm_field_bit_length(eth_dst) -> 48;
nxm_field_bit_length(eth_src) -> 48;
nxm_field_bit_length(eth_type) -> 16;
nxm_field_bit_length(vlan_tci) -> 16;
nxm_field_bit_length(ip_tos) -> 8;
nxm_field_bit_length(ip_proto) -> 8;
nxm_field_bit_length(ip_src) -> 32;
nxm_field_bit_length(ip_dst) -> 32;
nxm_field_bit_length(tcp_src) -> 16;
nxm_field_bit_length(tcp_dst) -> 16;
nxm_field_bit_length(udp_src) -> 16;
nxm_field_bit_length(udp_dst) -> 16;
nxm_field_bit_length(icmp_type) -> 8;
nxm_field_bit_length(icmp_code) -> 8;
nxm_field_bit_length(arp_op) -> 16;
nxm_field_bit_length(arp_spa) -> 32;
nxm_field_bit_length(arp_tpa) -> 32;
nxm_field_bit_length(nx_reg0) -> 32;
nxm_field_bit_length(nx_reg1) -> 32;
nxm_field_bit_length(nx_reg2) -> 32;
nxm_field_bit_length(nx_reg3) -> 32;
nxm_field_bit_length(nx_reg4) -> 32;
nxm_field_bit_length(nx_reg5) -> 32;
nxm_field_bit_length(nx_reg6) -> 32;
nxm_field_bit_length(nx_reg7) -> 32;
nxm_field_bit_length(nx_tun_id) -> 64;
nxm_field_bit_length(nx_arp_sha) -> 48;
nxm_field_bit_length(nx_arp_tha) -> 48;
nxm_field_bit_length(nx_ipv6_src) -> 128;
nxm_field_bit_length(nx_ipv6_dst) -> 128;
nxm_field_bit_length(nx_icmpv6_type) -> 8;
nxm_field_bit_length(nx_icmpv6_code) -> 8;
nxm_field_bit_length(nx_nd_target) -> 128;
nxm_field_bit_length(nx_nd_sll) -> 48;
nxm_field_bit_length(nx_nd_tll) -> 48;
nxm_field_bit_length(nx_ip_frag) -> 8;
nxm_field_bit_length(nx_ipv6_label) -> 32;
nxm_field_bit_length(nx_ip_ecn) -> 8;
nxm_field_bit_length(nx_ip_ttl) -> 8;
nxm_field_bit_length(nx_cookie) -> 64;
nxm_field_bit_length(nx_tun_ipv4_src) -> 32;
nxm_field_bit_length(nx_tun_ipv4_dst) -> 32;
nxm_field_bit_length(nx_pkt_mark) -> 32;
nxm_field_bit_length(nx_tcp_flags) -> 16;
nxm_field_bit_length(nx_dp_hash) -> 32;
nxm_field_bit_length(nx_recirc_id) -> 32.
