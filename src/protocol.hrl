%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 四月 2017 15:38
%%%-------------------------------------------------------------------
-author("erow").


-record(data_packet, {
  timestamp,
  destination_socket_id = 0,
  sequence_number,
  packet_flag = 0,
  message_number = 0,
  payload
}).

-record(handshake_packet, {
  timestamp,
  destination_socket_id = 0,
  udt_version = 0,
  socket_type = 0,
  initial_sequence_number = 1,
  packet_size = 1024,
  flow_window_size = 1000,
  connection_type = 1,
  socket_id = 0,
  syn_cookie = 0,
  peer = 0
}).

-record(keep_alive, {
  timestamp,
  destination_socket_id = 0
}).

-record(ack_packet, {
  timestamp,
  destination_socket_id = 0,
  previous_packets_received
}).

-record(info_packet, {
  timestamp,
  destination_socket_id = 0,
  rtt,
  rtt_variance,
  available_buffer_size,
  receiving_rate,
  capacity
}).

-record(nak, {
  timestamp,
  destination_socket_id = 0,
  loss_list = []
}).

-record(ack2_packet, {
  timestamp,
  destination_socket_id = 0,
  ack_sequence
}).

-record(shutdown_packet, {
  timestamp,
  destination_socket_id = 0
}).