%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 四月 2017 13:57
%%%-------------------------------------------------------------------
-module(protocol).
-author("erow").
-include("protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([encode_packet/1, decode_packet/1, timestamp/0]).

encode_packet(#data_packet{
  sequence_number = Seq,
  packet_flag = FF,
  message_number = Message,
  timestamp = Timestamp,
  destination_socket_id = SocketId,
  payload = Payload
}) ->
  <<0:1, Seq:31,
    FF:2, 0:1, Message:29,
    Timestamp:32,
    SocketId:32,
    Payload/binary>>;
encode_packet(#handshake_packet{
  timestamp = Timestamp,
  destination_socket_id = DestSocketId,
  udt_version = Version,
  socket_type = SocketType,
  initial_sequence_number = InitalSeq,
  packet_size = PacketSize,
  flow_window_size = FlowWindowSize,
  connection_type = CnntType,
  socket_id = SocketId,
  syn_cookie = Cookie,
  peer = Peer
}) ->
  <<1:1, 0:15, 0:16,
    0:32,
    Timestamp:32,
    DestSocketId:32,
    Version:32,
    SocketType:32,
    InitalSeq:32,
    PacketSize:32,
    FlowWindowSize:32,
    CnntType:32,
    SocketId:32,
    Cookie:32,
    Peer:128>>;
encode_packet(#keep_alive{
  timestamp = Timestamp,
  destination_socket_id = DestSocketId
}) ->
  <<1:1, 1:15, 0:16,
    0:32,
    Timestamp:32,
    DestSocketId:32>>;
encode_packet(#ack_packet{
  timestamp = Timestamp,
  destination_socket_id = DestSocketId,
  previous_packets_received = Received
}) ->
  <<1:1, 2:15, 0:16,
    0:32,
    Timestamp:32,
    DestSocketId:32,
    Received:32>>;
encode_packet(#nak{
  timestamp = Timestamp,
  destination_socket_id = DestSocketId,
  loss_list = LossList
}) ->
  LossInfo = compress_list(LossList),
  <<1:1, 3:15, 0:16,
    0:32,
    Timestamp:32,
    DestSocketId:32,
    LossInfo/binary>>;
encode_packet(#info_packet{
  timestamp = Timestamp,
  destination_socket_id = DestSocketId,
  rtt = Rtt,
  rtt_variance = RttVariance,
  available_buffer_size = BufferSize,
  receiving_rate = RecvRate,
  capacity = Capacity
}) ->
  <<1:1, 4:15, 0:16,
    0:32,
    Timestamp:32,
    DestSocketId:32,
    Rtt:32,
    RttVariance:32,
    BufferSize:32,
    RecvRate:32,
    Capacity:32>>;
encode_packet(#shutdown_packet{
  timestamp = Timestamp,
  destination_socket_id = DestSocketId
}) ->
  <<1:1, 5:15, 0:16,
    0:32,
    Timestamp:32,
    DestSocketId:32>>;
encode_packet(#ack2_packet{
  timestamp = Timestamp,
  destination_socket_id = DestSocketId,
  ack_sequence = AckSeq
}) ->
  <<1:1, 6:15, 0:16,
    AckSeq:32,
    Timestamp:32,
    DestSocketId:32>>.


decode_packet(<<0:1, Seq:31,
  FF:2, 0:1, Message:29,
  Timestamp:32,
  SocketId:32,
  Payload/binary>>) ->
  #data_packet{
    sequence_number = Seq,
    packet_flag = FF,
    message_number = Message,
    timestamp = Timestamp,
    destination_socket_id = SocketId,
    payload = Payload
  };
decode_packet(<<1:1, 0:15, 0:16,
  0:32,
  Timestamp:32,
  DestSocketId:32,
  Version:32,
  SocketType:32,
  InitalSeq:32,
  PacketSize:32,
  FlowWindowSize:32,
  CnntType:32,
  SocketId:32,
  Cookie:32,
  Peer:128>>) ->
  #handshake_packet{
    timestamp = Timestamp,
    destination_socket_id = DestSocketId,
    udt_version = Version,
    socket_type = SocketType,
    initial_sequence_number = InitalSeq,
    packet_size = PacketSize,
    flow_window_size = FlowWindowSize,
    connection_type = CnntType,
    socket_id = SocketId,
    syn_cookie = Cookie,
    peer = Peer
  };
decode_packet(<<1:1, 1:15, 0:16,
  0:32,
  Timestamp:32,
  DestSocketId:32>>) ->
  #keep_alive{
    timestamp = Timestamp,
    destination_socket_id = DestSocketId
  };
decode_packet(<<1:1, 2:15, 0:16,
  0:32,
  Timestamp:32,
  DestSocketId:32,
  Received:32>>) ->
  #ack_packet{
    timestamp = Timestamp,
    destination_socket_id = DestSocketId,
    previous_packets_received = Received
  };
decode_packet(<<1:1, 3:15, 0:16,
  0:32,
  Timestamp:32,
  DestSocketId:32,
  LossInfo/binary>>) ->
  LossList = uncompress_binary(LossInfo),
  #nak{
    timestamp = Timestamp,
    destination_socket_id = DestSocketId,
    loss_list = LossList
  };
decode_packet(<<1:1, 4:15, 0:16,
  0:32,
  Timestamp:32,
  DestSocketId:32,
  Rtt:32,
  RttVariance:32,
  BufferSize:32,
  RecvRate:32,
  Capacity:32>>) ->
  #info_packet{
    timestamp = Timestamp,
    destination_socket_id = DestSocketId,
    rtt = Rtt,
    rtt_variance = RttVariance,
    available_buffer_size = BufferSize,
    receiving_rate = RecvRate,
    capacity = Capacity
  };
decode_packet(<<1:1, 5:15, 0:16,
  0:32,
  Timestamp:32,
  DestSocketId:32>>) ->
  #shutdown_packet{
    timestamp = Timestamp,
    destination_socket_id = DestSocketId
  };
decode_packet(<<1:1, 6:15, 0:16,
  AckSeq:32,
  Timestamp:32,
  DestSocketId:32>>) ->
  #ack2_packet{
    timestamp = Timestamp,
    destination_socket_id = DestSocketId,
    ack_sequence = AckSeq
  }.

timestamp() ->
  erlang:system_time(milli_seconds).

compress_list(LossList) ->
  lists:map(fun(Seq) -> <<Seq:32>> end, LossList).

uncompress_binary(<<>>) ->
  [];
uncompress_binary(<<Seq:32, Rest/binary>>) ->
  [Seq | uncompress_binary(Rest)].


compress_test() ->
  LossList = lists:seq(1, 10),
  Binary = compress_list(LossList),
  LossList = uncompress_binary(list_to_binary(Binary)).