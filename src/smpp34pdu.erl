%%%-------------------------------------------------------------------
%%% @author Praveen Paulose
%%% @copyright (C) 2017, Celusion Technologies Pvt. Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 30. Oct 2017 12:35
%%%-------------------------------------------------------------------
-module(smpp34pdu).
-author("Praveen Paulose").
-include("../include/smpp34pdu.hrl").
-include("types.hrl").

%% API
-export([pack/3, unpack/1,start/0,stop/0]).

-type(unpack_status() :: 'header_length' | 'body_length' | 'ok').

-spec pack(integer(), integer(), valid_pdu()) -> binary().
-spec pack(integer(), integer(), integer(), binary()) -> binary().

-spec unpack(binary()) -> {unpack_status(), [pdu()],binary()}.
-spec unpack(binary(), unpack_status(), [pdu()]) -> {unpack_status(), [pdu()], binary()}.

-spec unpack_body(integer(), binary()) -> valid_pdu() | invalid_command_id().


start() ->
  application:start(smpp34pdu).

stop() ->
  application:stop(smpp34pdu).

pack(CmdStat, Snum, #generic_nack{}) ->
  pack(?GENERIC_NACK, CmdStat, Snum, <<>>);

pack(CmdStat, Snum, #bind_transmitter{}=Body) ->
  Bin = smpp34pdu_bind_transmitter:pack(Body),
  pack(?BIND_TRANSMITTER, CmdStat, Snum, Bin);

pack(CmdStat, Snum, #bind_transmitter_resp{}=Body) ->
  Bin = smpp34pdu_bind_transmitter_resp:pack(Body),
  pack(?BIND_TRANSMITTER_RESP, CmdStat, Snum, Bin);

pack(CmdStat, Snum, #bind_receiver{} = Body) ->
  Bin = smpp34pdu_bind_receiver:pack(Body),
  pack(?BIND_RECEIVER, CmdStat, Snum, Bin);

pack(CmdStat, Snum, #bind_receiver_resp{}=Body) ->
  Bin = smpp34pdu_bind_receiver_resp:pack(Body),
  pack(?BIND_RECEIVER_RESP, CmdStat, Snum, Bin);

pack(CmdStat, Snum, #bind_transceiver{}=Body) ->
  Bin = smpp34pdu_bind_transceiver:pack(Body),
  pack(?BIND_TRANSCEIVER, CmdStat, Snum, Bin);

pack(CmdStat, Snum, #bind_transceiver_resp{}=Body) ->
  Bin = smpp34pdu_bind_transceiver_resp:pack(Body),
  pack(?BIND_TRANSCEIVER_RESP, CmdStat, Snum, Bin);

pack(CmdStat, Snum, #unbind{}) ->
  pack(?UNBIND, CmdStat, Snum, <<>>);

pack(CmdStat, Snum, #unbind_resp{}) ->
  pack(?UNBIND_RESP, CmdStat, Snum, <<>>);

pack(CmdStat, Snum, #enquire_link{}) ->
  pack(?ENQUIRE_LINK, CmdStat, Snum, <<>>);

pack(CmdStat, Snum, #enquire_link_resp{}) ->
  pack(?ENQUIRE_LINK_RESP, CmdStat, Snum, <<>>);

pack(CmdStat, Snum, #submit_sm{}=Body)  ->
  Bin = smpp34pdu_submit_sm:pack(Body),
  pack(?SUBMIT_SM, CmdStat, Snum, Bin);

pack(CmdStat, Snum, #submit_sm_resp{}=Body) ->
  Bin = smpp34pdu_submit_sm_resp:pack(Body),
  pack(?SUBMIT_SM_RESP, CmdStat, Snum, Bin).


pack(Cid, CmdStat, Snum, Body) ->
  Clen = byte_size(Body) + ?HEADER_OCTET_SIZE,
  L = [<<Clen:32,Cid:32,CmdStat:32,Snum:32>>, Body],
  list_to_binary(L).



unpack(Bin) ->
  unpack(Bin, ok, []).


unpack(<<>>, _, Accm) ->
  {ok, lists:reverse(Accm), <<>>};
unpack(Bin, header_length, Accm) ->
  {header_length, lists:reverse(Accm), Bin};
unpack(Bin, body_length, Accm) ->
  {body_length, lists:reverse(Accm), Bin};
unpack(Bin0, ok, Accm) ->
  case byte_size(Bin0) of
    N when N < 16 ->
      unpack(Bin0, header_length, Accm);
    _ ->
      {CommandLength, Bin1} = tools:bin_to_integer(Bin0, 4),
      {CommandId, Bin2} = tools:bin_to_integer(Bin1, 4),
      {CommandStatus, Bin3} = tools:bin_to_integer(Bin2, 4),
      {SequenceNumber, Bin4} = tools:bin_to_integer(Bin3, 4),

      BodyLength = CommandLength - ?HEADER_OCTET_SIZE,

      case byte_size(Bin4) of
        N when N < BodyLength ->
          unpack(Bin0, body_length, Accm);
        _ ->
          <<BodyBin:BodyLength/binary, Rest/binary>> = Bin4,
          Body = unpack_body(CommandId, BodyBin),
          Pdu = #pdu{command_length=CommandLength,
            command_id=CommandId,
            command_status=CommandStatus,
            sequence_number=SequenceNumber,
            body=Body},
          unpack(Rest, ok, [Pdu|Accm])
      end
  end.

unpack_body(?GENERIC_NACK, _) ->
  #generic_nack{};
unpack_body(?BIND_TRANSMITTER, Bin) ->
  smpp34pdu_bind_transmitter:unpack(Bin);

unpack_body(?BIND_TRANSMITTER_RESP, Bin) ->
  smpp34pdu_bind_transmitter_resp:unpack(Bin);

unpack_body(?BIND_RECEIVER, Bin) ->
  smpp34pdu_bind_receiver:unpack(Bin);

unpack_body(?BIND_RECEIVER_RESP, Bin) ->
  smpp34pdu_bind_receiver_resp:unpack(Bin);

unpack_body(?BIND_TRANSCEIVER, Bin) ->
  smpp34pdu_bind_transceiver:unpack(Bin);

unpack_body(?BIND_TRANSCEIVER_RESP, Bin) ->
  smpp34pdu_bind_transceiver_resp:unpack(Bin);

unpack_body(?SUBMIT_SM, Bin) ->
  smpp34pdu_submit_sm:unpack(Bin);

unpack_body(?SUBMIT_SM_RESP, Bin) ->
  smpp34pdu_submit_sm_resp:unpack(Bin);

unpack_body(?UNBIND, _) ->
  #unbind{};
unpack_body(?UNBIND_RESP, _) ->
  #unbind_resp{};

unpack_body(?ENQUIRE_LINK, _) ->
  #enquire_link{};
unpack_body(?ENQUIRE_LINK_RESP, _) ->
  #enquire_link_resp{};

unpack_body(CommandId, _) ->
  {error, {command_id, CommandId}}.
