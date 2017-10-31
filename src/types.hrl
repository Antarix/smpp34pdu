%%%-------------------------------------------------------------------
%%% @author Praveen Paulose
%%% @copyright (C) 2017, Celusion Technologies Pvt. Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 30. Oct 2017 12:30
%%%-------------------------------------------------------------------
-author("Praveen Paulose").

-ifndef(types).
-define(types, true).

-include("smpp34pdu.hrl").

-type(generic_nack() :: #empty_body{}).
-type(bind_receiver() :: #bind{}).
-type(bind_receiver_resp() :: #bind_resp{}).
-type(bind_transmitter() :: #bind{}).
-type(bind_transmitter_resp() :: #bind_resp{}).
-type(submit_sm() :: #submit_sm{}).
-type(submit_sm_resp() :: #submit_sm_resp{}).
-type(deliver_sm() :: #deliver_sm{}).
-type(deliver_sm_resp() :: #deliver_sm_resp{}).
-type(unbind() :: #empty_body{}).
-type(unbind_resp() :: #empty_body{}).
-type(bind_transceiver() :: #bind{}).
-type(bind_transceiver_resp() :: #bind_resp{}).
-type(outbind() :: #outbind{}).

-type(invalid_command_id() :: {'error', {'command_id', integer()}}).

-type(valid_pdu() ::  generic_nack()
| bind_receiver()
| bind_receiver_resp()
| bind_transmitter()
| bind_transmitter_resp()
| submit_sm()
| submit_sm_resp()
| deliver_sm()
| deliver_sm_resp()
| unbind()
| unbind_resp()
| bind_transceiver()
| bind_transceiver_resp()
| outbind()).

-type(valid_pdu_error() :: invalid_command_id()).

-type(pdu() :: #pdu{command_length :: integer(),
command_id :: integer(),
command_status :: integer(),
sequence_number :: integer(),
body :: valid_pdu() | valid_pdu_error()}).

-endif.