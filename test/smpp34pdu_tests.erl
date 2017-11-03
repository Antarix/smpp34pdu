%%%-------------------------------------------------------------------
%%% @author Antarix Tandon
%%% @copyright (C) 2017, Celusion Technologies Pvt. Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 03. Nov 2017 1:21 PM
%%%-------------------------------------------------------------------
-module(smpp34pdu_tests).
-author("Antarix Tandon").

-include("../include/smpp34pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


pack_test_() ->
  [
    {"Packing #bind_receiver{} PDU",
      ?_assertEqual(<<0,0,0,37,0,0,0,1,0,0,0,0,0,0,0,1,
        97,98,99,100,101,102,103,104,105,
        106,0,97,98,99,100,0,0,52,2,1,0>>,
        smpp34pdu:pack(?ESME_ROK, 1, #bind_receiver{system_id="abcdefghij",
          password="abcd", system_type="",
          interface_version=?VERSION, addr_ton=2,
          addr_npi=1,address_range=""}))},
    {"Packing #bind_receiver_resp{} PDU",
      ?_assertEqual(<<0,0,0,32,128,0,0,1,0,0,0,0,0,0,0,1,
        97,98,99,100,101,102,103,104,105,
        106,0,2,16,0,1,52>>,
        smpp34pdu:pack(?ESME_ROK, 1, #bind_receiver_resp{system_id="abcdefghij",
          sc_interface_version=?VERSION}))},
    {"Packing #bind_transmitter{} PDU",
      ?_assertEqual(<<0,0,0,37,0,0,0,2,0,0,0,0,0,0,0,1,
        97,98,99,100,101,102,103,104,105,
        106,0,97,98,99,100,0,0,52,2,1,0>>,
        smpp34pdu:pack(?ESME_ROK, 1, #bind_transmitter{system_id="abcdefghij",
          password="abcd", system_type="",
          interface_version=?VERSION, addr_ton=2,
          addr_npi=1,address_range=""}))},
    {"Packing #bind_transmitter_resp{} PDU",
      ?_assertEqual(<<0,0,0,32,128,0,0,2,0,0,0,0,0,0,0,1,
        97,98,99,100,101,102,103,104,105,
        106,0,2,16,0,1,52>>,
        smpp34pdu:pack(?ESME_ROK, 1, #bind_transmitter_resp{system_id="abcdefghij",
          sc_interface_version=?VERSION}))}

  ].

unpack_test_() ->
  [

    {"Unpacking #bind_receiver{} PDU",
      ?_assertEqual({ok, [#pdu{command_length=37,
        command_id=?BIND_RECEIVER, command_status=0,
        sequence_number=1, body=#bind_receiver{system_id="abcdefghij",
          password="abcd", system_type="", interface_version=?VERSION,
          addr_ton=2, addr_npi=1,address_range=""}}], <<>>},
        smpp34pdu:unpack(<<0,0,0,37,0,0,0,1,0,0,0,0,0,0,0,
          1,97,98,99,100,101,102,103,104,
          105,106,0,97,98,99,100,0,0,52,2,
          1,0>>))},
    {"Unpacking #bind_receiver_resp{} PDU",
      ?_assertEqual({ok, [#pdu{command_length=32,
        command_id=?BIND_RECEIVER_RESP, command_status=0,
        sequence_number=1, body=#bind_receiver_resp{system_id="abcdefghij",
          sc_interface_version=?VERSION}}], <<>>},
        smpp34pdu:unpack(<<0,0,0,32,128,0,0,1,0,0,0,0,0,0,0,
          1,97,98,99,100,101,102,103,104,
          105,106,0,2,16,0,1,52>>))},
    {"Unpacking #bind_transmitter{} PDU",
      ?_assertEqual({ok, [#pdu{command_length=37,
        command_id=?BIND_TRANSMITTER, command_status=0,
        sequence_number=1, body=#bind_transmitter{system_id="abcdefghij",
          password="abcd", system_type="", interface_version=?VERSION,
          addr_ton=2, addr_npi=1,address_range=""}}], <<>>},
        smpp34pdu:unpack(<<0,0,0,37,0,0,0,2,0,0,0,0,0,0,0,
          1,97,98,99,100,101,102,103,104,
          105,106,0,97,98,99,100,0,0,52,2,
          1,0>>))},
    {"Unpacking #bind_transmitter_resp{} PDU",
      ?_assertEqual({ok, [#pdu{command_length=32,
        command_id=?BIND_TRANSMITTER_RESP, command_status=0,
        sequence_number=1, body=#bind_transmitter_resp{system_id="abcdefghij",
          sc_interface_version=?VERSION}}], <<>>},
        smpp34pdu:unpack(<<0,0,0,32,128,0,0,2,0,0,0,0,0,0,0,
          1,97,98,99,100,101,102,103,104,
          105,106,0,2,16,0,1,52>>))}

  ].

