-module(smpp34_pdu_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


pack_test_() ->
	[
		{"Packing #bind_receiver{} PDU",
			?_assertEqual(<<0,0,0,37,0,0,0,1,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,97,98,99,100,0,0,52,2,1,0>>,
					   smpp34_pdu:pack(1, #bind_receiver{system_id="abcdefghij", 
						   password="abcd", system_type="", 
						   interface_version=?VERSION, addr_ton=2, 
						   addr_npi=1,address_range=""}))},
		{"Packing #bind_transmitter{} PDU",
			?_assertEqual(<<0,0,0,37,0,0,0,2,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,97,98,99,100,0,0,52,2,1,0>>,
					   smpp34_pdu:pack(1, #bind_transmitter{system_id="abcdefghij", 
						   password="abcd", system_type="", 
						   interface_version=?VERSION, addr_ton=2, 
						   addr_npi=1,address_range=""}))}
	].

unpack_test_() ->
	[
		{"Unpacking #bind_receiver{} PDU",
			?_assertEqual({ok, [#pdu{command_length=37, 
					command_id=?BIND_RECEIVER, command_status=0, 
					sequence_number=1, body=#bind_receiver{system_id="abcdefghij", 
								password="abcd", system_type="", interface_version=?VERSION, 
								addr_ton=2, addr_npi=1,address_range=""}}], <<>>}, 
								smpp34_pdu:unpack(<<0,0,0,37,0,0,0,1,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,97,98,99,100,0,0,52,2,
													1,0>>))},
		{"Unpacking #bind_transmitter{} PDU",
			?_assertEqual({ok, [#pdu{command_length=37, 
					command_id=?BIND_TRANSMITTER, command_status=0, 
					sequence_number=1, body=#bind_transmitter{system_id="abcdefghij", 
								password="abcd", system_type="", interface_version=?VERSION, 
								addr_ton=2, addr_npi=1,address_range=""}}], <<>>}, 
								smpp34_pdu:unpack(<<0,0,0,37,0,0,0,2,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,97,98,99,100,0,0,52,2,
													1,0>>))}
	].

unpack_multiple_test_() ->
	[
		{"Unpack same type of PDUs WITH NO remainder",
			?_assertEqual({ok, [#pdu{command_length=37, command_id=?BIND_RECEIVER,
						command_status=0, sequence_number=1,
						body=#bind_receiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}},
					    #pdu{command_length=37, command_id=?BIND_RECEIVER,
						command_status=0, sequence_number=2,
						body=#bind_receiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}},
					    #pdu{command_length=37, command_id=?BIND_RECEIVER,
						command_status=0, sequence_number=3,
						body=#bind_receiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}}], <<>>},
						smpp34_pdu:unpack(<<0,0,0,37,0,0,0,1,0,0,0,0,0,
								  0,0,1,97,98,99,100,101,102,
								  103,104,105,106,0,97,98,99,
								  100,0,0,52,2,1,0,0,0,0,37,0,
								  0,0,1,0,0,0,0,0,0,0,2,97,98,
								  99,100,101,102,103,104,105,
								  106,0,97,98,99,100,0,0,52,2,
								  1,0,0,0,0,37,0,0,0,1,0,0,0,
								  0,0,0,0,3,97,98,99,100,101,
								  102,103,104,105,106,0,97,98,
								  99,100,0,0,52,2,1,0>>))},
		{"Unpack same type of PDUs WITH remainder",
			?_assertEqual({header_length, [#pdu{command_length=37, command_id=?BIND_RECEIVER,
						command_status=0, sequence_number=1,
						body=#bind_receiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}},
					    #pdu{command_length=37, command_id=?BIND_RECEIVER,
						command_status=0, sequence_number=2,
						body=#bind_receiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}},
					    #pdu{command_length=37, command_id=?BIND_RECEIVER,
						command_status=0, sequence_number=3,
						body=#bind_receiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}}], <<0,0,0,37>>},
						smpp34_pdu:unpack(<<0,0,0,37,0,0,0,1,0,0,0,0,0,
								  0,0,1,97,98,99,100,101,102,
								  103,104,105,106,0,97,98,99,
								  100,0,0,52,2,1,0,0,0,0,37,0,
								  0,0,1,0,0,0,0,0,0,0,2,97,98,
								  99,100,101,102,103,104,105,
								  106,0,97,98,99,100,0,0,52,2,
								  1,0,0,0,0,37,0,0,0,1,0,0,0,
								  0,0,0,0,3,97,98,99,100,101,
								  102,103,104,105,106,0,97,98,
								  99,100,0,0,52,2,1,0,0,0,0,37>>))}
	].
