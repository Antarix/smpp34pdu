-module(tlv_tests).
-include("../src/constants.hrl").
-include_lib("eunit/include/eunit.hrl").



tlv_test_() ->
	[
		{"pack helpers", 
			[
				{"pack_noval", 
					?_assertEqual(<<16,12,0,0>>, tlv:pack_noval(16#100C))}, 
				{"pack_int with Len == 0", 
					?_assertEqual(<<16,12,0,0>>, tlv:pack_int(16#100C, 5, 0))}, 
				{"pack_int with Len == 1", 
					?_assertEqual(<<16,12,0,1,5>>, tlv:pack_int(16#100C, 5, 1))}, 
				{"pack_int with Len > 1", 
					?_assertEqual(<<16,12,0,3,0,0,5>>, tlv:pack_int(16#100C, 5, 3))},
				{"pack_cstring with Len < Max", 
					?_assertEqual(<<16,12,0,4,97,98,99,0>>, tlv:pack_cstring(16#100C, "abc", 10))},
				{"pack_cstring with Len == Max-1", 
					?_assertEqual(<<16,12,0,4,97,98,99,0>>, tlv:pack_cstring(16#100C, "abc", 4))},
				{"pack_cstring with Len == Max", 
					?_assertEqual(<<16,12,0,3,97,98,0>>, tlv:pack_cstring(16#100C, "abc", 3))},
				{"pack_cstring with Len > Max", 
					?_assertEqual(<<16,12,0,3,97,98,0>>, tlv:pack_cstring(16#100C, "abcde", 3))},
				{"pack_octstring_fixedlen with Len < Min", 
					?_assertEqual({error, {less_than_min, 10}}, 
							tlv:pack_octstring_fixedlen(16#100C, <<"abc">>, 10))},
				{"pack_octstring_fixedlen with Len == Min", 
					?_assertEqual(<<16,12,0,3,97,98,99>>, 
							tlv:pack_octstring_fixedlen(16#100C, <<"abc">>, 3))},
				{"pack_octstring_fixedlen with Len > Min", 
					?_assertEqual(<<16,12,0,3,97,98,99>>,
							tlv:pack_octstring_fixedlen(16#100C, <<"abcde">>, 3))},
				{"pack_octstring_varlen with Len < Min", 
					?_assertEqual({error, {less_than_min, 10}},
							tlv:pack_octstring_varlen(16#100C, <<"abc">>, {10, 30}))},
				{"pack_octstring_varlen with Len == Min", 
					?_assertEqual(<<16,12,0,3,97,98,99>>,
							tlv:pack_octstring_varlen(16#100C, <<"abc">>, {3, 5}))},
				{"pack_octstring_varlen with Len > Min and Len < Max", 
					?_assertEqual(<<16,12,0,5,97,98,99,100,101>>, 
							tlv:pack_octstring_varlen(16#100C, <<"abcde">>, {3, 10}))},
				{"pack_octstring_varlen with Len == Max", 
					?_assertEqual(<<16,12,0,5,97,98,99,100,101>>, 
							tlv:pack_octstring_varlen(16#100C, <<"abcde">>, {3, 5}))},
				{"pack_octstring_varlen with Len > Max", 
					?_assertEqual(<<16,12,0,5,97,98,99,100,101>>,
							tlv:pack_octstring_varlen(16#100C, <<"abcdefgh">>, {2, 5}))}
			]
		},

		{"pack", 
			[{"undefined yields empty binary",
				?_assertEqual(<<>>, tlv:pack(?SC_INTERFACE_VERSION, undefined))},
			 {"dest_addr_subunit", 
				?_assertEqual(<<0,5,0,1,0>>, tlv:pack(?DEST_ADDR_SUBUNIT, ?ADDR_SUBUNIT_UNKNOWN))},
			 {"source_addr_subunit", 
				?_assertEqual(<<0,13,0,1,2>>, tlv:pack(?SOURCE_ADDR_SUBUNIT, ?ADDR_SUBUNIT_MOBILE_EQUIPMENT))},
			 {"dest_network_type", 
				?_assertEqual(<<0,6,0,1,1>>, tlv:pack(?DEST_NETWORK_TYPE, ?NETWORK_TYPE_GSM))},
			 {"source_network_type", 
				?_assertEqual(<<0,14,0,1,3>>, tlv:pack(?SOURCE_NETWORK_TYPE, ?NETWORK_TYPE_CDMA))},
			 {"dest_bearer_type", 
				?_assertEqual(<<0,7,0,1,1>>, tlv:pack(?DEST_BEARER_TYPE, ?BEARER_TYPE_SMS))},
			 {"source_bearer_type", 
				?_assertEqual(<<0,15,0,1,4>>, tlv:pack(?SOURCE_BEARER_TYPE, ?BEARER_TYPE_USSD))},
			 {"dest_telematics_id", 
				?_assertEqual(<<0,8,0,2,0,2>>, tlv:pack(?DEST_TELEMATICS_ID, 16#0002))},
			 {"source_telematics_id", 
				?_assertEqual(<<0,16,0,1,1>>, tlv:pack(?SOURCE_TELEMATICS_ID, 16#01))},
			 {"qos_time_to_live", 
				?_assertEqual(<<0,23,0,4,0,0,1,44>>, tlv:pack(?QOS_TIME_TO_LIVE, 300))},
			 {"payload_type", 
				?_assertEqual(<<0,25,0,1,0>>, tlv:pack(?PAYLOAD_TYPE, ?PAYLOAD_TYPE_DEFAULT))},
			 {"additional_status_info_text", 
				?_assertEqual(<<0,29,0,8,102,111,111,32,98,97,114,0>>, tlv:pack(?ADDITIONAL_STATUS_INFO_TEXT, "foo bar"))},
			 {"receipted_message_id", 
				?_assertEqual(<<0,30,0,7,102,111,111,98,97,114,0>>, tlv:pack(?RECEIPTED_MESSAGE_ID, "foobar"))},
			 {"ms_msg_wait_facilities", 
				?_assertEqual(<<0,48,0,1,130>>, tlv:pack(?MS_MSG_WAIT_FACILITIES, ?MSG_WAIT_ACTIVE bor ?MSG_WAIT_TYPE_EMAIL))},
			 {"privacy_indicator", 
				?_assertEqual(<<2,1,0,1,3>>, tlv:pack(?PRIVACY_INDICATOR, ?PRIVACY_INDICATOR_SECRET))},
			 {"addtional_status_info_text", 
				?_assertEqual(<<0,29,0,8,102,111,111,32,98,97,114,0>>, tlv:pack(?ADDITIONAL_STATUS_INFO_TEXT, "foo bar"))},
			 {"receipted_message_id", 
				?_assertEqual(<<0,30,0,7,102,111,111,98,97,114,0>>, tlv:pack(?RECEIPTED_MESSAGE_ID, "foobar"))},
			 {"source_subaddress", 
				?_assertEqual(<<2,2,0,5,1,2,3,4,5>>, tlv:pack(?SOURCE_SUBADDRESS, <<1,2,3,4,5>>))},
			 {"dest_subaddress", 
				?_assertEqual(<<2,3,0,5,1,2,3,4,5>>, tlv:pack(?DEST_SUBADDRESS, <<1,2,3,4,5>>))},
			 {"user_message_reference", 
				?_assertEqual(<<2,4,0,2,1,4>>, tlv:pack(?USER_MESSAGE_REFERENCE, 260))},
			 {"user_response_code", 
				?_assertEqual(<<2,5,0,1,67>>, tlv:pack(?USER_RESPONSE_CODE, 67))},
			 {"language_indicator", 
				?_assertEqual(<<2,13,0,1,2>>, tlv:pack(?LANGUAGE_INDICATOR, ?LANGUAGE_INDICATOR_FRENCH))},
			 {"source_port", 
				?_assertEqual(<<2,10,0,2,1,59>>, tlv:pack(?SOURCE_PORT, 315))},
			 {"destination_port", 
				?_assertEqual(<<2,11,0,2,2,26>>, tlv:pack(?DESTINATION_PORT, 538))},
			 {"sar_msg_ref_num", 
				?_assertEqual(<<2,12,0,2,0,111>>, tlv:pack(?SAR_MSG_REF_NUM, 111))},
			 {"sar_total_segments", 
				?_assertEqual(<<2,14,0,1,5>>, tlv:pack(?SAR_TOTAL_SEGMENTS, 5))},
			 {"sar_segment_seqnum", 
				?_assertEqual(<<2,15,0,1,39>>, tlv:pack(?SAR_SEGMENT_SEQNUM, 39))},
			 {"sc_interface_version", 
				?_assertEqual(<<2,16,0,1,52>>, tlv:pack(?SC_INTERFACE_VERSION, 16#34))},
			 {"display_time", 
				?_assertEqual(<<18,1,0,1,1>>, tlv:pack(?DISPLAY_TIME, ?DISPLAY_TIME_DEFAULT))},
			 {"ms_validity", 
				?_assertEqual(<<18,4,0,1,3>>, tlv:pack(?MS_VALIDITY, ?MS_VALIDITY_DISPLAY))},
			 {"dpf_result", 
				?_assertEqual(<<4,32,0,1,0>>, tlv:pack(?DPF_RESULT, ?DPF_RESULT_NOT_SET))},
			 {"set_dpf", 
				?_assertEqual(<<4,33,0,1,1>>, tlv:pack(?SET_DPF, ?SET_DPF_DEFAULT))},
			 {"ms_availability_status", 
				?_assertEqual(<<4,34,0,1,1>>, tlv:pack(?MS_AVAILABILITY_STATUS, ?MS_AVAILABILITY_DENIED))},
			 {"network_error_code", 
				?_assertEqual(<<4,35,0,3,3,1,1>>, tlv:pack(?NETWORK_ERROR_CODE, <<?NETWORK_ERROR_TYPE_GSM,257:16>>))},
			 {"message_payload", 
				?_assertEqual(<<4,36,0,5,97,98,99,100,101>>, tlv:pack(?MESSAGE_PAYLOAD, <<"abcde">>))},
			 {"delivery_failure_reason", 
				?_assertEqual(<<4,37,0,1,0>>, tlv:pack(?DELIVERY_FAILURE_REASON, ?DEL_FAIL_REASON_DEST_UNAVAIL))},
			 {"more_messages_to_send", 
				?_assertEqual(<<4,38,0,1,1>>, tlv:pack(?MORE_MESSAGES_TO_SEND, ?MORE_MESSAGES_TRUE))},
			 {"message_state", 
				?_assertEqual(<<4,39,0,1,0>>, tlv:pack(?MESSAGE_STATE, ?DPF_RESULT_NOT_SET))},
			 {"callback_num", 
				?_assertEqual(<<3,129,0,14,1,2,8,48,56,48,51,53,54,55,49,50,51,52>>,
						tlv:pack(?CALLBACK_NUM, <<?DMI_ASCII,?TON_NATIONAL,?NPI_NATIONAL,"08035671234">>))},
			 {"callback_num_pres_ind", 
				?_assertEqual(<<3,2,0,1,3>>, 
						tlv:pack(?CALLBACK_NUM_PRES_IND, ?CALLBACK_NUM_PRES_ALLOWED bor ?CALLBACK_NUM_NETWORK_PROVIDED))},
			 {"callback_num_atag", 
				?_assertEqual(<<3,3,0,12,1,48,56,48,51,49,50,51,52,53,54,55>>, 
						tlv:pack(?CALLBACK_NUM_ATAG, <<?DCS_ASCII, "08031234567">>))},
			 {"number_of_messages", 
				?_assertEqual(<<3,4,0,1,48>>, tlv:pack(?NUMBER_OF_MESSAGES, 48))},
			 {"sms_signal", 
				?_assertEqual(<<18,3,0,2,1,1>>, tlv:pack(?SMS_SIGNAL, 257))},
			 {"alert_on_message_delivery", 
				?_assertEqual(<<19,12,0,0>>, tlv:pack(?ALERT_ON_MESSAGE_DELIVERY, nil))},
			 {"its_reply_type", 
				?_assertEqual(<<19,8,0,1,2>>, tlv:pack(?ITS_REPLY_TYPE, ?ITS_REPLY_TELEPHONE))},
			 {"its_session_info", 
				?_assertEqual(<<19,131,0,2,55,29>>, tlv:pack(?ITS_SESSION_INFO, <<55,29>>))},
			 {"ussd_service_op", 
				?_assertEqual(<<5,1,0,1,19>>, tlv:pack(?USSD_SERVICE_OP, <<?USSD_USSN_CONFIRM>>))}
			]
		},

		{"pack_multi", 
			[
				{"list with no value will give empty binary", 
					?_assertEqual(<<>>, tlv:pack_multi(?NUMBER_OF_MESSAGES, []))}, 
				{"will pack list with single value", 
					?_assertEqual(<<3,4,0,1,10>>, tlv:pack_multi(?NUMBER_OF_MESSAGES, [10]))}, 
				{"will pack list with multiple values", 
					?_assertEqual(<<3,4,0,1,10,3,4,0,1,14,3,4,0,1,15,3,4,0,1,16>>, tlv:pack_multi(?NUMBER_OF_MESSAGES, [10,14,15,16]))} 
			]
		},

		{"unpack",
			[
				{"dest_addr_subunit", 
					?_assertEqual({?ADDR_SUBUNIT_UNKNOWN, <<>>}, tlv:unpack(?DEST_ADDR_SUBUNIT, <<0,1,0>>))},
				{"source_addr_subunit", 
					?_assertEqual({?ADDR_SUBUNIT_MOBILE_EQUIPMENT, <<>>}, tlv:unpack(?SOURCE_ADDR_SUBUNIT, <<0,1,2>>))},
				{"dest_network_type", 
					?_assertEqual({?NETWORK_TYPE_GSM, <<>>}, tlv:unpack(?DEST_NETWORK_TYPE, <<0,1,1>>))},
				{"source_network_type", 
					?_assertEqual({?NETWORK_TYPE_CDMA, <<>>}, tlv:unpack(?SOURCE_NETWORK_TYPE, <<0,1,3>>))},
				{"dest_bearer_type", 
					?_assertEqual({?BEARER_TYPE_SMS, <<>>}, tlv:unpack(?DEST_BEARER_TYPE, <<0,1,1>>))},
				{"source_bearer_type", 
					?_assertEqual({?BEARER_TYPE_USSD, <<>>}, tlv:unpack(?SOURCE_BEARER_TYPE, <<0,1,4>>))},
				{"dest_telematics_id", 
					?_assertEqual({2, <<>>}, tlv:unpack(?DEST_TELEMATICS_ID, <<0,2,0,2>>))},
				{"source_telematics_id", 
					?_assertEqual({1, <<>>}, tlv:unpack(?SOURCE_TELEMATICS_ID, <<0,1,1>>))},
				{"qos_time_to_live", 
					?_assertEqual({300, <<>>}, tlv:unpack(?SOURCE_TELEMATICS_ID, <<0,4,0,0,1,44>>))},
				{"payload_type", 
					?_assertEqual({?PAYLOAD_TYPE_WCMP, <<>>}, tlv:unpack(?PAYLOAD_TYPE, <<0,1,1>>))},
				{"additional_status_info_text", 
					?_assertEqual({"foo bar", <<>>}, tlv:unpack(?ADDITIONAL_STATUS_INFO_TEXT, <<0,8,102,111,111,32,98,97,114,0>>))},
				{"receipted_message_id", 
					?_assertEqual({"foobar", <<>>}, tlv:unpack(?RECEIPTED_MESSAGE_ID, <<0,7,102,111,111,98,97,114,0>>))},
				{"ms_msg_wait_facilities", 
					?_assertEqual({?MSG_WAIT_ACTIVE bor ?MSG_WAIT_TYPE_EMAIL, <<>>}, tlv:unpack(?MS_MSG_WAIT_FACILITIES, <<0,1,130>>))},
				{"privacy_indicator", 
					?_assertEqual({?PRIVACY_INDICATOR_SECRET, <<>>}, tlv:unpack(?PRIVACY_INDICATOR, <<0,1,3>>))},
				{"source_subaddress", 
					?_assertEqual({<<1,2,3,4,5>>, <<>>}, tlv:unpack(?SOURCE_SUBADDRESS, <<0,5,1,2,3,4,5>>))},
				{"dest_subaddress", 
					?_assertEqual({<<0,1,2>>, <<>>}, tlv:unpack(?DEST_SUBADDRESS, <<0,3,0,1,2>>))},
				{"user_message_reference", 
					?_assertEqual({260, <<>>}, tlv:unpack(?USER_MESSAGE_REFERENCE, <<0,2,1,4>>))},
				{"user_response_code", 
					?_assertEqual({67, <<>>}, tlv:unpack(?USER_RESPONSE_CODE, <<0,1,67>>))},
				{"language_indicator", 
					?_assertEqual({?LANGUAGE_INDICATOR_FRENCH, <<>>}, tlv:unpack(?LANGUAGE_INDICATOR, <<0,1,2>>))},
				{"source_port", 
					?_assertEqual({315, <<>>}, tlv:unpack(?SOURCE_PORT, <<0,2,1,59>>))},
				{"destination_port", 
					?_assertEqual({538, <<>>}, tlv:unpack(?SOURCE_PORT, <<0,2,2,26>>))},
				{"sar_msg_ref_num", 
					?_assertEqual({111, <<>>}, tlv:unpack(?SOURCE_PORT, <<0,2,0,111>>))},
				{"sar_total_segments", 
					?_assertEqual({5, <<>>}, tlv:unpack(?SAR_TOTAL_SEGMENTS, <<0,1,5>>))},
				{"sar_segment_seqnum", 
					?_assertEqual({39, <<>>}, tlv:unpack(?SAR_SEGMENT_SEQNUM, <<0,1,39>>))},
				{"sc_interface_version", 
					?_assertEqual({16#34, <<>>}, tlv:unpack(?SC_INTERFACE_VERSION, <<0,1,52>>))},
				{"display_time", 
					?_assertEqual({?DISPLAY_TIME_DEFAULT, <<>>}, tlv:unpack(?DISPLAY_TIME, <<0,1,1>>))},
				{"ms_validity", 
					?_assertEqual({?MS_VALIDITY_DISPLAY, <<>>}, tlv:unpack(?MS_VALIDITY, <<0,1,3>>))},
				{"dpf_result", 
					?_assertEqual({?DPF_RESULT_NOT_SET, <<>>}, tlv:unpack(?DPF_RESULT, <<0,1,0>>))},
				{"set_dpf", 
					?_assertEqual({?SET_DPF_DEFAULT, <<>>}, tlv:unpack(?SET_DPF, <<0,1,1>>))},
				{"ms_availability_status", 
					?_assertEqual({?MS_AVAILABILITY_DENIED, <<>>}, tlv:unpack(?MS_AVAILABILITY_STATUS, <<0,1,1>>))},
				{"network_error_code",
					?_assertEqual({<<?NETWORK_ERROR_TYPE_GSM:8,257:16>>, <<>>}, tlv:unpack(?NETWORK_ERROR_CODE, <<0,3,3,1,1>>))},
				{"message_payload",
					?_assertEqual({<<"abcde">>, <<>>}, tlv:unpack(?MESSAGE_PAYLOAD, <<0,5,97,98,99,100,101>>))},
				{"delivery_failure_reason",
					?_assertEqual({?DEL_FAIL_REASON_TEMP_NET_ERR, <<>>}, tlv:unpack(?DELIVERY_FAILURE_REASON, <<0,1,3>>))},
				{"more_messages_to_send",
					?_assertEqual({?MORE_MESSAGES_TRUE, <<>>}, tlv:unpack(?MORE_MESSAGES_TO_SEND, <<0,1,1>>))},
				{"message_state",
					?_assertEqual({?DPF_RESULT_NOT_SET, <<>>}, tlv:unpack(?MESSAGE_STATE, <<0,1,0>>))}, 
				{"callback_num",
					?_assertEqual({<<?DMI_ASCII, ?TON_NATIONAL, ?NPI_NATIONAL, "08035671234">>, <<>>}, 
							tlv:unpack(?CALLBACK_NUM, <<0,14,1,2,8,48,56,48,51,53,54,55,49,50,51,52>>))},
				{"callback_num_pres_ind",
					?_assertEqual({?CALLBACK_NUM_PRES_ALLOWED bor ?CALLBACK_NUM_NETWORK_PROVIDED, <<>>}, 
							tlv:unpack(?CALLBACK_NUM_PRES_IND, <<0,1,3>>))},
				{"callback_num_atag",
					?_assertEqual({<<?DCS_ASCII,"08031234567">>, <<>>}, 
							tlv:unpack(?CALLBACK_NUM_ATAG, <<0,12,1,48,56,48,51,49,50,51,52,53,54,55>>))},
				{"number_of_messages",
					?_assertEqual({48, <<>>}, tlv:unpack(?NUMBER_OF_MESSAGES, <<0,1,48>>))},
				{"sms_signal",
					?_assertEqual({257, <<>>}, tlv:unpack(?SMS_SIGNAL, <<0,2,1,1>>))},
				{"alert_on_message_delivery will unpack nothing when nothing is given",
					?_assertEqual({<<>>, <<>>}, tlv:unpack(?ALERT_ON_MESSAGE_DELIVERY, <<0,0>>))}, 
				{"alert_on_message_delivery will unpack nothing with remains if given data",
					?_assertEqual({<<>>, <<1,2,3>>},
							tlv:unpack(?ALERT_ON_MESSAGE_DELIVERY, <<0,0,1,2,3>>))},
				{"its_reply_type",
					?_assertEqual({?ITS_REPLY_CONTINUE, <<>>}, tlv:unpack(?ITS_REPLY_TYPE, <<0,1,8>>))},
				{"its_session_info",
					?_assertEqual({<<129,10>>, <<>>}, tlv:unpack(?ITS_SESSION_INFO, <<0,2,129,10>>))},
				{"ussd_service_op",
					?_assertEqual({<<?USSD_USSN_CONFIRM>>, <<>>}, tlv:unpack(?USSD_SERVICE_OP, <<0,1,19>>))} 			
			]
		}
	].
