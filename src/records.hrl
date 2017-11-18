%%%-------------------------------------------------------------------
%%% @author Praveen Paulose
%%% @copyright (C) 2017, Celusion Technologies Pvt. Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2017 22:06
%%%-------------------------------------------------------------------
-author("Praveen Paulose").

-ifndef(records).
-define(records, true).

-include("constants.hrl").

-record(pdu, {command_length,
  command_id,
  command_status,
  sequence_number,
  body}).

-record(bind_transmitter, {system_id=?DEFAULT_CSTRING,
  password=?DEFAULT_CSTRING,
  system_type=?DEFAULT_CSTRING,
  interface_version=?VERSION,
  addr_ton=?DEFAULT_TON,
  addr_npi=?DEFAULT_NPI,
  address_range=?DEFAULT_CSTRING}).

-record(bind_transmitter_resp, {system_id=?DEFAULT_CSTRING,
  sc_interface_version}).

-record(bind_receiver, {system_id=?DEFAULT_CSTRING,
  password=?DEFAULT_CSTRING,
  system_type=?DEFAULT_CSTRING,
  interface_version=?VERSION,
  addr_ton=?DEFAULT_TON,
  addr_npi=?DEFAULT_NPI,
  address_range=?DEFAULT_CSTRING}).

-record(bind_receiver_resp, {system_id=?DEFAULT_CSTRING,
  sc_interface_version}).

-record(bind_transceiver, {system_id=?DEFAULT_CSTRING,
  password=?DEFAULT_CSTRING,
  system_type=?DEFAULT_CSTRING,
  interface_version=?VERSION,
  addr_ton=?DEFAULT_TON,
  addr_npi=?DEFAULT_NPI,
  address_range=?DEFAULT_CSTRING}).

-record(bind_transceiver_resp, {system_id=?DEFAULT_CSTRING,
  sc_interface_version}).

-record(outbind, {system_id=?DEFAULT_CSTRING,
  password=?DEFAULT_CSTRING}).

-record(generic_nack, {}).

-record(submit_sm, {service_type=?DEFAULT_CSTRING,
  source_addr_ton=?DEFAULT_TON,
  source_addr_npi=?DEFAULT_NPI,
  source_addr=?DEFAULT_CSTRING,
  dest_addr_ton=?DEFAULT_TON,
  dest_addr_npi=?DEFAULT_NPI,
  destination_addr=?DEFAULT_CSTRING,
  esm_class=?DEFAULT_ESM_CLASS,
  protocol_id=?DEFAULT_PROTOCOL_ID,
  priority_flag=?DEFAULT_PRIORITY_FLAG,
  schedule_delivery_time=?DEFAULT_CSTRING,
  validity_period=?DEFAULT_CSTRING,
  registered_delivery=?DEFAULT_REGISTERED_DELIVERY,
  replace_if_present_flag=?DEFAULT_REPLACE_IF_PRESENT_FLAG,
  data_coding=?DEFAULT_DATA_CODING,
  sm_default_msg_id=?DEFAULT_SM_DEFAULT_MSG_ID,
  sm_length=?DEFAULT_SM_LENGTH,
  short_message=?DEFAULT_CSTRING,
  user_message_reference,
  source_port,
  source_addr_subunit,
  destination_port,
  dest_addr_subunit,
  sar_msg_ref_num,
  sar_total_segments,
  sar_segment_seqnum,
  more_messages_to_send,
  payload_type,
  message_payload,
  privacy_indicator,
  callback_num,
  callback_num_pres_ind,
  callback_num_atag,
  source_subaddress,
  dest_subaddress,
  user_response_code,
  display_time,
  sms_signal,
  ms_validity,
  ms_msg_wait_facilities,
  number_of_messages,
  alert_on_msg_delivery,
  language_indicator,
  its_reply_type,
  its_session_info,
  ussd_service_op}).

-record(submit_sm_resp,{message_id=?DEFAULT_CSTRING}).

-record(deliver_sm, {service_type=?DEFAULT_CSTRING,
  source_addr_ton=?DEFAULT_TON,
  source_addr_npi=?DEFAULT_NPI,
  source_addr=?DEFAULT_CSTRING,
  dest_addr_ton=?DEFAULT_TON,
  dest_addr_npi=?DEFAULT_NPI,
  destination_addr=?DEFAULT_CSTRING,
  esm_class=?DEFAULT_ESM_CLASS,
  protocol_id=?DEFAULT_PROTOCOL_ID,
  priority_flag=?DEFAULT_PRIORITY_FLAG,
  schedule_delivery_time=?DEFAULT_CSTRING,
  validity_period=?DEFAULT_CSTRING,
  registered_delivery=?DEFAULT_REGISTERED_DELIVERY,
  replace_if_present_flag=?DEFAULT_REPLACE_IF_PRESENT_FLAG,
  data_coding=?DEFAULT_DATA_CODING,
  sm_default_msg_id=?DEFAULT_SM_DEFAULT_MSG_ID,
  sm_length=?DEFAULT_SM_LENGTH,
  short_message=?DEFAULT_STRING,
  user_message_reference,
  source_port,
  destination_port,
  sar_msg_ref_num,
  sar_total_segments,
  sar_segment_seqnum,
  user_response_code,
  privacy_indicator,
  payload_type,
  message_payload,
  callback_num,
  source_subaddress,
  dest_subaddress,
  language_indicator,
  its_session_info,
  network_error_code,
  message_state,
  receipted_message_id
}).

-record(deliver_sm_resp, {message_id=?DEFAULT_CSTRING}).

-record(unbind, {}).

-record(unbind_resp, {}).

-record(replace_sm_resp, {}).


-record(enquire_link, {}).
-record(enquire_link_resp, {}).

-record(alert_notification, {source_addr_ton=?DEFAULT_TON,
  source_addr_npi=?DEFAULT_NPI,
  source_addr=?DEFAULT_CSTRING,
  esme_addr_ton=?DEFAULT_TON,
  esme_addr_npi=?DEFAULT_NPI,
  esme_addr=?DEFAULT_CSTRING,
  ms_availability_status}).


-record(query_sm, {message_id=?DEFAULT_CSTRING,
  source_addr_ton=?DEFAULT_TON,
  source_addr_npi=?DEFAULT_NPI,
  source_addr=?DEFAULT_CSTRING}).

-record(data_sm_resp, {message_id=?DEFAULT_CSTRING,
  delivery_failure_reason,
  network_error_code,
  additional_status_info_text,
  dpf_result}).

-record(query_sm_resp, {message_id=?DEFAULT_CSTRING,
  final_date=?DEFAULT_CSTRING,
  message_state=0,
  error_code=0}).

-record(cancel_sm, {service_type=?DEFAULT_CSTRING,
  message_id=?DEFAULT_CSTRING,
  source_addr_ton=?DEFAULT_TON,
  source_addr_npi=?DEFAULT_NPI,
  source_addr=?DEFAULT_CSTRING,
  dest_addr_ton=?DEFAULT_TON,
  dest_addr_npi=?DEFAULT_NPI,
  destination_addr=?DEFAULT_CSTRING}).

-record(cancel_sm_resp, {}).

-record(replace_sm, {message_id=?DEFAULT_CSTRING,
  source_addr_ton=?DEFAULT_TON,
  source_addr_npi=?DEFAULT_NPI,
  source_addr=?DEFAULT_CSTRING,
  schedule_delivery_time=?DEFAULT_CSTRING,
  validity_period=?DEFAULT_CSTRING,
  registered_delivery = 0,
  sm_default_msg_id = 0,
  sm_length=0,
  short_message=?DEFAULT_STRING}).

-record(replace_sm_resp, {}).


%%Names.erl
-define(SMPP_VERSION(Version),
  case Version of
    16#30 -> 3.0;
    16#33 -> 3.3;
    16#34 -> 3.4;
    16#50 -> 5.0;
    N -> N
  end).

-define(SMPP_STATUS(Status),
  case Status of
    ?ESME_ROK -> 'ESME_ROK';
    ?ESME_RINVMSGLEN -> 'ESME_RINVMSGLEN';
    ?ESME_RINVCMDLEN -> 'ESME_RINVCMDLEN';
    ?ESME_RINVCMDID -> 'ESME_RINVCMDID';
    ?ESME_RINVBNDSTS -> 'ESME_RINVBNDSTS';
    ?ESME_RALYBND -> 'ESME_RALYBND';
    ?ESME_RINVPRTFLG -> 'ESME_RINVPRTFLG';
    ?ESME_RINVREGDLVFLG -> 'ESME_RINVREGDLVFLG';
    ?ESME_RSYSERR -> 'ESME_RSYSERR';
    ?ESME_RINVSRCADR -> 'ESME_RINVSRCADR';
    ?ESME_RINVDSTADR -> 'ESME_RINVDSTADR';
    ?ESME_RINVMSGID -> 'ESME_RINVMSGID';
    ?ESME_RBINDFAIL -> 'ESME_RBINDFAIL';
    ?ESME_RINVPASWD -> 'ESME_RINVPASWD';
    ?ESME_RINVSYSID -> 'ESME_RINVSYSID';
    ?ESME_RCANCELFAIL -> 'ESME_RCANCELFAIL';
    ?ESME_RREPLACEFAIL -> 'ESME_RREPLACEFAIL';
    ?ESME_RMSGQFUL -> 'ESME_RMSGQFUL';
    ?ESME_RINVSERTYP -> 'ESME_RINVSERTYP';
    ?ESME_RINVNUMDESTS -> 'ESME_RINVNUMDESTS';
    ?ESME_RINVDLNAME -> 'ESME_RINVDLNAME';
    ?ESME_RINVDESTFLAG -> 'ESME_RINVDESTFLAG';
    ?ESME_RINVSUBREP -> 'ESME_RINVSUBREP';
    ?ESME_RINVESMCLASS -> 'ESME_RINVESMCLASS';
    ?ESME_RCNTSUBDL -> 'ESME_RCNTSUBDL';
    ?ESME_RSUBMITFAIL -> 'ESME_RSUBMITFAIL';
    ?ESME_RINVSRCTON -> 'ESME_RINVSRCTON';
    ?ESME_RINVSRCNPI -> 'ESME_RINVSRCNPI';
    ?ESME_RINVDSTTON -> 'ESME_RINVDSTTON';
    ?ESME_RINVDSTNPI -> 'ESME_RINVDSTNPI';
    ?ESME_RINVSYSTYP -> 'ESME_RINVSYSTYP';
    ?ESME_RINVREPFLAG -> 'ESME_RINVREPFLAG';
    ?ESME_RINVNUMMSGS -> 'ESME_RINVNUMMSGS';
    ?ESME_RTHROTTLED -> 'ESME_RTHROTTLED';
    ?ESME_RINVSCHED -> 'ESME_RINVSCHED';
    ?ESME_RINVEXPIRY -> 'ESME_RINVEXPIRY';
    ?ESME_RINVDFTMSGID -> 'ESME_RINVDFTMSGID';
    ?ESME_RX_T_APPN -> 'ESME_RX_T_APPN';
    ?ESME_RX_P_APPN -> 'ESME_RX_P_APPN';
    ?ESME_RX_R_APPN -> 'ESME_RX_R_APPN';
    ?ESME_RQUERYFAIL -> 'ESME_RQUERYFAIL';
    ?ESME_RINVOPTPARSTREAM -> 'ESME_INVOPTPARSTREAM';
    ?ESME_ROPTPARNOTALLWD -> 'ESME_ROPTPARNOTALLWD';
    ?ESME_RINVPARLEN -> 'ESME_RINVPARLEN';
    ?ESME_RMISSINGOPTPARAM -> 'ESME_RMISSINGOPTPARAM';
    ?ESME_RINVOPTPARAMVAL -> 'ESME_RINVOPTPARAMVAL';
    ?ESME_RDELIVERYFAILURE -> 'ESME_RDELIVERYFAILURE';
    ?ESME_RUNKNOWNERR -> 'ESME_RUNKNOWNERR';
    _-> 'INVALID_SMPP_STATUS'
  end).

-define(SMPP_PDU2CMDID(PduBody),
  case PduBody of
    #generic_nack{} -> 'GENERIC_NACK';
    #bind_receiver{} -> 'BIND_RECEIVER';
    #bind_receiver_resp{} -> 'BIND_RECEIVER_RESP';
    #bind_transmitter{} -> 'BIND_TRANSMITTER';
    #bind_transmitter_resp{} -> 'BIND_TRANSMITTER_RESP';
    #query_sm{} -> 'QUERY_SM';
    #query_sm_resp{} -> 'QUERY_SM_RESP';
    #submit_sm{} -> 'SUBMIT_SM';
    #submit_sm_resp{} -> 'SUBMIT_SM_RESP';
    #deliver_sm{} -> 'DELIVER_SM';
    #deliver_sm_resp{} -> 'DELIVER_SM_RESP';
    #unbind{} -> 'UNBIND';
    #unbind_resp{} -> 'UNBIND_RESP';
    #replace_sm{} -> 'REPLACE_SM';
    #replace_sm_resp{} -> 'REPLACE_SM_RESP';
    #cancel_sm{} -> 'CANCEL_SM';
    #cancel_sm_resp{} -> 'CANCEL_SM_RESP';
    #bind_transceiver{} -> 'BIND_TRANSCEIVER';
    #bind_transceiver_resp{} -> 'BIND_TRANSCEIVER_RESP';
    #outbind{} -> 'OUTBIND';
    #enquire_link{} -> 'ENQUIRE_LINK';
    #enquire_link_resp{} -> 'ENQUIRE_LINK_RESP';
    %     #submit_multi{} -> 'SUBMIT_MULTI';
    %     #submit_multi_resp{} -> 'SUBMIT_MULTI_RESP';
    #alert_notification{} -> 'ALERT_NOTIFICATION';
    %%     #data_sm{} -> 'DATA_SM';
    #data_sm_resp{} -> 'DATA_SM_RESP'
  end).

-endif.