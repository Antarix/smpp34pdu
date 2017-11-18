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
-record(unbind, {}).
-record(unbind_resp, {}).

-record(alert_notification, {source_addr_ton=?DEFAULT_TON,
  source_addr_npi=?DEFAULT_NPI,
  source_addr=?DEFAULT_CSTRING,
  esme_addr_ton=?DEFAULT_TON,
  esme_addr_npi=?DEFAULT_NPI,
  esme_addr=?DEFAULT_CSTRING,
  ms_availability_status}).

-endif.