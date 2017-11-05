%%%-------------------------------------------------------------------
%%% @author Antarix Tandon
%%% @copyright (C) 2017, Celusion Technologies Pvt. Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2017 6:34 PM
%%%-------------------------------------------------------------------
-module(smpp34pdu_submit_sm_resp).
-author("Antarix Tandon").

-include("smpp34pdu.hrl").
-include("types.hrl").

-import(tools, [cstring_to_bin/2, integer_to_bin/2]).
-import(tools, [bin_to_cstring/2, bin_to_integer/2]).


%% API
-export([pack/1,unpack/1]).

-spec pack(submit_sm_resp()) -> binary().
-spec unpack(binary()) -> submit_sm_resp().

pack(#submit_sm_resp{message_id = MessageId}) ->
  L = [cstring_to_bin(MessageId, 65)],
  list_to_binary(L).


unpack(Bin0) ->
  {MessageId, _} = bin_to_cstring(Bin0, 65),

  #submit_sm_resp{message_id=MessageId}.