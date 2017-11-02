%%%-------------------------------------------------------------------
%%% @author Antarix Tandon
%%% @copyright (C) 2017, Celusion Technologies Pvt. Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2017 6:26 PM
%%%-------------------------------------------------------------------
-module(smpp34pdu_bind_receiver).
-author("Antarix Tandon").

-include("smpp34pdu.hrl").
-include("types.hrl").

-import(tools, [cstring_to_bin/2, integer_to_bin/2]).
-import(tools, [bin_to_cstring/2, bin_to_integer/2]).

%% API
-export([pack/1, unpack/1]).
-spec pack(bind_receiver()) -> binary().
-spec unpack(binary()) -> bind_receiver().


pack(#bind_receiver{system_id=SystemId,
  password=Password,
  system_type=SystemType,
  interface_version=InterfaceVersion,
  addr_ton=AddrTon,
  addr_npi=AddrNpi,
  address_range=AddressRange}) ->

  L = [cstring_to_bin(SystemId, 16),
    cstring_to_bin(Password, 9),
    cstring_to_bin(SystemType, 13),
    integer_to_bin(InterfaceVersion, 1),
    integer_to_bin(AddrTon, 1),
    integer_to_bin(AddrNpi, 1),
    cstring_to_bin(AddressRange, 41)],

  list_to_binary(L).


unpack(Bin0) ->
  {SystemId, Bin1} = bin_to_cstring(Bin0, 16),
  {Password, Bin2} = bin_to_cstring(Bin1, 9),
  {SystemType, Bin3} = bin_to_cstring(Bin2, 13),
  {InterfaceVersion, Bin4} = bin_to_integer(Bin3, 1),
  {AddrTon, Bin5} = bin_to_integer(Bin4, 1),
  {AddrNpi, Bin6} = bin_to_integer(Bin5, 1),
  {AddressRange, _} = bin_to_cstring(Bin6, 41),

  #bind_receiver {system_id=SystemId,
    password=Password,
    system_type=SystemType,
    interface_version=InterfaceVersion,
    addr_ton=AddrTon,
    addr_npi=AddrNpi,
    address_range=AddressRange}.
