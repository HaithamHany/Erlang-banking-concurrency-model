-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  PotentialBanks = money:get_potential_banks(LoanNeeded, BankInfo),
  BankIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, PotentialBanks),
  io:fwrite("Bank IDs: ~p~n", [BankIDs]),
  Msg = {Name, LoanNeeded, PotentialBanks},
  MasterPID ! {process_customer, self(), Msg}, % Include self() in the message
  receive
    {completed, CustomerID} ->
      io:fwrite("Customer ~s [~p] completed.~n", [Name, CustomerID]),
      ok
  end.





