-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  BankIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, BankInfo),
  io:fwrite("~s knows about ~p IDs~n", [Name, BankIDs]),
  Msg = {Name, LoanNeeded, BankInfo},
  MasterPID ! {process_customer, self(), Msg}, % Include self() in the message
  receive
    {completed, CustomerID} ->
      io:fwrite("Customer ~s [~p] completed.~n", [Name, CustomerID]),
      ok
  end.





