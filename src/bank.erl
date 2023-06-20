-module(bank).

%% API
-export([process_bank/4]).

process_bank(MasterPID, Name, Loan_to_give, CustomerInfo) ->
  %io:fwrite("~s knows about: ~p~n", [Name, CustomerInfo]),
  Msg = {Name, Loan_to_give},
  MasterPID ! {process_bank, self(), Msg},
  receive
    {completed, BankId} ->
      io:fwrite("Bank ~s [~p] completed.~n", [Name, BankId]),
      ok
  end.