-module(bank).

%% API
-export([process_bank/3]).

process_bank(MasterPID, Name, Loan_to_give) ->
  Msg = {Name, Loan_to_give},
  MasterPID ! {process_bank, self(), Msg},
  receive
    {completed, BankId} ->
      io:fwrite("Bank ~s [~p] completed.~n", [Name, BankId]),
      ok
  end.