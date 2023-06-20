-module(customer).
-export([process_customer/5]).

process_customer(MasterPID, Pid, Name, LoanNeeded, BankInfo) ->
  PotentialBanks = money:get_potential_banks(LoanNeeded, BankInfo),
  Msg = {Pid, Name, LoanNeeded, PotentialBanks},
  MasterPID ! {process_customer, Msg},
  receive
    {completed} ->
      ok
  end.
