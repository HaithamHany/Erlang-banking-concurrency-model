-module(customer).
-export([process_customer/3]).

process_customer(Name, LoanNeeded, BankInfo) ->
  PotentialBanks = money:get_potential_banks(LoanNeeded, BankInfo),
  io:format("~s needs a loan of ~B. Potential banks: ~p~n", [Name, LoanNeeded, PotentialBanks]).