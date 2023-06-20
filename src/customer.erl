-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  BankIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, BankInfo),
  %io:fwrite("~s knows about ~p IDs~n", [Name, BankIDs]),

  %Random selection of loan and bank
  RandomLoan = generate_random_loan(),
  io:fwrite("Customer ~s requesting a loan of ~B dollars.~n", [Name, RandomLoan]),
  RandomBank = select_random_bank(BankIDs),
  io:fwrite("Customer ~s targeting bank: ~p~n", [Name, RandomBank]),


  %Message formulation Send/Recive
  Msg = {Name, LoanNeeded, BankInfo},
  MasterPID ! {process_customer, self(), Msg}, % Include self() in the message
  receive
    {completed, CustomerID} ->
      io:fwrite("Customer ~s [~p] completed.~n", [Name, CustomerID]),
      ok
  end.


generate_random_loan() ->
  RandomLoan = rand:uniform(50),  % Generate a random loan amount between 1 and 50
  RandomLoan.

select_random_bank(BankIDs) ->
  RandomIndex = rand:uniform(length(BankIDs)),
  {BankName, BankPID} = lists:nth(RandomIndex, BankIDs),
  {BankName, BankPID}.
