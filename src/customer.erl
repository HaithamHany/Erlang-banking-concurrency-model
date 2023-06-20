-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  BanksNamesWithIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, BankInfo),
  %io:fwrite("~s knows about ~p IDs~n", [Name, BankIDs]),

  %Random selection of loan and bank
  make_request(Name, BanksNamesWithIDs),

  %Message formulation Send/Recive
  Msg = {Name, LoanNeeded, BankInfo},
  MasterPID ! {process_customer, self(), Msg}, % Include self() in the message
  receive
    {completed, CustomerID} ->
      io:fwrite("Customer ~s [~p] completed.~n", [Name, CustomerID])
  end.


generate_random_loan() ->
  RandomLoan = rand:uniform(50),  % Generate a random loan amount between 1 and 50
  RandomLoan.

select_random_bank(BankIDs) ->
  RandomIndex = rand:uniform(length(BankIDs)),
  {BankName, BankPID} = lists:nth(RandomIndex, BankIDs),
  {BankName, BankPID}.


sleep_random_period() ->
  RandomSleep = rand:uniform(91) + 10,  % Generate a random number between 10 and 100
  timer:sleep(RandomSleep).


make_request(Name, BankIDs) ->
  sleep_random_period(),
  RandomLoanAmount = generate_random_loan(),
  {_, BankPID} = select_random_bank(BankIDs), % Extract BankPID from tuple
  io:fwrite("Customer ~s making a loan request to bank ~p for ~B dollars.~n", [Name, BankPID, RandomLoanAmount]),
  BankPID ! {loan_request, RandomLoanAmount}.