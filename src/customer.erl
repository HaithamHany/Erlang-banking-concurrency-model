-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  BanksNamesWithIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, BankInfo),
  %io:fwrite("~s knows about ~p IDs~n", [Name, BankInfo]),
  io:fwrite("~s has an objective of  ~p ~n", [Name, LoanNeeded]),

  %Random selection of loan and bank
  make_request(Name, BanksNamesWithIDs, LoanNeeded),

  %Message formulation Send/Recive
  Msg = {Name, LoanNeeded, BankInfo},
  MasterPID ! {process_customer, self(), Msg}, % Include self() in the message
  receive
    {completed, CustomerID} ->
      io:fwrite("~s has an objective of  ~p ~n", [Name, LoanNeeded])
      %make_request(Name, BanksNamesWithIDs, LoanNeeded)
      %process_customer(MasterPID, Name, LoanNeeded, BankInfo)
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

make_request_temp(Name, BankIDs) ->
  sleep_random_period(),
  RandomLoanAmount = generate_random_loan(),

  {BankName, BankPID} = select_random_bank(BankIDs), % Extract BankName and BankPID from tuple
  %io:fwrite("Ids are ~p selected id ~p ~n", [BankIDs, BankPID]),
  io:fwrite("Customer ~s making a loan request to bank ~p for ~B dollars.~n", [Name, {BankName, BankPID}, RandomLoanAmount]),
  BankPID ! {loan_request, Name, RandomLoanAmount}. % Include self() as the requester ID

make_request(Name, BankIDs, LoanNeeded) ->
  sleep_random_period(),
  RandomLoanAmount = generate_random_loan(),

  RemainingLoan = LoanNeeded - RandomLoanAmount,
  NewLoanNeeded = if RemainingLoan < 0 -> 0; true -> RemainingLoan end,

  {BankName, BankPID} = select_random_bank(BankIDs), % Extract BankName and BankPID from tuple
  %io:fwrite("Ids are ~p selected id ~p ~n", [BankIDs, BankPID]),
  io:fwrite("Customer ~s making a loan request to bank ~p for ~B dollars. Remaining Loan Needed: ~B~n", [Name, {BankName, BankPID}, RandomLoanAmount, NewLoanNeeded]),

  case NewLoanNeeded of
    0 -> % Loan objective met
      ok;
    _ -> % Loan objective not met, make another request
      BankPID ! {loan_request, Name, RandomLoanAmount},
      make_request(Name, BankIDs, NewLoanNeeded)
  end.