-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  BanksNamesWithIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, BankInfo),
  %io:fwrite("~s knows about ~p IDs~n", [Name, BankInfo]),
  io:fwrite("~s has an objective of  ~p ~n", [Name, LoanNeeded]),

  %Random selection of loan and bank
  make_request(Name, BanksNamesWithIDs, LoanNeeded, MasterPID).

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


make_request(Name, BankIDs, LoanNeeded, MasterPID) ->
  sleep_random_period(),
  RandomLoanAmount = generate_random_loan(),

  MaxLoanAmount = min(RandomLoanAmount, LoanNeeded),

  RemainingLoan = LoanNeeded - MaxLoanAmount,
  NewLoanNeeded = if RemainingLoan < 0 -> 0; true -> RemainingLoan end,

  {BankName, BankPID} = select_random_bank(BankIDs),
  io:fwrite("Customer ~s making a loan request to bank ~p for ~B dollars. Remaining Loan Needed: ~B~n", [Name, {BankName, BankPID}, RandomLoanAmount, NewLoanNeeded]),

  %sendingBnak
  BankPID ! {loan_request, Name, MaxLoanAmount},

  %sending Master
  Msg = {Name, MaxLoanAmount, BankName},
  MasterPID ! {process_customer, self(), Msg}, % Include self() in the message

  process_request(Name, BankIDs, NewLoanNeeded, MasterPID, BankName).


process_request(Name, BankIDs, LoanNeeded, MasterPID, RejectedBankName) ->
  receive
    {loan_request_accepted, CustomerID} ->
      io:fwrite("~s has an objective of ~p~n", [Name, LoanNeeded]),
      case LoanNeeded of
        0 -> % Loan objective met
          ok;
        _ -> % Loan objective not met, make another request
          make_request(Name, BankIDs, LoanNeeded, MasterPID)
      end;
    {loan_request_rejected, CustomerID} ->
      io:fwrite("~s's loan request was rejected by ~p Making another request.~n", [Name, RejectedBankName]),
      UpdatedBankIDs = remove_rejected_bank(BankIDs, RejectedBankName),
      io:fwrite("Updated bank IDs: ~p~n", [UpdatedBankIDs]),
      make_request(Name, UpdatedBankIDs, LoanNeeded, MasterPID)
  end.

remove_rejected_bank(BankIDs, RejectedBankName) ->
  lists:filter(fun({BankName, _}) -> BankName /= RejectedBankName end, BankIDs).