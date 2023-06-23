-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  BanksNamesWithIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, BankInfo),
  %io:fwrite("~s knows about ~p IDs~n", [Name, BankInfo]),
  io:fwrite("~s has an objective of  ~p ~n", [Name, LoanNeeded]),

  %Random selection of loan and bank
  make_request(Name, BanksNamesWithIDs, LoanNeeded, MasterPID, LoanNeeded).

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


make_request(Name, BankIDs, LoanNeeded, MasterPID, OriginalObjective) ->
  sleep_random_period(),
  RandomLoanAmount = generate_random_loan(),

  MaxLoanAmount = min(RandomLoanAmount, LoanNeeded),

  RemainingLoan = LoanNeeded - MaxLoanAmount,
  NewRemainingLoan = if RemainingLoan < 0 -> 0; true -> RemainingLoan end,

  {BankName, BankPID} = select_random_bank(BankIDs),
  io:fwrite("~s has an objective of ~p~n", [Name, NewRemainingLoan]),
  io:fwrite("Customer ~s making a loan request to bank ~p for ~B dollars. Remaining Loan Needed: ~B~n", [Name, {BankName, BankPID}, RandomLoanAmount, NewRemainingLoan]),

  %sendingBnak
  BankPID ! {loan_request, Name, MaxLoanAmount},

  %sending Master
  Msg = {Name, MaxLoanAmount, BankName},
  MasterPID ! {process_customer, self(), Msg}, % Include self() in the message

  process_request(Name, BankIDs, NewRemainingLoan, MasterPID, BankName, MaxLoanAmount, OriginalObjective).


process_request(Name, BankIDs, TotalLoanNeeded, MasterPID, RejectedBankName, RequestedAmount, OriginalObjective) ->
  receive
    {loan_request_accepted, CustomerID} ->
      %io:fwrite("~s has an objective of ~p~n", [Name, TotalLoanNeeded]),
      case TotalLoanNeeded of
        0 -> % Loan objective met
          io:fwrite("OBJECTIVE MET FOR ~p~n", [Name]),
          AmountTakenSoFar = OriginalObjective - TotalLoanNeeded,
          Msg = {Name, AmountTakenSoFar, OriginalObjective},
          MasterPID ! {customer_done, self(), Msg}, % Include self() in the me
          ok;
        _ -> % Loan objective not met, make another request
          make_request(Name, BankIDs, TotalLoanNeeded, MasterPID, OriginalObjective)
      end;
    {loan_request_rejected, CustomerID} ->
      io:fwrite("~s's loan request was rejected by ~p Making another request.~n", [Name, RejectedBankName]),

      %sending to Master
      Msg = {Name, RequestedAmount, RejectedBankName},
      MasterPID ! {process_customer_rejected, self(), Msg}, % Include self() in the me

      UpdatedBankIDs = remove_rejected_bank(BankIDs, RejectedBankName),
      io:fwrite("Updated bank IDs: ~p~n", [UpdatedBankIDs]),

      case UpdatedBankIDs of
        [] -> % No more banks available
          io:fwrite("NO MORE BANKS FOR ~p~n", [Name]),
          io:fwrite("ORIGINAL OBJECTIVE ~p~n", [OriginalObjective]),
          io:fwrite("TOTAL OAD NEEDED ~p~n", [TotalLoanNeeded]),
          AmountTakenSoFar = OriginalObjective - TotalLoanNeeded,
          CustomerData = {Name, AmountTakenSoFar, OriginalObjective},
          MasterPID ! {customer_done, self(), CustomerData}, % Include self() in the message
          ok;
        _ -> % Make another request with updated bank list
          make_request(Name, UpdatedBankIDs, TotalLoanNeeded, MasterPID, OriginalObjective)
      end
  end.

remove_rejected_bank(BankIDs, RejectedBankName) ->
  lists:filter(fun({BankName, _}) -> BankName /= RejectedBankName end, BankIDs).

