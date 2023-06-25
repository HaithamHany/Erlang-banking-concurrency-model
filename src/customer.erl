-module(customer).
-export([process_customer/4]).

process_customer(MasterPID, Name, LoanNeeded, BankInfo) ->
  BanksNamesWithIDs = lists:map(fun({BankName, _}) -> {BankName, whereis(BankName)} end, BankInfo),

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

  case MaxLoanAmount of
    0 ->

      AmountTakenSoFar = OriginalObjective - LoanNeeded,
      notify_master(MasterPID, Name, AmountTakenSoFar, OriginalObjective),
      ok;
    _ ->
      {BankName, BankPID} = select_random_bank(BankIDs),

      %sending Bank
      BankPID ! {loan_request, Name, MaxLoanAmount},

      %sending Master
      Msg = {Name, MaxLoanAmount, BankName},
      MasterPID ! {process_customer, self(), Msg}, % Include self() in the message

      process_request(Name, BankIDs, LoanNeeded, MasterPID, BankName, MaxLoanAmount, OriginalObjective)
  end.



process_request(Name, BankIDs, TotalLoanNeeded, MasterPID, RejectedBankName, RequestedAmount, OriginalObjective) ->
  receive
    {loan_request_accepted, CustomerID} ->
      case TotalLoanNeeded of
        0 -> % Loan objective met
          AmountTakenSoFar = OriginalObjective - TotalLoanNeeded,
          notify_master(MasterPID, Name, AmountTakenSoFar, OriginalObjective),
          ok;
        _ -> % Loan objective not met, make another request
          RemainingLoan = TotalLoanNeeded - RequestedAmount,
          NewRemainingLoan = if RemainingLoan < 0 -> 0; true -> RemainingLoan end,
          make_request(Name, BankIDs, NewRemainingLoan, MasterPID, OriginalObjective)
      end;
    {loan_request_rejected, CustomerID} ->

      %sending to Master
      Msg = {Name, RequestedAmount, RejectedBankName},
      MasterPID ! {process_customer_rejected, self(), Msg}, % Include self() in the me

      UpdatedBankIDs = remove_rejected_bank(BankIDs, RejectedBankName),

      case UpdatedBankIDs of
        [] -> % No more banks available
          AmountTakenSoFar = OriginalObjective - TotalLoanNeeded,
          notify_master(MasterPID, Name, AmountTakenSoFar, OriginalObjective),
          ok;
        _ -> % Make another request with updated bank list
          make_request(Name, UpdatedBankIDs, TotalLoanNeeded, MasterPID, OriginalObjective)
      end
  end.

remove_rejected_bank(BankIDs, RejectedBankName) ->
  lists:filter(fun({BankName, _}) -> BankName /= RejectedBankName end, BankIDs).

notify_master(MasterPID, Name, AmountTakenSoFar, OriginalObjective) ->
  Msg = {Name, AmountTakenSoFar, OriginalObjective},
  MasterPID ! {customer_done, self(), Msg}.