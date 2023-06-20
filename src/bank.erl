-module(bank).

%% API
-export([process_bank/4]).

process_bank(MasterPID, Name, Loan_to_give, CustomerInfo) ->
  %io:fwrite("~s knows about: ~p~n", [Name, CustomerInfo]),
  Msg = {Name, Loan_to_give},
  MasterPID ! {process_bank, self(), Msg},
  receive
    {loan_request, CustomerName, LoanAmount} -> % Add RequesterID pattern matching
      io:fwrite("Bank ~s received loan request from customer ~s [~p] for ~B dollars.~n", [Name, CustomerName, whereis(CustomerName), LoanAmount]),
      % Process the loan request and send the response
      % ...
      % Assuming the loan request is processed successfully and completed
      io:fwrite("Bank ~s [~p] completed request processing.~n", [Name, Name]),
      process_bank(MasterPID, Name, Loan_to_give, CustomerInfo)
  end.