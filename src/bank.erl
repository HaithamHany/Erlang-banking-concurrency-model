-module(bank).

%% API
-export([process_bank/4]).

process_bank(MasterPID, BankName, Total_Loan, CustomerInfo) ->
  %io:fwrite("~s knows about: ~p~n", [Name, CustomerInfo]),
  receive
    {loan_request, CustomerName, NeededLoanAmount} -> % Add RequesterID pattern matching
      CustomerID = whereis(CustomerName),
      io:fwrite("Bank ~s received loan request from customer ~s [~p] for ~B dollars.~n", [BankName, CustomerName,CustomerID , NeededLoanAmount]),
      % Process the loan request and send the response
      % ...
      % Assuming the loan request is processed successfully and completed

      %sending Customer
      ResponseMsg = {loan_request_accepted, CustomerID},
      CustomerID ! ResponseMsg,

      %sending Master
      Msg = {BankName, NeededLoanAmount, CustomerName},
      MasterPID ! {process_bank, self(), Msg},

      io:fwrite("Bank ~s [~p] completed request processing.~n", [BankName, BankName]),
      process_bank(MasterPID, BankName, NeededLoanAmount, CustomerInfo)
  end.