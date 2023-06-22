-module(bank).

%% API
-export([process_bank/4]).

process_bank(MasterPID, BankName, Total_Loan, CustomerInfo) ->
  process_bank(MasterPID, BankName, Total_Loan, Total_Loan, CustomerInfo). % Pass Total_Loan as an additional argument

process_bank(MasterPID, BankName, AvailableResources, Total_Loan, CustomerInfo) ->
  BankResources = AvailableResources, % Initialize bank resources with available resources

  receive
    {loan_request, CustomerName, NeededLoanAmount} ->
      CustomerID = whereis(CustomerName),
      io:fwrite("Bank ~s received loan request from customer ~s [~p] for ~B dollars.~n", [BankName, CustomerName, CustomerID, NeededLoanAmount]),

      case BankResources - NeededLoanAmount >= 0 of
        true -> % Loan can be granted
          NewBankResources = BankResources - NeededLoanAmount, % Calculate the new bank resources
          io:fwrite("[RESOURCE CALCULATION] Bank ~s granted loan to customer ~s of $~p  remaining resources of the bank are ~B dollars.~n", [BankName, CustomerName, NeededLoanAmount, NewBankResources]),
          % Sending customer a loan request accepted message
          CustomerID ! {loan_request_accepted, self()},
          process_bank(MasterPID, BankName, NewBankResources, Total_Loan, CustomerInfo); % Continue processing requests

        false -> % Loan cannot be granted
          io:fwrite("Bank [~s] rejected the loan request from customer ~s. Insufficient resources.~n", [BankName, CustomerName]),
          % Sending customer a loan request rejected message
          CustomerID ! {loan_request_rejected, self()},
          process_bank(MasterPID, BankName, BankResources, Total_Loan, CustomerInfo) % Continue processing requests
      end;

    _ ->
      io:fwrite("Bank [~s] completed request processing.~n", [BankName]),
      process_bank(MasterPID, BankName, BankResources, Total_Loan, CustomerInfo) % Continue processing requests
  end.