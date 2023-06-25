-module(bank).

%% API
-export([process_bank/4]).

process_bank(MasterPID, BankName, Total_Loan, CustomerInfo) ->
  process_bank(MasterPID, BankName, Total_Loan, Total_Loan, CustomerInfo).

process_bank(MasterPID, BankName, AvailableResources, Total_Loan, CustomerInfo) ->
  BankResources = AvailableResources,

  receive
    {loan_request, CustomerName, NeededLoanAmount} ->
      CustomerID = whereis(CustomerName),

      case BankResources - NeededLoanAmount >= 0 of
        true -> % Loan can be granted
          NewBankResources = BankResources - NeededLoanAmount,
          % Sending customer a loan request accepted message
          CustomerID ! {loan_request_accepted, self()},

          OriginalAmount = Total_Loan,
          Msg = {CustomerName, NeededLoanAmount, BankName, OriginalAmount},
          MasterPID ! {process_bank, self(), Msg},

          process_bank(MasterPID, BankName, NewBankResources, Total_Loan, CustomerInfo);

        false -> % Loan cannot be granted
          % Sending customer a loan request rejected message
          CustomerID ! {loan_request_rejected, self()},

          Msg = {CustomerName, NeededLoanAmount, BankName},
          MasterPID ! {process_bank_rejected, self(), Msg},

          process_bank(MasterPID, BankName, BankResources, Total_Loan, CustomerInfo)
      end;

    _ ->
      process_bank(MasterPID, BankName, BankResources, Total_Loan, CustomerInfo)
  end.