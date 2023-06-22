-module(money).
-export([start/1]).


start(Args) ->
  CustomerFile = lists:nth(1, Args),
  BankFile = lists:nth(2, Args),
  {ok, CustomerInfoTerms} = file:consult(CustomerFile),
  {ok, BankInfoTerms} = file:consult(BankFile),

  % Print the file names
  %io:fwrite("Customer file: ~s~n", [CustomerFile]),
  %io:fwrite("Bank file: ~s~n", [BankFile]),

  % Spawn money process
  MasterPID = spawn(fun() -> master_process(CustomerInfoTerms) end),

  % Spawn Banks
  spawn_banks(BankInfoTerms, MasterPID, CustomerInfoTerms),

% Spawn Customers
  spawn_customers(CustomerInfoTerms, BankInfoTerms, MasterPID).

%get_potential_banks(LoanNeeded, BankInfo) ->
%lists:filter(
%fun({_BankName, BankLoan}) ->
%BankLoan >= LoanNeeded
%end,
%BankInfo).

master_process(CustomerInfo) ->
  receive
    %Customer message
    {process_customer, Pid, Msg} -> % Include Pid in the pattern
      {Name, LoanNeeded, BankName} = Msg,
      io:format("[MASTER FEEDBACK CUSTOMER] ~s requested a loan of $~B. from bank: ~p~n", [Name, LoanNeeded, BankName]),
      master_process(CustomerInfo);

    {process_customer_rejected, Pid, Msg} -> % Include Pid in the pattern
      {Name, LoanNeeded, RejectedBankName} = Msg,
      io:format("[MASTER FEEDBACK CUSTOMER] ~s rejected a loan request of $~B from customer: ~p~n", [RejectedBankName, LoanNeeded, Name]),
      master_process(CustomerInfo);

    %Bank Message
    {process_bank, Pid, Msg} ->
      {CustomerName, NeededLoanAmount, BankName} = Msg,
      io:format("[MASTER FEEDBACK BANK] The ~s bank granted amount of $~B. to customer: ~p~n", [BankName, NeededLoanAmount, CustomerName]),
      master_process(CustomerInfo);
    {process_bank_rejected, Pid, Msg} ->
      {CustomerName, NeededLoanAmount, BankName} = Msg,
      io:format("[MASTER FEEDBACK BANK] The ~s bank denied amount of $~B. to customer: ~p~n", [BankName, NeededLoanAmount, CustomerName]),
      master_process(CustomerInfo)

  end.

%Process Spawners
spawn_customers(CustomerInfo, BankInfo, MasterPID) ->
  lists:foreach(
    fun({Name, LoanNeeded}) ->
      CustomerPID = spawn(customer, process_customer, [MasterPID, Name, LoanNeeded, BankInfo]),
      register(Name, CustomerPID), % Register the bank process with its name
      %io:format("Registered customer process ~p with name ~p~n", [CustomerPID, Name]), % Print the registration information
      timer:sleep(200)
    end,
    CustomerInfo).

spawn_banks(BankInfo, MasterPID, CustomerInfo) ->
  lists:foreach(
    fun({Name, Lending_amount}) ->
      BankPID = spawn(bank, process_bank, [MasterPID, Name, Lending_amount, CustomerInfo]),
      register(Name, BankPID) % Register the bank process with its name
      %io:format("Registered bank process ~p with name ~p~n", [BankPID, Name]) % Print the registration information
    end,
    BankInfo).


