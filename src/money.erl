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
  CustomersDoneList = [],
  % Spawn money process
  MasterPID = spawn(fun() -> master_process(CustomerInfoTerms, CustomersDoneList) end),

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

master_process(CustomerInfo, CustomersDoneList) ->
  receive
    %Customer message
    {process_customer, Pid, Msg} -> % Include Pid in the pattern
      {Name, LoanNeeded, BankName} = Msg,
      %io:format("[MASTER FEEDBACK CUSTOMER] ~s requested a loan of $~B. from bank: ~p~n", [Name, LoanNeeded, BankName]),
      io:fwrite("? ~s requests a loan of ~B dollar(s) from the ~s bank~n", [Name, LoanNeeded, BankName]),
      master_process(CustomerInfo, CustomersDoneList);

    {process_customer_rejected, Pid, Msg} -> % Include Pid in the pattern
      {Name, LoanNeeded, RejectedBankName} = Msg,
      io:format("[MASTER FEEDBACK CUSTOMER] ~s rejected a loan request of $~B from customer: ~p~n", [RejectedBankName, LoanNeeded, Name]),
      master_process(CustomerInfo, CustomersDoneList);

    {customer_done, Pid, Msg} ->
      {Name, AmountTakenSoFar, OriginalObjective} = Msg,
      UpdatedCustomers = [Msg | CustomersDoneList],
      io:fwrite("~p IS THE CURRENT UPDATED CUSTOMERS.~n", [UpdatedCustomers]),
      io:format("~p IS THE CURRENT UPDATED CUSTOMERS COUNT.~n", [length(UpdatedCustomers)]),
      io:format("~p IS THE CURRENT UPDATED CUSTOMERSINFO COUNT.~n", [length(CustomerInfo)]),

      case length(UpdatedCustomers) == length(CustomerInfo) of
        true ->
          io:format("SIMULATION DONE!~n"),
          generate_report(UpdatedCustomers),
          ok;
        false ->
          master_process(CustomerInfo, UpdatedCustomers)
      end;



    %Bank Message
    {process_bank, Pid, Msg} ->
      {CustomerName, NeededLoanAmount, BankName} = Msg,
      %io:format("[MASTER FEEDBACK BANK] The ~s bank granted amount of $~B. to customer: ~p~n", [BankName, NeededLoanAmount, CustomerName]),
      io:format("$ The ~s bank approves a loan of $~B to ~p~n", [BankName, NeededLoanAmount, CustomerName]),
      master_process(CustomerInfo, CustomersDoneList);
    {process_bank_rejected, Pid, Msg} ->
      {CustomerName, NeededLoanAmount, BankName} = Msg,
      %io:format("[MASTER FEEDBACK BANK] The ~s bank denied amount of $~B. to customer: ~p~n", [BankName, NeededLoanAmount, CustomerName]),
      io:format("$ The ~s bank denies a loan of $~B to ~p~n", [BankName, NeededLoanAmount, CustomerName]),
      master_process(CustomerInfo, CustomersDoneList)

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


generate_report(CustomerDataList) ->
  io:fwrite("~p DATALIST.~n", [CustomerDataList]),
  {TotalObjective, TotalReceived, OriginalObjective} = calculate_totals(CustomerDataList),
  print_report(CustomerDataList, TotalObjective, TotalReceived, OriginalObjective).

calculate_totals(CustomerDataList) ->
  lists:foldl(
    fun({_, AmountTakenSoFar, OriginalObjective}, {TotalObjective, TotalReceived, TotalOriginalObjective}) ->
      {TotalObjective + AmountTakenSoFar, TotalReceived + AmountTakenSoFar, TotalOriginalObjective + OriginalObjective}
    end,
    {0, 0, 0},
    CustomerDataList
  ).

print_report(CustomerDataList, TotalObjective, TotalReceived, OriginalObjective) ->
  io:format("Customers:~n"),
  print_customers(CustomerDataList),
  io:format("-----~n"),
  io:format("Total: objective ~B, received ~B~n", [OriginalObjective, TotalObjective]).

print_customers([]) ->
  ok;
print_customers([{Name, AmountTakenSoFar, OriginalObjective} | Rest]) ->
  io:format("~s: objective ~B, received ~B~n", [Name, OriginalObjective, AmountTakenSoFar]),
  print_customers(Rest).