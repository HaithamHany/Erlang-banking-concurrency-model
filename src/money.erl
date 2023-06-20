-module(money).
-export([start/1]).


start(Args) ->
  CustomerFile = lists:nth(1, Args),
  BankFile = lists:nth(2, Args),
  {ok, CustomerInfoTerms} = file:consult(CustomerFile),
  {ok, BankInfoTerms} = file:consult(BankFile),

  % Print the file names
  io:fwrite("Customer file: ~s~n", [CustomerFile]),
  io:fwrite("Bank file: ~s~n", [BankFile]),

  % Spawn money process
  MasterPID = spawn(fun() -> spawn_master_process(CustomerInfoTerms, BankInfoTerms) end),

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

spawn_master_process(CustomerInfo, BankInfo) ->
  receive
    %Customer message
    {process_customer, Pid, Msg} -> % Include Pid in the pattern
      {Name, LoanNeeded, BankInfo} = Msg,
      process_customer_feedback(Pid, Name, LoanNeeded, BankInfo),
      spawn_master_process(CustomerInfo, BankInfo);

    %Bank Message
    {process_bank, Pid, Msg} ->
      {Name, Lending_amount} = Msg,
      process_bank_feedback(Pid, Name, Lending_amount),
      spawn_master_process(CustomerInfo, BankInfo)
  end.

%Process Spawners
spawn_customers(CustomerInfo, BankInfo, MasterPID) ->
  lists:foreach(
    fun({Name, LoanNeeded}) ->
      CustomerPID = spawn(customer, process_customer, [MasterPID, Name, LoanNeeded, BankInfo]),
      register(Name, CustomerPID), % Register the bank process with its name
      timer:sleep(200)
    end,
    CustomerInfo).

spawn_banks(BankInfo, MasterPID, CustomerInfo) ->
  lists:foreach(
    fun({Name, Lending_amount}) ->
      BankPID = spawn(bank, process_bank, [MasterPID, Name, Lending_amount, CustomerInfo]),
      register(Name, BankPID), % Register the bank process with its name
      io:format("Registered bank process ~p with name ~p~n", [BankPID, Name]) % Print the registration information
    end,
    BankInfo).


%FeedBack
process_customer_feedback(CustomerId, Name, LoanNeeded, BankInfo) ->
  Feedback = io_lib:format("~s needs a loan of ~B. Potential banks: ~p~n", [Name, LoanNeeded, BankInfo]),
  io:fwrite(Feedback).
  %CustomerId ! {completed, self()}. % Include self() in the completion message

process_bank_feedback(BankId, Name, Lending_amount) ->
  Feedback = io_lib:format("~s can lend the amount of ~B~n", [Name, Lending_amount]),
  io:fwrite(Feedback).
  %BankId ! {completed, self()}.