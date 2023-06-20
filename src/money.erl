-module(money).
-export([start/1]).
-export([get_potential_banks/2]).

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

  % Spawn Customers
  spawn_customers(CustomerInfoTerms, BankInfoTerms, MasterPID),

  % Spawn Banks
  spawn_banks(BankInfoTerms, MasterPID).

%get_potential_banks(LoanNeeded, BankInfo) ->
%lists:filter(
%fun({_BankName, BankLoan}) ->
%BankLoan >= LoanNeeded
%end,
%BankInfo).

get_potential_banks(_LoanNeeded, BankInfo) ->
  BankInfo.

spawn_master_process(CustomerInfo, BankInfo) ->
  receive
    {process_customer, Msg} ->
      {Pid, Name, LoanNeeded, BankInfo} = Msg,
      process_customer_feedback(Pid, Name, LoanNeeded, BankInfo),
      spawn_master_process(CustomerInfo, BankInfo);
    {process_bank, Msg} ->
      {Pid, Name, Lending_amount} = Msg,
      process_bank_feedback(Pid, Name, Lending_amount),
      spawn_master_process(CustomerInfo, BankInfo)
  end.

spawn_customers(CustomerInfo, BankInfo, MasterPID) ->
  lists:foreach(
    fun({Name, LoanNeeded}) ->
      spawn(customer, process_customer, [MasterPID, self(), Name, LoanNeeded, BankInfo]),
      timer:sleep(200)
    end,
    CustomerInfo).

spawn_banks(BankInfo, MasterPID) ->
  lists:foreach(
    fun({Name, Lending_amount}) ->
      spawn(bank, process_bank, [MasterPID, self(), Name, Lending_amount])
      %timer:sleep(200)
    end,
    BankInfo).


process_customer_feedback(Pid, Name, LoanNeeded, BankInfo) ->
  Feedback = io_lib:format("~s needs a loan of ~B. Potential banks: ~p~n", [Name, LoanNeeded, BankInfo]),
  io:fwrite(Feedback),
  Pid ! {completed}.

process_bank_feedback(Pid, Name, Lending_amount) ->
  Feedback = io_lib:format("~s can lend the amount of ~B~n", [Name, Lending_amount]),
  io:fwrite(Feedback),
  Pid ! {completed}.