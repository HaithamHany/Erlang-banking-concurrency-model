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

  % Spawn customers
  spawn_customers(CustomerInfoTerms, BankInfoTerms),
  receive_feedback(length(CustomerInfoTerms)).

%get_potential_banks(LoanNeeded, BankInfo) ->
  %lists:filter(
    %fun({_BankName, BankLoan}) ->
      %BankLoan >= LoanNeeded
    %end,
    %BankInfo).

get_potential_banks(_LoanNeeded, BankInfo) ->
  BankInfo.

spawn_customers(CustomerInfo, BankInfo) ->
  lists:foreach(
    fun({Name, LoanNeeded}) ->
      spawn(customer, process_customer, [Name, LoanNeeded, BankInfo])
    end,
    CustomerInfo).

receive_feedback(0) ->  % All customers have completed
  ok;
receive_feedback(NumCustomers) ->
  receive
    {completed, Feedback} ->
      io:fwrite(Feedback),
      receive_feedback(NumCustomers - 1)
  end.