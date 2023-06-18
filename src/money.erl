-module(money).
-export([start/1]).

start(Args) ->
  CustomerFile = lists:nth(1, Args),
  BankFile = lists:nth(2, Args),
  {ok, CustomerInfo} = file:consult(CustomerFile),
  {ok, BankInfo} = file:consult(BankFile),
  % Print the file names
  io:fwrite("Customer file: ~s~n", [CustomerFile]),
  io:fwrite("Bank file: ~s~n", [BankFile]),
  % Read the file contents
  {ok, CustomerContent} = file:read_file(CustomerFile),
  {ok, BankContent} = file:read_file(BankFile),
  % Print the file contents
  io:fwrite("Customer file content:~n~s~n", [CustomerContent]),
  io:fwrite("Bank file content:~n~s~n", [BankContent]).