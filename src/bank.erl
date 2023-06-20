-module(bank).

%% API
-export([process_bank/4]).

process_bank(MasterPID, Pid, Name, Loan_to_give) ->
  Msg = {Pid, Name, Loan_to_give},
  MasterPID ! {process_bank, Msg}.