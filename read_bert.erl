%%% Load with 
%%% c:c(read_bert).

-module(read_bert).
-author('florian@florianluescher.ch').

-export([read_bert/1]).

read_bert(FName) -> 
	case file:read_file(FName) of
		{ok, Binary} -> erlang:binary_to_term(Binary);
		other -> "Nope"
	end.
