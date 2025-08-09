-module(dev_weavedb_wal).
-export([snapshot/3, compute/3, init/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").


compute(Msg1, Msg2, Opts) ->
    case hb_ao:get([<<"body">>,<<"type">>], Msg2, Opts) of
	<<"Process">> ->
	    {ok,
	     hb_ao:set(
	       Msg1,
	       #{ <<"ok">> => true, <<"current">> => 0 },
	       Opts
	      )
	    };
	_Otehr ->
	    Data = hb_ao:get([<<"body">>,<<"data">>], Msg2, Opts),
	    JSON = dev_codec_json:from(Data),
	    LastHashpath = lists:foldl(fun(Item, _Acc) ->
                maps:get(<<"hashpath">>, Item)
            end, 0, JSON),
	    {ok,
	     hb_ao:set(
	       Msg1,
	       #{ <<"ok">> => true, <<"current">> => LastHashpath },
	       Opts
	      )
	    }
	end.		      

init(Msg, _Msg2, Opts) -> {ok, hb_ao:set(Msg, #{ }, Opts)}.

snapshot(_Msg1, _Msg2, _Opts) -> {ok, #{}}.

