-module(dev_weavedb).
-export([ compute/3, init/3, snapshot/3, normalize/3, query/3, start/3, is_started/1, get_message/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(STATUS_TIMEOUT, 100).

get_message(Msg1, Msg2, Opts) ->
    ProcID = hb_ao:get(<<"pid">>, Msg2, Opts),
    Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
    CacheKey = <<"bundler-msg-", (hb_util:human_id(ProcID))/binary, "-", (hb_util:bin(Slot))/binary>>,
    io:format("GET_MSG: Looking for key: ~p~n", [CacheKey]),
    case hb_cache:read(CacheKey, Opts) of
        {ok, BundlerMsgBinary} when is_binary(BundlerMsgBinary) -> 
            io:format("FOUND IN BUNDLER CACHE~n"),
						% Deserialize the term
            BundlerMsg = binary_to_term(BundlerMsgBinary),
            io:format("Msg: ~p~n", [BundlerMsg]),
            {ok, BundlerMsg};
        not_found -> 
            io:format("NOT FOUND IN BUNDLER CACHE, using scheduler cache~n"),
            dev_scheduler_cache:read(ProcID, Slot, Opts)
    end.

request_cu(ProcID, Slot, Body, AOS2, Opts)->
    hb_ao:resolve(
      #{
	<<"device">> => <<"relay@1.0">>,
	<<"content-type">> => <<"application/json">>
       },
      AOS2#{
	    <<"path">> => <<"call">>,
	    <<"relay-method">> => <<"POST">>,
	    <<"relay-body">> => Body,
	    <<"relay-path">> =>
		<< "/weavedb/", (hb_util:bin(Slot))/binary, "?process-id=", ProcID/binary >>,
	    <<"content-type">> => <<"application/json">>
	   },
      Opts#{
	    hashpath => ignore,
	    cache_control => [<<"no-store">>, <<"no-cache">>]
	   }
     ).

get_data(Res, Opts)->
    ResultJSON = hb_ao:get(<<"body">>, Res, Opts),
    Data = try dev_codec_json:from(ResultJSON) of
	       Result -> dev_codec_json:from(ResultJSON),
			 case Result of
			     #{ <<"Output">> := #{ <<"data">> := D } } -> D;
			     #{ <<"data">> := D } -> D;
			     DirectData -> DirectData
			 end
	   catch
	       _Err -> #{}
	   end.

get_result(Res, ID, Msg2, Opts) ->
    Data = get_data(Res, Opts),
    FromProcess = case hb_ao:get([<<"body">>, <<"from-process">>], Msg2, not_found, Opts) of
		      not_found -> hb_ao:get(<<"from-process">>, Msg2, not_found, Opts);
		      FoundFrom -> FoundFrom
		  end,
    Reference = case hb_ao:get([<<"body">>, <<"reference">>], Msg2, not_found, Opts) of
		    not_found -> hb_ao:get(<<"reference">>, Msg2, not_found, Opts);
		    FoundRef -> FoundRef
		end,
    case {FromProcess, Reference} of
	{not_found, _} -> #{ <<"data">> => Data };
	{_, not_found} ->  #{ <<"data">> => Data };
	{FromProc, Ref} ->
	    Outbox = #{
		       <<"target">> => FromProc,
		       <<"data">> => Data,
		       <<"x-reference">> => Ref,
		       <<"type">> => <<"Message">>,
		       <<"from-db">> => ID
		      },

	    #{ 
	       <<"data">> => Data,
	       <<"outbox">> => #{ <<"1">> => Outbox }
	     }
    end.

compute(Msg1, Msg2, Opts) ->
    Action = hb_ao:get([<<"body">>, <<"Action">>], Msg2, Opts),
    Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
    ProcID = hb_ao:get(<<"process">>, Msg2, Opts),
    ID = hb_ao:get(<<"db">>, Msg1, Opts),
    case Action of
        <<"Query">> ->
            {ok, AOS2 = #{ <<"body">> := Body }} =
                dev_scheduler_formats:assignments_to_aos2(
		  ProcID,
		  #{ Slot => Msg2 },
		  false,
		  Opts
		 ),
            {ok, Res} = request_cu(ProcID, Slot, Body, AOS2, Opts),
	    Results = get_result(Res, ID, Msg2, Opts),
            {ok, hb_ao:set(Msg1, #{ <<"results">> => Results }, Opts)};
	
        <<"Commit">> ->
	    Body = hb_ao:get(<<"body">>, Msg2, Opts),
            ZKHash = hb_ao:get([<<"body">>,<<"zkhash">>], Msg2, Opts),
	    {ok, Res} = request_cu(ProcID, Slot, Body, Msg2, Opts),
	    Results = get_result(Res, ID, Msg2, Opts),
	    {ok, hb_ao:set(Msg1, #{ <<"zkhash">> => ZKHash, <<"results">> => Results}, Opts)};

        _Other ->
	    Body = hb_ao:get(<<"body">>, Msg2, Opts),
	    {ok, Res} = request_cu(ProcID, Slot, Body, Msg2, Opts),
	    Results = get_result(Res, ID, Msg2, Opts),
	    {ok, hb_ao:set(Msg1, #{ <<"results">> => Results}, Opts)}
    end.

init(Msg, _Msg2, Opts) -> 
    DB = hb_ao:get([<<"process">>,<<"db">>], Msg, Opts),    
    {ok, hb_ao:set(Msg, #{ <<"db">> => DB, <<"zkhash">> => "0" }, Opts)}.

snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.

normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

query(_M1, M2, _Opts) ->
    A = maps:get(<<"a">>, M2),
    B = maps:get(<<"b">>, M2),
    {ok, Sum} = dev_weavedb_nif:query(A, B),
    {ok, #{ <<"sum">> => Sum }}.

start(_, _, Opts) ->
    JSON = dev_codec_json:to(#{ <<"status">> => ensure_started(Opts)}),
    {ok, #{ <<"content-type">> => <<"application/json">>, <<"body">> => JSON }}.

is_started(Opts) ->
    Status = status(Opts),
    JSON = dev_codec_json:to(#{ <<"status">> => Status}),
    {ok, #{ <<"content-type">> => <<"application/json">>, <<"body">> => JSON }}.

ensure_started(Opts) ->
    ?event({ensure_started, weavedb, self()}),
    IsRunning = is_weavedb_server_running(Opts),
    IsCompiled = hb_features:weavedb(),
    GenWDBProc = is_pid(hb_name:lookup(<<"weavedb@1.0">>)),
    case IsRunning orelse (IsCompiled andalso GenWDBProc) of
        true ->
            true;
        false ->
            PID =
                spawn(
		  fun() ->
			  ?event({weavedb_booting, {pid, self()}}),
			  NodeURL =
			      "http://localhost:" ++
			      integer_to_list(hb_opts:get(port, no_port, Opts)),
			  Port =
			      open_port(
                                {spawn_executable,
				 "_build/weavedb-server/launch-monitored.sh"
                                },
                                [
				 binary, use_stdio, stderr_to_stdout,
				 {args, [
					 "npm",
					 "--prefix",
					 "_build/weavedb-server",
					 "run",
					 "dev"
					]},
				 {env,
				  [
				  ]
				 }
                                ]
			       ),
			  ?event({weavedb_port_opened, {port, Port}}),
			  collect_events(Port)
		  end
		 ),
            hb_name:register(<<"weavedb@1.0">>, PID),
            ?event({weavedb_starting, {pid, PID}}),
            hb_util:until(
	      fun() ->
		      receive after 2000 -> ok end,
		      Status = is_weavedb_server_running(Opts),
		      ?event({weavedb_boot_wait, {received_status, Status}}),
		      Status
	      end
	     ),
            ?event({weavedb_started, {pid, PID}}),
            true
    end.

is_weavedb_server_running(Opts) ->
    case get(weavedb_pid) of
        undefined ->
            ?event(weavedb_pinging_server),
            Parent = self(),
            PID = spawn(
		    fun() ->
			    ?event({weavedb_get_info_endpoint, {worker, self()}}),
			    Parent ! {ok, self(), status(Opts)}
		    end
		   ),
            receive
                {ok, PID, Status} ->
                    put(weavedb_pid, Status),
                    ?event({weavedb_received_status, Status}),
                    Status
            after ?STATUS_TIMEOUT ->
		    ?event({weavedb_status_check, timeout}),
		    erlang:exit(PID, kill),
		    false
            end;
        _ -> true
    end.

status(Opts) ->
    ServerPort = hb_opts:get(weavedb_port, 6364, Opts),
    Peer = <<"http://localhost:", (integer_to_binary(ServerPort))/binary>>,
    Path = <<"/status">>,
    Args = #{
	     peer => Peer,
	     path => Path,
	     method => <<"GET">>,
	     headers => #{},
	     body => <<>>
	    },
    try hb_http_client:req(Args, Opts) of
        {ok, 200, _Headers, _Body} ->
            ?event({weavedb_status_check, {status, 200}}),
            true;
        {ok, Status, _Headers, _Body} ->
            ?event({weavedb_status_check, {err, Status}}),
            false;
        Err ->
            ?event({weavedb_status_check, {err, Err}}),
            false
    catch
        _:Err ->
            ?event({weavedb_status_check, {error, Err}}),
            false
    end.


collect_events(Port) ->
    collect_events(Port, <<>>).
collect_events(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_events(Port,
			   log_server_events(<<Acc/binary, Data/binary>>)
			  );
        stop ->
            port_close(Port),
            ?event(weavedb_stopped, {pid, self()}),
            ok
    end.

log_server_events(Bin) when is_binary(Bin) ->
    log_server_events(binary:split(Bin, <<"\n">>, [global]));
log_server_events([Remaining]) -> Remaining;
log_server_events([Line | Rest]) ->
    ?event(weavedb_server, {server_logged, Line}),
    log_server_events(Rest).
