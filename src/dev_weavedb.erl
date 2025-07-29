-module(dev_weavedb).
-export([ compute/3, init/3, snapshot/3, normalize/3, query/3 ]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(STATUS_TIMEOUT, 100).

compute(Msg1, Msg2, Opts) ->
    case hb_ao:get([<<"body">>,<<"Action">>], Msg2, Opts) of
	<<"Query">> ->
	    Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
	    ProcID = hb_ao:get(<<"process">>, Msg2, Opts),
	    {ok, AOS2 = #{ <<"body">> := Body }} =
		dev_scheduler_formats:assignments_to_aos2(
		  ProcID,
		  #{
		    Slot => Msg2
		   },
		  false,
		  Opts
		 ),
	    {ok, Res} = 
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
		 ),
	    ID = hb_ao:get(<<"db">>, Msg1, Opts),
	    ZKHash = hb_ao:get(<<"zkhash">>, Msg1, Opts),
	    Result = dev_codec_json:from(hb_ao:get(<<"body">>, Res, Opts)),
	    Data = hb_ao:get([<<"Output">>,<<"data">>], Result, Opts),
	    {ok, hb_ao:set( Msg1, #{ <<"db">> => ID, <<"zkhash">> => ZKHash, <<"results">> => #{ <<"data">> => Data } }, Opts )};
	Other ->
	    ID = hb_ao:get(<<"db">>, Msg1, Opts),
	    ZKHash = hb_ao:get([<<"body">>,<<"zkhash">>], Msg2, Opts),
	    {ok, hb_ao:set( Msg1, #{ <<"db">> => ID, <<"zkhash">> => ZKHash }, Opts )}
    end.

init(Msg, Msg2, Opts) -> 
    DB = hb_ao:get([<<"process">>,<<"db">>], Msg, Opts),    
    {ok, hb_ao:set(Msg, #{ <<"db">> => DB, <<"zkhash">> => "0" }, Opts)}.


snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.

normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

query(_M1, M2, _Opts) ->
    A = maps:get(<<"a">>, M2),
    B = maps:get(<<"b">>, M2),
    {ok, Sum} = dev_weavedb_nif:query(A, B),
    {ok, #{ <<"sum">> => Sum }}.

info(_, _, Opts) ->
    ensure_started(Opts),
    {ok, #{ <<"node">> => <<"HyperBEAM">> }}.

%% @doc Ensure the local `weavedb@1.0' is live. If it not, start it.
ensure_started(Opts) ->
    % Check if the `weavedb@1.0' device is already running. The presence
    % of the registered name implies its availability.
    ?event({ensure_started, weavedb, self()}),
    IsRunning = is_weavedb_server_running(Opts),
    IsCompiled = hb_features:weavedb(),
    GenWDBProc = is_pid(hb_name:lookup(<<"weavedb@1.0">>)),
    case IsRunning orelse (IsCompiled andalso GenWDBProc) of
        true ->
            % If it is, do nothing.
            true;
        false ->
            % The device is not running, so we need to start it.
            PID =
                spawn(
                    fun() ->
                        ?event({weavedb_booting, {pid, self()}}),
                        % Create weavedb cache dir, if it does not exist.
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
            % Wait for the device to start.
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

%% @doc Check if the weavedb server is running, using the cached process ID
%% if available.
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

%% @doc Check if the weavedb server is running by requesting its status
%% endpoint.
status(Opts) ->
    ServerPort =
        integer_to_binary(
            hb_opts:get(
                weavedb_port,
                6364,
                Opts
            )
        ),
    try hb_http:get(<<"http://localhost:", ServerPort/binary, "/status">>, Opts) of
        {ok, Res} ->
            ?event({weavedb_status_check, {res, Res}}),
            true;
        Err ->
            ?event({weavedb_status_check, {err, Err}}),
            false
    catch
        _:Err ->
            ?event({weavedb_status_check, {error, Err}}),
            false
    end.

%% @doc Collect events from the port and log them.
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

%% @doc Log lines of output from the weavedb server.
log_server_events(Bin) when is_binary(Bin) ->
    log_server_events(binary:split(Bin, <<"\n">>, [global]));
log_server_events([Remaining]) -> Remaining;
log_server_events([Line | Rest]) ->
    ?event(weavedb_server, {server_logged, Line}),
    log_server_events(Rest).

%%% Tests
resolve_add_test() ->
    M1 = #{ <<"device">> => <<"weavedb@1.0">> },
    M2 = #{ <<"path">> => <<"query">>, <<"a">> => 8, <<"b">> => 9 },
    {ok, #{ <<"sum">> := 17 }} = hb_ao:resolve(M1, M2, #{}).
