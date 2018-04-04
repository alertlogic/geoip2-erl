-module(geoip2).

-behaviour(gen_server).

%% API functions
-export([
    find/2,
    find/3,
    get/2,
    get/3,
    get/4,
    lookup/2,
    restart/1,
    start/3,
    start/2,
    start_link/3,
    stop/1,
    server_name/1
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    port    :: port() | undefined,
    cache   :: map() | undefined,
    options :: list()
}).

-spec lookup(atom(), string() | binary() | ntoa:ip_address()) ->
    {ok, map()} | {error, term()}.
lookup(Name, IP) ->
    case ip_to_binary(IP) of
        {ok, BIP}   -> gen_server:call(server_name(Name), {lookup, BIP});
        Error       -> Error
    end.

-spec start(atom(), string()) ->
    ok | {error, term()}.
start(Name, Path) ->
    start(Name, Path, []).

-spec start(atom(), string(), list()) ->
    ok | {error, term()}.
start(Name, Path, Options) ->
    geoip2_sup:add_child(Name, Path, Options).

-spec stop(atom()) ->
    ok.
stop(Name) ->
    geoip2_sup:remove_child(Name).

-spec restart(atom()) ->
    ok | {error, term()}.
restart(Name) ->
    geoip2_sup:restart_child(Name).

find(Path, Map) when is_map(Map) ->
    do_find(Path, Map).

find(Name, Path, IP) ->
    case lookup(Name, IP) of
        {ok, Map} -> do_find(Path, Map);
        Error -> Error
    end.

get(Path, Map) when is_map(Map) ->
    case find(Path, Map) of
        {ok, Value} -> Value;
        _ -> error(badarg)
    end.

get(Path, Map, Default) when is_map(Map) ->
    case find(Path, Map) of
        {ok, Value} -> Value;
        _ -> Default
    end;

get(Name, Path, IP) ->
    case find(Name, Path, IP) of
        {ok, Value} -> Value;
        _ -> error(badarg)
    end.

get(Name, Path, IP, Default) ->
    case find(Name, Path, IP) of
        {ok, Value} -> Value;
        _ -> Default
    end.

-spec start_link(atom(), string(), list()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Name, Path, Options) ->
    gen_server:start_link({local, server_name(Name)}, ?MODULE, [Path, Options], []).

server_name(Name) ->
    list_to_atom("geoip2_svr_" ++ atom_to_list(Name)).

init([DbPath, Options]) ->
    Cache = case proplists:get_value(cache, Options, true) of
        true    -> #{};
        _       -> undefined
    end,
    Exec = application:get_env(geoip2, portcmd, filename:join([code:priv_dir(geoip2), "geoip2erl"])),
    Port = open_port({spawn, Exec}, [{packet, 4}, binary, exit_status]),
    try
        port_command(Port, DbPath),
        ok = recv(Port),
        {ok, #state{port = Port, cache = Cache, options = Options}}
    catch error:Error ->
        error_logger:error_msg("Error occured, port closed: ~p", [Error]),
        catch port_close(Port),
        {ok, #state{port = undefined, cache = Cache, options = Options}}
    end.

handle_call({lookup, _}, _From, #state{ port = undefined } = State) ->
    {reply, {error, port_closed}, State};

handle_call({lookup, IP}, _From, #state{ port = Port, cache = Cache } = State) when is_binary(IP)->
    case Cache of
        #{IP := Reply} -> {reply, Reply, State};
        _ ->
            try
                port_command(Port, IP),
                Reply = recv(Port),
                NewCache = case Cache of
                    undefined   -> undefined;
                    _           -> Cache#{IP => Reply}
                end,
                {reply, Reply, State#state{cache = NewCache}}
            catch error:Error ->
                error_logger:error_msg("Error occured, port closed: ~p", [Error]),
                catch port_close(Port),
                {reply, {error, port_closed}, State}
            end
    end;

handle_call(_, _, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, _Status}}, #state{ port = Port } = State) ->
    {noreply, State#state{ port = undefined }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ port = Port }) ->
    if is_port(Port) -> 
           catch port_close(Port);
       true ->
           ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


ip_to_binary(IP) when is_binary(IP) -> 
    {ok, IP};
ip_to_binary(IP) when is_list(IP) -> 
    {ok, iolist_to_binary(IP)};
ip_to_binary(IP) when is_tuple(IP) -> 
    case inet:ntoa(IP) of
        {error, _} = E -> E;
        Value -> {ok, iolist_to_binary(Value)}
    end;
ip_to_binary(_) -> 
    {error, badarg}.


recv(Port) ->
    receive
        {Port, {data, Bin}} -> 
            binary_to_term(Bin);
        {Port, _} = Msg ->
            self ! Msg,
            {error, unknown}
    after 5000 ->
        exit(timeout)
    end.

do_find(Path, Source) when erlang:is_binary(Path) ->
    do_find(binary:split(Path, <<"/">>, [global]), Source);
do_find([], _) -> error;
do_find(['_'], List) when is_list(List) -> {ok, List};
do_find(['_'|Rest], List) when is_list(List) -> 
    Result = lists:filtermap(fun(Element) ->
        case do_find(Rest, Element) of
            error -> false;
            {ok, Value} -> {true, Value}
        end
    end, List),
    {ok, Result};
do_find([Key|Rest], List) when is_list(List) ->
    case is_integer(Key) andalso Key >= 0 andalso Key < length(List) of
    true  -> 
        Value = lists:nth(Key + 1, List),
        case Rest of
            [] -> {ok, Value};
            _  -> do_find(Rest, Value)
        end;
    false -> 
        error
    end;
do_find([Key|Rest], Map) when is_map(Map) ->
    BinKey = if is_binary(Key) -> Key;
                is_atom(Key)   -> atom_to_binary(Key, utf8);
                is_list(Key)   -> list_to_binary(Key)
             end,
    case maps:find(BinKey, Map) of
    {ok, Value} -> 
        case Rest of
            [] -> {ok, Value};
            _  -> do_find(Rest, Value)
        end;
    error -> 
        error
    end.
