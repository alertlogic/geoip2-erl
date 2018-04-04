-module(geoip2_sup).

-behaviour(supervisor).

%% API functions
-export([
  add_child/3,
  remove_child/1,
  restart_child/1,
  start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-spec add_child(atom(), string(), list()) ->
  ok | {error, term()}.
add_child(Name, Path, Options) ->
  case supervisor:start_child(?MODULE, child({Name, Path, Options})) of
    {ok, _} -> ok;
    {error, already_present} ->
      remove_child(Name),
      add_child(Name, Path, Options);
    {error, {already_started, _}} ->
      remove_child(Name),
      add_child(Name, Path, Options);
    {error, _} = Error ->
      Error
  end.

-spec restart_child(atom()) ->
  ok | {error, term()}.
restart_child(Name) ->
  supervisor:terminate_child(?MODULE, Name),
  supervisor:restart_child(?MODULE, Name).

-spec remove_child(atom()) ->
  ok | {error, term()}.
remove_child(Name) ->
  supervisor:terminate_child(?MODULE, Name),
  supervisor:delete_child(?MODULE, Name).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% priv
child({Name, Path}) ->
  child({Name, Path, []});
child({Name, Path, Options}) ->
  Module = geoip2,
  #{
    id          => Name,
    start       => {Module, start_link, [Name, Path, Options]},
    restart     => permanent,
    shutdown    => 5000,
    type        => worker,
    modules     => [Module]
  }.

init([]) ->
  Instances = application:get_env(geoip2, instances, []),
  Children = lists:map(fun child/1, Instances),
  {ok, {{one_for_one, 5, 10}, Children}}.
