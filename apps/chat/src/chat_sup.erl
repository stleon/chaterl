%%%-------------------------------------------------------------------
%% @author Lev Tonkikh <leonst998@gmail.com>
%% @doc chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), #{id       => I,
                          start    => {I, start_link, []},
                          restart  => permanent,
                          shutdown => 5000,
                          type     => Type,
                          modules  => [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags   = #{strategy  => one_for_one,
                   intensity => 1,
                   period    => 5},

    ChildSpecs = [
                  ?CHILD(chat,           worker),
                  ?CHILD(chat_roulette,  worker)
                 ],
    {ok, {SupFlags, ChildSpecs}}.



%%====================================================================
%% Internal functions
%%====================================================================
