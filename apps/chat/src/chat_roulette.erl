%%%-------------------------------------------------------------------
%%% @author Lev Tonkikh <leonst998@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chat_roulette).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register_client/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          queue :: list()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% When client connects, we get his Pid and:
%% 0. if not another ClientPid in queue, save Pid
%% 1. if we have ClientPid in queue, so they can chat, clear queue
%% 2. if we have offline ClientPid in queue, clear queue, save Pid
%% @spec
%% @end
%%--------------------------------------------------------------------
register_client(Pid) when is_pid(Pid) ->
    gen_server:cast(?SERVER, {register_client, Pid}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{queue = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({register_client, Pid}, #state{queue=[]} = State) ->
    NewQueue = [Pid],
    {noreply, State#state{queue = NewQueue}};

handle_cast({register_client, Pid}, #state{queue=[Pid]} = State) ->
    {noreply, State};

handle_cast({register_client, Pid0},
            #state{queue=[Pid1]} = State) when is_pid(Pid1) ->
    NewQueue = [],

    %% send them pids & signal, that they can chat
    [ Sender ! {chat, Receiver} || {Sender, Receiver} <- [{Pid0, Pid1},
                                                          {Pid1, Pid0}]],

    {noreply, State#state{queue = NewQueue}};

handle_cast({register_client, Pid},
            #state{queue=[WaiterPid]} = State) when not is_pid(WaiterPid) ->
    NewQueue = [Pid],
    {noreply, State#state{queue = NewQueue}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
