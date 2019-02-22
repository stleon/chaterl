%%%-------------------------------------------------------------------
%%% @author Lev Tonkikh <leonst998@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chat_storage_session).

-behaviour(gen_server).

%% API
-export([start_link/0, create/0, read/1, list/0, delete/0, delete/1, exists/1]).

-export([delete_old/0, start_timer/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("common.hrl").
-include("log.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
create() ->
    gen_server:call(?MODULE, create).

read(SessionID) when is_integer(SessionID) ->
    gen_server:call(?MODULE, {read, SessionID}).

list() ->
    gen_server:call(?MODULE, list).

exists(SessionID) when is_integer(SessionID) ->
    case read(SessionID) of
        Session when is_record(Session, ?CHAT_SESSION) ->
            true;
        not_found ->
            false;
        error ->
            false
    end.

delete() ->
    gen_server:cast(?MODULE, delete).

delete(SessionID) when is_integer(SessionID) ->
    gen_server:cast(?MODULE, {delete, SessionID}).

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
    {ok, #state{}}.

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
handle_call({read, SessionID}, _From, State) when is_integer(SessionID) ->
    F = fun() ->
                mnesia:read(?CHAT_SESSION, SessionID)
        end,

    Result = case mnesia:transaction(F) of
                 {atomic, [Session]} ->
                     Session;
                 {atomic, []} ->
                     not_found;
                 {aborted, Reason} ->
                     ?ERR("Can not read session: ~p. Reason ~p", [SessionID, Reason]),
                     error
             end,

    {reply, Result, State};

handle_call(list, _From, State) ->

    MatchSpec = [
                 {#?CHAT_SESSION{_='_'},
                  [],
                  ['$_']}
                ],

    F = fun() ->
                mnesia:select(?CHAT_SESSION, MatchSpec)
        end,

    Result = case mnesia:transaction(F) of
                 {atomic, Sessions} ->
                     Sessions;
                 {aborted, Reason} ->
                     ?ERR("Can not read sessions. Reason ~p", [Reason]),
                     error
             end,

    {reply, Result, State};

handle_call(create, _From, State) ->
    SessionID = erlang:phash2({node(), erlang:unique_integer()}),
    Expire    = erlang:system_time(millisecond) + ?SESSION_COOKIE_EXPIRE,
    Session   = #?CHAT_SESSION{
                    session_id  = SessionID,
                    expire_date = Expire
                   },
    F = fun() ->
                mnesia:write(Session)
        end,

    Result = case mnesia:transaction(F) of
                 {atomic, ok} ->
                     erlang:send_after(?SESSION_COOKIE_EXPIRE, self(), {'$gen_cast', {delete, SessionID}}),
                     Session;
                 {aborted, Reason} ->
                     ?ERR("Can not create session: ~p. Reason ~p", [Session, Reason]),
                     error
             end,

    {reply, Result, State};

handle_call(Request, _From, State) ->
    ?ERR("Unhandled request: ~p", [Request]),
    {reply, ok, State}.

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
handle_cast(delete, State) ->
    case mnesia:clear_table(?CHAT_SESSION) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            ?ERR("Can not delete sessions: ~p", [Reason])
    end,
    {noreply, State};

handle_cast({delete, SessionID}, State) when is_integer(SessionID) ->
    F = fun() ->
                mnesia:delete({?CHAT_SESSION, SessionID})
        end,

    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            ?ERR("Can not delete session: ~p. Reason: ~p", [SessionID, Reason])
    end,

    {noreply, State};


handle_cast(Msg, State) ->
    ?ERR("Unhandled message: ~p", [Msg]),
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
handle_info(Info, State) ->
    ?ERR("Unhandled info: ~p", [Info]),
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
delete_old() ->
    Now = erlang:system_time(millisecond),

    MatchSpec = [
                 {#?CHAT_SESSION{session_id  = '$1',
                                 expire_date = '$2'},
                  [{'<', '$2', Now}],
                  ['$_']} %% @todo get session_id and use delete/1
                ],

    F = fun() ->
                case mnesia:select(?CHAT_SESSION, MatchSpec) of
                    [] ->
                        ok;
                    Sessions when is_list(Sessions) ->
                        Fun = fun(S) ->
                                      mnesia:delete_object(S)
                              end,
                        lists:foreach(Fun, Sessions)
                end

        end,

    Result = case mnesia:transaction(F) of
                 {atomic, ok} ->
                     ok;
                 {aborted, Reason} ->
                     ?ERR("Can not delete old sessions. Reason ~p", [Reason]),
                     error
             end,

    Result.

start_timer() ->
    Now = erlang:system_time(millisecond),
    MatchSpec = [
                 {#?CHAT_SESSION{session_id  = '$1',
                                 expire_date = '$2'},
                  [{'>', '$2', Now}],
                  ['$$']}
                ],

    F = fun() ->
                mnesia:select(?CHAT_SESSION, MatchSpec)
        end,

    Result = case mnesia:transaction(F) of
                 {atomic, Sessions} ->
                     Fun = fun([SessionID, ExpireDate]) when is_integer(ExpireDate) ->
                                   Timer = ExpireDate - Now,
                                   ?INFO("For session (ID ~p) set timer to ~p", [SessionID, Timer]),
                                   erlang:send_after(Timer, self(), {'$gen_cast', {delete, SessionID}})
                           end,
                     lists:foreach(Fun, Sessions),
                     ok;
                 {aborted, Reason} ->
                     ?ERR("Can not start timer for old sessions. Reason ~p", [Reason]),
                     error
             end,

    Result.
