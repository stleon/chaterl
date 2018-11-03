%%%-------------------------------------------------------------------
%%% @author Lev Tonkikh <leonst998@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chat_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("log.hrl").

-record(state, {
          receiver  :: pid()
         }).

init(Req, State) ->
    Timeout = 60 * 1000,
    {cowboy_websocket, Req, State, #{idle_timeout => Timeout}}.


websocket_init(_State) ->
    ?INFO("New connection process: ~p", [self()]),
    chat_roulette:register_client(self()),
    {reply, {text, <<"Hello, world!">>}, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Incoming message in room sends to receiver
%% @end
%%--------------------------------------------------------------------
websocket_handle({text, Msg}, #state{receiver = Pid} = State) when is_pid(Pid) ->
    Pid ! {chat, Msg},
    {ok, State};

%%--------------------------------------------------------------------
%% @doc
%% Incoming message ignoring, when no receiver
%% @end
%%--------------------------------------------------------------------
websocket_handle({text, Msg}, State) ->
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.


websocket_info({chat, Pid}, State) when is_pid(Pid) ->
    ?INFO("Got another process ~p in room", [Pid]),
    link(Pid),
    {reply, {text, <<"Got companion!">>}, State#state{receiver = Pid}};

%%--------------------------------------------------------------------
%% @doc
%% Receiver get message and send it to client
%% @end
%%--------------------------------------------------------------------
websocket_info({chat, Msg}, State) when is_binary(Msg) ->
    {reply, {text, << "said: ", Msg/binary >>}, State};

websocket_info({'EXIT', Pid, normal}, State) when is_pid(Pid) ->
    ?INFO("Room is closed, pid ~p went offline", [Pid]),
    {stop, State};

websocket_info(Info, State) ->
    ?ERR("Unhandled info: ~p", [Info]),
    {ok, State}.


terminate(Reason, _Req, _State) ->
    ?INFO("Terminate with reason: ~p", [Reason]),
    ok.
