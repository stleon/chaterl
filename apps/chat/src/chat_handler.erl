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

    Msg   = {msg, {text, <<"Hello, world!">>}},
    Reply = {binary, bert:encode(Msg)},

    {reply, Reply, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Incoming message sends to receiver (from A to B)
%% @end
%%--------------------------------------------------------------------
websocket_handle({text, Msg}, #state{receiver = Pid} = State) when is_pid(Pid),
                                                                   is_binary(Msg) ->

    Send = fun(Receiver, Message) ->
                   Receiver ! {chat, Message}
           end,

    case bert:decode(Msg) of
        {msg, {text, Text}} when is_bitstring(Text) ->
            Send(Pid, Msg);
        {signal, <<"typing">>} ->
            Send(Pid, Msg);
        _ ->
            ok
    end,

    {ok, State};

%%--------------------------------------------------------------------
%% @doc
%% Incoming message ignoring, when no receiver
%% @end
%%--------------------------------------------------------------------
websocket_handle({text, _Msg}, State) ->
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Send channel_created signal, link processes (A & B)
%% @end
%%--------------------------------------------------------------------
websocket_info({chat, Pid}, State) when is_pid(Pid) ->
    ?INFO("Got another process ~p in channel", [Pid]),
    link(Pid),

    Signal = {signal, <<"channel_created">>},
    Reply  = {binary, bert:encode(Signal)},

    {reply, Reply, State#state{receiver = Pid}};

%%--------------------------------------------------------------------
%% @doc
%% Receiver (B) get message and send it to client through ws
%% @end
%%--------------------------------------------------------------------
websocket_info({chat, Msg}, State) when is_binary(Msg) ->
    {reply, {binary, Msg}, State};

websocket_info({'EXIT', Pid, normal}, State) when is_pid(Pid) ->
    ?INFO("Channel is closed, pid ~p went offline", [Pid]),
    {stop, State};

websocket_info(Info, State) ->
    ?ERR("Unhandled info: ~p", [Info]),
    {ok, State}.


terminate(Reason, _Req, _State) ->
    ?INFO("Terminate with reason: ~p", [Reason]),
    ok.
