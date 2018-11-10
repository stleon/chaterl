%%%-------------------------------------------------------------------
%%% @author Lev Tonkikh <leonst998@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chat_public_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("common.hrl").
-include("log.hrl").

-record(state, {
          group   :: bitstring(),
          id      :: binary()
}).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout   => ?IDLE_TIMEOUT,
                                     max_frame_size => ?MAX_FRAME_SIZE}}.


websocket_init(_State) ->
    ?INFO("New group connection process: ~p", [self()]),

    Rgb    = crypto:strong_rand_bytes(3),
    Signal = {signal, <<"join">>, Rgb},
    Msg    = bert:encode(Signal),

    send(?PUBLIC_CHANNEL, Msg),
    pg2:join(?PUBLIC_CHANNEL, self()),

    State = #state{group = ?PUBLIC_CHANNEL,
                   id    = Rgb},
    {ok, State}.


websocket_handle({text, Msg}, #state{group = Group,
                                     id = Rgb} = State) when is_bitstring(Group),
                                                             is_binary(Msg),
                                                             is_binary(Rgb) ->

    case bert:decode(Msg) of
        {msg, {text, Text}} when is_bitstring(Text) ->
            NewMsg = {msg, {text, Text}, {from, Rgb}},
            send(Group, bert:encode(NewMsg));
        {signal, <<"typing">>} ->
            NewMsg = {signal, <<"typing">>, Rgb},
            send(Group, bert:encode(NewMsg));
        _ ->
            ok
    end,

    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Receiver (B) get message and send it to client through ws
%% @end
%%--------------------------------------------------------------------
websocket_info({chat, Msg}, State) when is_binary(Msg) ->
    {reply, {binary, Msg}, State};

websocket_info(Info, State) ->
    ?ERR("Unhandled info: ~p", [Info]),
    {ok, State}.


terminate(Reason, _Req, #state{group = Group,
                               id = Rgb} = _State) when is_bitstring(Group),
                                                        is_binary(Rgb) ->
    ?INFO("Terminate with reason: ~p", [Reason]),

    Signal = {signal, <<"leave">>, Rgb},
    Msg    = bert:encode(Signal),

    send(?PUBLIC_CHANNEL, Msg),

    ok.

%%====================================================================
%% Internal functions
%%====================================================================
send(Group, Msg) when is_bitstring(Group), is_binary(Msg) ->
    case pg2:get_members(Group) of
        [] ->
            ok;
        [Pid] when Pid == self() ->
            ok;
        Members when is_list(Members) ->
            %% not send to self
            [Pid ! {chat, Msg} || Pid <- Members, Pid /= self()],
            ok
    end.

