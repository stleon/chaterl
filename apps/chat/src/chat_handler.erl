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

-record(state, {
          receiver  :: pid()
         }).

init(Req, State) ->
    Timeout = 30 * 1000,
    {cowboy_websocket, Req, State, #{idle_timeout => Timeout}}.


websocket_init(State) ->
    chat_roulette:register_client(self()),
    {reply, {text, <<"Hello, world!">>}, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, << "said: ", Msg/binary >>}, State};

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({chat, Pid}, State) when is_pid(Pid) ->
    {reply, {text, <<"Got companion!">>}, State};

websocket_info(_Info, State) ->
    {ok, State}.
