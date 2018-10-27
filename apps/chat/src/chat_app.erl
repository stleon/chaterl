%%%-------------------------------------------------------------------
%% @author Lev Tonkikh <leonst998@gmail.com>
%% @doc chat public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/ws/",   chat_handler,  []},
               {"/",      cowboy_static, {priv_file, chat, "static/index.html"}},
               {"/[...]", cowboy_static, {priv_dir,  chat, "static"}}
              ]}
    ]),

    {ok, _} = cowboy:start_clear(chat_ws,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    chat_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
