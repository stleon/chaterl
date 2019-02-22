%%%-------------------------------------------------------------------
%% @author Lev Tonkikh <leonst998@gmail.com>
%% @doc chat public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_phase/3]).

-include("common.hrl").
-include("log.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    chat_sup:start_link().


%%====================================================================
%% Start Phases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% This phase need to start mnesia, delete old sessions
%% & start timer for not expired sessions
%% @end
%%--------------------------------------------------------------------
start_phase(init, _StartType, _Args) ->

    {ok, MnesiaBaseDir} = application:get_env(chat, mnesia_base_dir),
    NodeDir             = MnesiaBaseDir ++ node(),

    ok = filelib:ensure_dir(NodeDir),
    application:set_env(mnesia, dir, NodeDir),

    case mnesia:create_schema([node()]) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok;
        {error, E} ->
            ?ERR("failed to create Mnesia schema: ~p", [E])
    end,

    ok = mnesia:start(),

    F = fun(Table, Attrs) when is_atom(Table), is_list(Attrs) ->
                ?DBG("Creating table: ~p", [Table]),
                case mnesia:create_table(Table, Attrs) of
                    {atomic, ok}                    -> ok;
                    {aborted, {already_exists, _}}  -> ok;
                    Error                           ->
                        ?ERR("Got error on create table: ~p", [Error])
                end
        end,

    [F(Table, Attrs) || {Table, Attrs} <- ?CHAT_TABLES],

    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),

    %% @todo in future use 1 transaction & 1 api call
    chat_storage_session:delete_old(),
    chat_storage_session:start_timer(),
    ok;

%%--------------------------------------------------------------------
%% @doc
%% This phase in future will be check status of 3d party services
%% @end
%%--------------------------------------------------------------------
start_phase(admin, _StartType, _Args) ->
    ok;


%%--------------------------------------------------------------------
%% @doc
%% In this phase we start web-server
%% @end
%%--------------------------------------------------------------------
start_phase(oper, _StartType, _Args) ->
   Dispatch = cowboy_router:compile([
        {'_', [
               {"/ws/",             chat_handler,  []},
               {"/public_channel/", chat_public_handler, []},
               {"/",                cowboy_static, {priv_file, chat, "static/index.html"}},
               {"/[...]",           cowboy_static, {priv_dir,  chat, "static"}}
              ]}
    ]),

    {ok, _} = cowboy:start_clear(chat_ws,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    ok = pg2:create(?PUBLIC_CHANNEL);


start_phase(StartPhase, StartType, Args) ->
    ?ERR("~p ~p ~p", [StartPhase, StartType, Args]).


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
