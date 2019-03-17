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

-include("common.hrl").
-include("log.hrl").


-define(CONNECTION_OPTS, #{idle_timeout   => ?IDLE_TIMEOUT,
                           max_frame_size => ?MAX_FRAME_SIZE}
).

-record(state, {
          receiver  :: pid()
         }).

%%--------------------------------------------------------------------
%% @doc
%% Get recapcha token and check it in google. if ok -- init ws
%% @end
%%--------------------------------------------------------------------
init(#{qs := <<"token=", Token/bitstring>>} = Req, State) ->

    ResponseBody = check_captcha(Token),

    case jiffy:decode(ResponseBody, [return_maps]) of
        #{<<"success">> := true} ->

            Cookies = cowboy_req:parse_cookies(Req),

            case lists:keyfind(?SESSION_COOKIE_NAME, 1, Cookies) of
                false ->
                    ?DBG("Req without session.."),

                    Session   = chat_storage_session:create(),
                    SessionID = erlang:integer_to_binary(Session#?CHAT_SESSION.session_id),
                    ?INFO("Create session (ID ~p)", [SessionID]),

                    Req1 = cowboy_req:set_resp_cookie(?SESSION_COOKIE_NAME, SessionID,
                                                      Req, req_opts()),
                    {cowboy_websocket, Req1, State, ?CONNECTION_OPTS};


                {_, SessionID} when is_binary(SessionID) ->
                    check_session(Req, SessionID, State)
            end;
        _ ->
            {ok, Req, State}
    end;

%%--------------------------------------------------------------------
%% @doc Request without token, so we check session cookie
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Req, State) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(?SESSION_COOKIE_NAME, 1, Cookies) of
        false ->
            %% no token, no cookie..
            {ok, Req, State};

        {_, SessionID} when is_binary(SessionID) ->
            check_session(Req, SessionID, State)
    end.

websocket_init(_State) ->
    ?INFO("New connection process: ~p", [self()]),
    chat_roulette:register_client(self()),

    %% Msg   = {msg, {text, <<"Hello, world!">>}},
    %% Reply = {binary, bert:encode(Msg)},
    %% {reply, Reply, #state{}}.

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Incoming message sends to receiver (from A to B)
%% @end
%%--------------------------------------------------------------------
websocket_handle({binary, Msg}, #state{receiver = Pid} = State) when is_pid(Pid),
                                                                     is_binary(Msg) ->

    Send = fun(Receiver, Message) ->
                   Receiver ! {chat, Message}
           end,

    case bert:decode(Msg) of
        {msg, {text, String}} when is_list(String) ->
            Send(Pid, Msg);
        {signal, 1, Bool} when Bool == true; Bool == false -> %% typing
            ?DBG("TYPING ~p", [Bool]),
            Send(Pid, Msg);
        X ->
            ?WRN("decode error ~p", [X])
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

    Signal = {signal, 0}, %% channel_created
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


%%%===================================================================
%%% Internal functions
%%%===================================================================
req_opts() ->
    {ok, Domain} = application:get_env(chat, domain),
    #{domain  => Domain,
      path    => "/",
      max_age => ?SESSION_COOKIE_EXPIRE div 1000 }.

%%--------------------------------------------------------------------
%% @doc Send token to google, if 200 return json in response body
%% @spec
%% @end
%%--------------------------------------------------------------------
check_captcha(Token) when is_bitstring(Token) ->

    {ok, SecretList} = application:get_env(chat, recapcha_secret),
    Secret           = list_to_bitstring(SecretList),

    Method      = post,
    Url         = "https://www.google.com/recaptcha/api/siteverify",
    Headers     = [],
    ContentType = "application/x-www-form-urlencoded; charset=utf-8",
    Body        = <<"secret=", Secret/bitstring, "&response=", Token/bitstring>>,

    Request = {Url, Headers, ContentType, Body},
    HTTPOpt = [],
    Options = [],

    {ok, Result} = httpc:request(Method, Request, HTTPOpt, Options),
    {{"HTTP/1.1", 200, "OK"}, _Headers, ResponseBody} = Result,
    ResponseBody.


%%--------------------------------------------------------------------
%% @doc check session in storage. Stop or init ws
%% @spec
%% @end
%%--------------------------------------------------------------------
check_session(Req, SessionID, State) when is_binary(SessionID) ->
    SessionID1 = list_to_integer(binary_to_list(SessionID)),
    ?DBG("Got session (ID ~p)", [SessionID1]),

    case chat_storage_session:exists(SessionID1) of
        true ->
            {cowboy_websocket, Req, State, ?CONNECTION_OPTS};
        false ->
            ?ERR("Unknown sesison (ID ~p)", [SessionID1]),
            ReqOpts1 = maps:update(max_age, 0, req_opts()),
            Req1     = cowboy_req:set_resp_cookie(?SESSION_COOKIE_NAME,
                                                  SessionID, Req, ReqOpts1),
            Req2 = cowboy_req:reply(400, Req1),

            %% @todo some clients do not delete cookies
            {ok, Req2, State}
    end.
