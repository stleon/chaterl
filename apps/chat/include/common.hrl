-ifndef(COMMON_HRL).
-define(COMMON_HRL, true).

-define(IDLE_TIMEOUT,          60 * 1000).
-define(MAX_FRAME_SIZE,        256000).
-define(PUBLIC_CHANNEL,        <<"public_channel">>).


-define(SESSION_COOKIE_EXPIRE, 24 * 60 * 60 * 1000).
-define(SESSION_COOKIE_NAME,   <<"sessionid">>).


-define(CHAT_SESSION,       chat_session).

-define(CHAT_TABLES, [
    {?CHAT_SESSION, [{type, set},
                        {disc_copies, [node()]},
                        {record_name, ?CHAT_SESSION},
                        {attributes, record_info(fields, ?CHAT_SESSION)}]}
                        ]).

-record(?CHAT_SESSION, {
           session_id     :: pos_integer(),
           expire_date    :: pos_integer(),
           data           :: binary()
}).

-endif.
