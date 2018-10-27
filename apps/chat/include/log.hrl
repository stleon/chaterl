-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-define(DBG(Format),              lager:debug(Format)).
-define(DBG(Format, Data),        lager:debug(Format, Data)).

-define(INFO(Format),             lager:info(Format)).
-define(INFO(Format, Data),       lager:info(Format, Data)).

-define(WRN(Format),              lager:warning(Format)).
-define(WRN(Format, Data),        lager:warning(Format, Data)).

-define(ERR(Format),              lager:error(Format)).
-define(ERR(Format, Data),        lager:error(Format, Data)).

-endif.

