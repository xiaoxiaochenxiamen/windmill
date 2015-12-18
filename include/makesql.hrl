-ifndef (MAKESQL_HRL).
-define(MAKESQL_HRL, true).


-define(MAKE_QUERY_REPLACE_TABLE_FIELD_VALUE(Table, Field, Value),
    "REPLACE INTO "++Table++"("++Field++")"++" VALUES"++Value).

-define(MAKE_QUERY_UPDATE_TABLE_FIELD_VALUE(Table, Field, Value, UpdateField),
    "INSERT INTO "++Table++"("++Field++")"++" VALUES"++Value++" ON DUPLICATE KEY UPDATE "++UpdateField).

-endif.
