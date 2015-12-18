-ifndef (MAKESQL_HRL).
-define(MAKESQL_HRL, true).

-define(MAKE_QUERY_SELECT_TABLE(Table, Field, Key, MinKey, MaxKey),
    "SELECT "++Field++" FROM "++Table++" WHERE "++Key++" BETWEEN "++MinKey++" AND "++MaxKey).

-define(MAKE_QUERY_REPLACE_TABLE_FIELD_VALUE(Table, Field, Value),
    "REPLACE INTO "++Table++"("++Field++")"++" VALUES"++Value).

-define(MAKE_QUERY_UPDATE_TABLE_FIELD_VALUE(Table, Field, Value, UpdateField),
    "INSERT INTO "++Table++"("++Field++")"++" VALUES"++Value++" ON DUPLICATE KEY UPDATE "++UpdateField).

-endif.