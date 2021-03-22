-record(tlv,
{
    type        ::      integer(),
    identifier  ::      integer(),
    length      ::      integer() | undefined,
    value       ::      binary() | integer() | list() %%  tlv list
}).

-define(OBJECT      , 0).
-define(RESOURCE    , 1).
-define(MULTIPLE    , 2).
-define(VALUE       , 3).
