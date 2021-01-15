-record(tlv,
{
    type ,              %% #tlv_type{}
    identifier  ::      integer(),
    length      ::      integer(),
    value       ::      binary() | integer() | list() %%  tlv list
}).
-record(tlv_type,
{
    name        ::      atom(),
    code        ::      integer()
}).

-define(OBJECT,                 #tlv_type{name = object         ,code = 0}).
-define(RESOURCE,               #tlv_type{name = resource       ,code = 1}).
-define(MULTIPLE,               #tlv_type{name = multiple       ,code = 2}).
-define(VALUE,                  #tlv_type{name = value          ,code = 3}).
