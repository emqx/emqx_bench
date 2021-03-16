-define( DEFAULT_COAP_PORT  , 5683  ).
-define( DEFAULT_COAPS_PORT , 5684  ).

-define( ACK_TIMEOUT        , 2000).
-define( MAX_RETRANSMIT     , 4).

-define( MAX_BLOCK_SIZE     , 1024  ).
-define( DEFAULT_MAX_AGE    , 60    ).

-define( VERSION            , 1     ).
%%  no_payload   =>      no 0xFF payload mark
%%  <<>>         =>      0xFF & empty payload
-define( NO_PAYLOAD         , no_payload).

-record(coap_message,
{
    type        ::      atom(),
    method ,            %% #method{}
    id = -1     ::      integer(),
    token       ::      binary(),
    options     ::      list(), %% [ #option{} ]
    %%  no_payload   =>      no 0xFF payload mark
    %%  <<>>         =>      0xFF & empty payload
    payload     ::      binary() | ?NO_PAYLOAD
}).

-record(method,
{
    name        ::      atom(),
    code        ::      integer(),
    detail      ::      integer()
}).
-record(option,
{
    name        ::      atom(),
    code        ::      integer(),
    value       ::      binary()
}).
%% type def
-define( CON        , con   ).
-define( NON        , non   ).
-define( ACK        , ack   ).
-define( RESET      , reset ).

%% option def
-define( RESERVED                       , #option{name = reserved                  , code = 0      }).
-define( IF_MATCH                       , #option{name = if_match                  , code = 1      }).
-define( URI_HOST                       , #option{name = uri_host                  , code = 3      }).
-define( ETAG                           , #option{name = etag                      , code = 4      }).
-define( IF_NOT_MATCH                   , #option{name = if_not_match              , code = 5      }).
-define( URI_OBSERVE                    , #option{name = uri_observe               , code = 6      }).
-define( URI_PORT                       , #option{name = uri_port                  , code = 7      }).
-define( LOCATION_PATH                  , #option{name = location_path             , code = 8      }).
-define( URI_PATH                       , #option{name = uri_path                  , code = 11     }).
-define( CONTENT_FORMAT                 , #option{name = content_format            , code = 12     }).
-define( MAX_AGE                        , #option{name = max_age                   , code = 14     }).
-define( URI_QUERY                      , #option{name = uri_query                 , code = 15     }).
-define( ACCEPT                         , #option{name = accept                    , code = 17     }).
-define( LOCATION_QUERY                 , #option{name = location_query            , code = 20     }).
-define( PROXY_URI                      , #option{name = proxy_uri                 , code = 35     }).
-define( PROXY_SCHEME                   , #option{name = proxy_scheme              , code = 39     }).
-define( SIZE1                          , #option{name = size1                     , code = 60     }).

%% content format def
-define( TEXT_PLAIN                     , 0).
-define( APPLICATION_LINK_FORMAT        , 40).
-define( APPLICATION_OCTET_STREAM       , 42).
-define( APPLICATION_VNDOMALWM2M_TLV    , 11542).
-define( APPLICATION_VNDOMALWM2M_JSON   , 11543).

%% method
-define( EMPTY                          , #method{name = empty                     , code = 0,     detail = 0  }).
-define( GET                            , #method{name = get                       , code = 0,     detail = 1  }).
-define( POST                           , #method{name = post                      , code = 0,     detail = 2  }).
-define( PUT                            , #method{name = put                       , code = 0,     detail = 3  }).
-define( DELETE                         , #method{name = delete                    , code = 0,     detail = 4  }).

-define( CREATED                        , #method{name = created                   , code = 2,     detail = 1  }).
-define( DELETED                        , #method{name = deleted                   , code = 2,     detail = 2  }).
-define( VALID                          , #method{name = valid                     , code = 2,     detail = 3  }).
-define( CHANGED                        , #method{name = changed                   , code = 2,     detail = 4  }).
-define( CONTENT                        , #method{name = content                   , code = 2,     detail = 5  }).
-define( BAD_REQUEST                    , #method{name = bad_request               , code = 4,     detail = 0  }).
-define( UNAUTHORIZED                   , #method{name = unauthorized              , code = 4,     detail = 1  }).
-define( BAD_OPTION                     , #method{name = bad_option                , code = 4,     detail = 2  }).
-define( FORBIDDEN                      , #method{name = forbidden                 , code = 4,     detail = 3  }).
-define( NOT_FOUND                      , #method{name = not_found                 , code = 4,     detail = 4  }).
-define( METHOD_NOT_ALLOWED             , #method{name = method_not_allowed        , code = 4,     detail = 5  }).
-define( NOT_ACCEPTABLE                 , #method{name = not_acceptable            , code = 4,     detail = 6  }).
-define( PRECONDITION_FAILED            , #method{name = precondition_failed       , code = 4,     detail = 12 }).
-define( REQUEST_ENTITY_TOO_LARGE       , #method{name = request_entity_too_large  , code = 4,     detail = 13 }).
-define( UNSUPPORTED_CONTENT_FORMAT     , #method{name = unsupported_content_format, code = 4,     detail = 15 }).
-define( INTERNAL_SERVER_ERROR          , #method{name = internal_server_error     , code = 5,     detail = 0  }).
-define( NOT_IMPLEMENTED                , #method{name = not_implemented           , code = 5,     detail = 1  }).
-define( BAD_GATEWAY                    , #method{name = bad_gateway               , code = 5,     detail = 2  }).
-define( SERVICE_UNAVAILABLE            , #method{name = service_unavailable       , code = 5,     detail = 3  }).
-define( GATEWAY_TIMEOUT                , #method{name = gateway_timeout           , code = 5,     detail = 4  }).
-define( PROXYING_NOT_SUPPORTED         , #method{name = proxying_not_supported    , code = 5,     detail = 5  }).
