-define( PASS_THROUGH    , "1.0").
-define( NO_PASS_THROUGH , "1.1").
-define( STANDARD_MODULE , "2.0").

-define( MESSAGE_TYPE_ERROR             , 0).
-define( MESSAGE_TYPE_LOGIN             , 1).
-define( MESSAGE_TYPE_UP                , 2).
-define( MESSAGE_TYPE_DOWN              , 3).
-define( MESSAGE_TYPE_UP_ACK            , 16#82).
-define( MESSAGE_TYPE_DOWN_ACK          , 16#83).
-define( MESSAGE_TYPE_HEART_BEAT        , 4).
-define( MESSAGE_TYPE_ACK               , 5).
-define( MESSAGE_TYPE_HEART_BEAT_ACK    , 6).

-define( LOGIN_RESULT_SUCCESS           , 0).
-define( LOGIN_RESULT_FAIL              , 1).
-define( LOGIN_RESULT_DEVICE_UNREGISTER , 2).
-define( LOGIN_RESULT_AUTHENTICATE_FAIL , 3).
-define( LOGIN_RESULT_ALREADY_LOGIN     , 4).

-define( EXECUTE_COMMAND_SUCCESS_NO_DATA, 0).
-define( EXECUTE_COMMAND_SUCCESS        , 1).
-define( EXECUTE_COMMAND_FAIL           , 2).
