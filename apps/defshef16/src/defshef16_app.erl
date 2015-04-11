%%%-------------------------------------------------------------------
%% @doc defshef16 public API
%% @end
%%%-------------------------------------------------------------------

-module(defshef16_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    defshef16_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
