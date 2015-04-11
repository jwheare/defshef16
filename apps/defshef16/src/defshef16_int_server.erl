%%%-------------------------------------------------------------------
%% @doc defshef16 integer server
%% @end
%%%-------------------------------------------------------------------

-module(defshef16_int_server).

-behaviour(gen_server).

%% public api
-export([
    get/0,
    increment/0, decrement/0, set/1
]).

%% supervisor api
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

%%====================================================================
%% API functions
%%====================================================================

get() ->
    gen_server:call(?MODULE, get).

increment() ->
    gen_server:call(?MODULE, increment).

decrement() ->
    gen_server:call(?MODULE, decrement).

set(Value) ->
    gen_server:cast(?MODULE, {set, Value}).

%%====================================================================
%% Supervisor api
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = 0,
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% call
%%====================================================================

handle_call(get, _From, State) ->
    {reply, State, State};

handle_call(increment, _From, State) ->
    NewState = State+1,
    {reply, NewState, NewState};

handle_call(decrement, _From, State) ->
    NewState = State-1,
    {reply, NewState, NewState}.

%%====================================================================
%% cast
%%====================================================================

handle_cast({set, Value}, _State) ->
    {noreply, Value}.

%%====================================================================
%% info
%%====================================================================

handle_info(_Info, State) ->
    {noreply, State}.
