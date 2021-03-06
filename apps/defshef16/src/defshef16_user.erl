%%%-------------------------------------------------------------------
%% @doc defshef16 user
%% @end
%%%-------------------------------------------------------------------

-module(defshef16_user).

-behaviour(gen_server).

%% public api
-export([
    get_full_name/1,
    set_name/3,
    first_name_starts_with/2
]).

%% supervisor api
-export([start_link/2]).

%% gen_server callbacks
-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

%%====================================================================
%% State record
%%====================================================================

-record(state, {
    first_name,
    last_name
}).

%%====================================================================
%% API functions
%%====================================================================

get_full_name(Pid) ->
    gen_server:call(Pid, get_full_name).

set_name(Pid, FirstName, LastName) ->
    gen_server:cast(Pid, {set_name, FirstName, LastName}).

first_name_starts_with(Pid, Letter) ->
    gen_server:call(Pid, {first_name_starts_with, Letter}).

%%====================================================================
%% Supervisor api
%%====================================================================

start_link(FirstName, LastName) when is_list(FirstName), is_list(LastName) ->
    State = #state{
        first_name=FirstName,
        last_name=LastName
    },
    gen_server:start_link(?MODULE, [State], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([State]) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change({down, _DownVsn}, #state{first_name=FirstName, last_name=LastName}, _Extra) ->
    {ok, dict:from_list([
        {first_name, FirstName},
        {last_name, LastName}
    ])};

code_change(_OldVsn, Dict, _Extra) ->
    {ok, FirstName} = dict:find(first_name, Dict),
    {ok, LastName} = dict:find(last_name, Dict),
    {ok, #state{first_name=FirstName, last_name=LastName}}.

%%====================================================================
%% call
%%====================================================================

handle_call(get_full_name, _From, State) ->
    FullName = lists:flatten(io_lib:format("~s ~s", [
        State#state.first_name,
        State#state.last_name
    ])),
    {reply, FullName, State};

handle_call({first_name_starts_with, Prefix}, _From, State = #state{first_name=FirstName}) ->
    StartsWith = lists:prefix(string:to_lower(Prefix), string:to_lower(FirstName)),
    {reply, StartsWith, State}.

%%====================================================================
%% cast
%%====================================================================

handle_cast({set_name, FirstName, LastName}, State) ->
    {noreply, State#state{
        first_name=FirstName,
        last_name=LastName
    }}.

%%====================================================================
%% info
%%====================================================================

handle_info(_Info, State) ->
    {noreply, State}.
