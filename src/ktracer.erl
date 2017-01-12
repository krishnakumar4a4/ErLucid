%%%!------------------------------------------------------------------
%%% ktracer -- 
%%%
%%% @Copyright:   
%%% @Creator:      Krishna Kumar Thokala
%%% @Date Created: 
%%% @Description:  
%%%-------------------------------------------------------------------
-module(ktracer).

-rcs('$Id$').
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([start_link/0]).
-export([stop/0]).
-export([r_mem/0,print_r_mem/1,mem_diff/2]).
%% For debugging:
-export([dump/0]).
-export([crash/0]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(TIMEOUT, infinity). %% milliseconds | infinity

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {r_mem_count,mem_diff}).

%%====================================================================
%% API
%%====================================================================

%%!-------------------------------------------------------------------
%% start_link -- Start the server.
%%
%% start_link() -> {ok, Pid} | ignore | {error, Reason}
%%   Reason = {already_started, Pid} | term()
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%!-------------------------------------------------------------------
%% stop -- Stop the server.
%%
%% stop() -> ok
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?SERVER, stop, ?TIMEOUT).

%%!-------------------------------------------------------------------
%% dump -- Dump the server's internal state (for debugging purposes).
%%
%% dump() -> state()
%%--------------------------------------------------------------------
dump() ->
    gen_server:call(?SERVER, dump, ?TIMEOUT).

%%!-------------------------------------------------------------------
%% crash -- Crash the server (for debugging purposes).
%%
%% crash() -> exit()
%%--------------------------------------------------------------------
crash() ->
    gen_server:call(?SERVER, crash, ?TIMEOUT).

%%====================================================================
%% Server functions
%%====================================================================

%%!-------------------------------------------------------------------
%% init -- Initialize the server.
%%
%% init(ArgList) -> Return
%%   Return = {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    ets:new(memory_data,[bag,named_table,public]),
    {ok, #state{r_mem_count=0,mem_diff=0}}.

%%!-------------------------------------------------------------------
%% handle_call -- Handle call messages.
%%
%% handle_call(Request, From, State) -> Return
%%   From = {pid(), Tag}
%%   Return = {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   | %(terminate/2 is called)
%%            {stop, Reason, State}            %(terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({r_mem}, _From, State) ->
    NewRMemCount = State#state.r_mem_count + 1,
    MemRecorded = memory(),
    Timestamp = calendar:local_time(),
    case ets:insert(memory_data,{NewRMemCount,Timestamp,MemRecorded}) of
	true ->
	    io:format("Process count is: ~p~n",[length(MemRecorded)]),
	    io:format("New record inserted in ETS with value ~p at ~p~n",[NewRMemCount,Timestamp]);
	Error ->
	    io:format("ETS insert failed for record ~p with reason ~p",[NewRMemCount,Error])
    end,
    {reply, ok, State#state{r_mem_count=NewRMemCount}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(dump, _From, State) ->
    io:format("~p:~p: State=~n  ~p~n", [?MODULE, self(), State]),
    {reply, State, State};
handle_call(crash, From, _State) ->
    erlang:error({deliberately_crashed_from,From});
handle_call(UnknownRequest, _From, State) ->
    {reply, {error, {bad_request, UnknownRequest}}, State}.

%%!-------------------------------------------------------------------
%% handle_cast -- Handle cast messages.
%%
%% handle_cast(Msg, State) -> Return
%%   Return = {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}            %(terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({sum,Diff}, State) ->
    NewState = State#state{mem_diff = State#state.mem_diff + Diff},
    {noreply, NewState}.

%%!-------------------------------------------------------------------
%% handle_info -- Handle all non call/cast messages.
%%
%% handle_info(Info, State) -> Return
%%   Return = {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}            %(terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%!-------------------------------------------------------------------
%% terminate -- Shutdown the server.
%%
%% terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%!-------------------------------------------------------------------
%% code_change -- Convert process state when code is changed.
%%
%% code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

memory() ->
    PidList = erlang:processes(),
    [{Pid,case erlang:process_info(Pid,registered_name) of [] -> undefined;Tuple ->element(2,Tuple) end,element(2,erlang:process_info(Pid,memory))}||Pid<-PidList].

high_mem_diff(L1,L2) ->
    io:format("difference between two snapshots ~p and ~p ~n",[element(1,L1),element(1,L2)]),
    io:format("Process count of ~p is: ~p~n",[element(1,L1),length(element(3,L1))]),
    io:format("Process count of ~p is: ~p~n",[element(1,L2),length(element(3,L2))]),
    process_processes(L1,L2).
    %%[io:format("ProcessName ~p Pid ~p, initial mem ~p,final mem ~p, diff ~p~n",[element(2,EachPM1),element(1,EachPM1),element(3,EachPM1),element(3,EachPM2),(element(3,EachPM1)-element(3,EachPM2))])||EachPM1<-element(3,L1),EachPM2<-element(3,L2),EachPM2>EachPM1,element(1,EachPM1)=:=element(1,EachPM2),element(2,EachPM1)=:=element(2,EachPM2)].

check_mem(EachPid1,EachName1,Mem1,Mem2) ->
    io:format("Old Process with pid ~p name ~p and oldmem ~p newmem ~p and diff ~p~n",[EachPid1,EachName1,Mem1,Mem2,(Mem2-Mem1)]),
    gen_server:cast(ktracer,{sum,Mem2-Mem1}).

check_each([],{Pid2,Name2,Mem2}) ->
    io:format("New Process with pid ~p name ~p and extramem ~p~n",[Pid2,Name2,Mem2]);
check_each([{EachPid1,EachName1,EachMem1}|Rem],{Pid2,Name2,Mem2}) ->
    case EachPid1 =:= Pid2 of
	true ->
	    case EachName1 =:= Name2 of
		true ->
		    check_mem(EachPid1,EachName1,EachMem1,Mem2);
		_ ->
		    io:format("same pid process with pid ~p name ~p and extramem ~p~n",[EachPid1,Name2,Mem2]),
		    gen_server:cast(ktracer,{sum,Mem2-EachMem1})
	    end;
	_ ->
	    case EachName1 =:= Name2 of
		true ->
		    io:format("same name process with pid ~p name ~p and extramem ~p~n",[Pid2,EachName1,Mem2]),
		    gen_server:cast(ktracer,{sum,Mem2-EachMem1});
		_ ->
		    check_each(Rem,{Pid2,Name2,Mem2})
	    end
    end.

check_each_pid(List1,{Pid2,Name2,Mem2}) ->
    %%[||{EachPid,EachName,EachMem}<-List1,Pid=:=EachPid,EachName=:=Name]
    check_each(List1,{Pid2,Name2,Mem2}).

check_each_p(_List1,[]) ->
    ok;
check_each_p(List1,[Each|Rem]) ->
    check_each_pid(List1,Each),
    check_each_p(List1,Rem).
process_processes(L1,L2) ->
    check_each_p(element(3,L1),element(3,L2)).
%%--------------------------------------------------------------------
%% External functions
%%--------------------------------------------------------------------

r_mem() ->
    gen_server:call(ktracer,{r_mem},infinity).

%%collect_r_mem(Cookie,Sname,Server,)

print_r_mem(Num) ->
    case ets:lookup(memory_data,Num) of
	[] ->
	    io:format("No more data with that number ~p~n",[Num]);
	DataLookedUp ->
	    io:format("Process name                MemoryUsed~n"),
	    [io:format("~p~n",[EachData])||EachData<-DataLookedUp]
    end.
	    
mem_diff(First,Second) when is_integer(First) andalso is_integer(Second)->
    case ets:lookup(memory_data,First) of
	[] ->
	    io:format("No data found with ~p~n",[First]);
	[LookedUp1|_] ->
	    case ets:lookup(memory_data,Second) of 
		[] ->
		    io:format("No data found with ~p~n",[Second]);
		[LookedUp2|_] ->
		    %%io:format("~p ~n~p ~n",[LookedUp1,LookedUp2]),
		    high_mem_diff(LookedUp1,LookedUp2)
	    end
    end.
