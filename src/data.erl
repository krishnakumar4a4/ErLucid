-module(data).
-behavior(gen_server).
-export([start/0,stop/0,stop/1]).
-export([start_rec_erlmem/1,start_rec_etsmemsize/1]).
-export([stop_rec_erlmem/0,stop_rec_etsmemsize/0]).
-export([apply_erl_mem/1,apply_ets_memsize/2]).
-export([init/1,terminate/2,handle_call/3]).
-record(state,{tref_erlmem,
                file_erlmem,
                tref_etsmemsize,
                file_etsmem,
                file_etssize}).

start() ->
	gen_server:start_link({local,data},?MODULE,[],[{timeout,infinity}]).

init([]) ->
	{ok,#state{}}.

handle_call({erl_mem,Int},_From,State) ->
	case file:open("erlangmem",[write]) of
		{error,Reason} ->
                    io:format("error opening file,~p~n",[Reason]),
                        {reply,ok,State};
		{ok,IoDev} ->
			Tref = erl_mem(Int,IoDev),
                        {reply,ok,State#state{tref_erlmem = Tref,file_erlmem = IoDev}}
		end;    
handle_call({ets_memsize,Int},_From,State) ->
	EtsMemDevId = case file:open("etsmem",[write]) of
		{error,Reason1} ->
                    io:format("error opening file,reason ~p~n",[Reason1]),
			undefined;
	    	{ok,IoDev1} ->
	    		IoDev1
		end,
	EtsSizeDevId = case file:open("etssize",[write]) of
		{error,Reason2} ->
                    io:format("error opening file,reason ~p~n",[Reason2]),
			undefined;
	    	{ok,IoDev2} ->
	    		IoDev2
		end,
                case {EtsMemDevId,EtsSizeDevId} of
                    {Temp1,Temp2} when Temp1==undefined orelse Temp2 == undefined ->
                        {reply,ok,State};
                    {EtsMemDevId,EtsSizeDevId} ->
                        Tref = ets_memsize(Int,EtsMemDevId,EtsSizeDevId),
                        {reply,ok,State#state{tref_etsmemsize = Tref,file_etsmem = EtsMemDevId,file_etssize = EtsSizeDevId}}
                end;
handle_call(stop_erl_mem,_From,State) ->
    timer:cancel(State#state.tref_erlmem),
    file:close(State#state.file_erlmem),
    {reply,ok,State#state{tref_erlmem = undefined,file_erlmem = undefined}};
handle_call(stop_ets_memsize,_From,State) ->    
    timer:cancel(State#state.tref_etsmemsize),
    file:close(State#state.file_etsmem),
    file:close(State#state.file_etssize),
    {reply,ok,State#state{tref_etsmemsize = undefined,file_etsmem = undefined,file_etssize = undefined}};
handle_call(stop,_From,State) ->
	{stop,normal,State}.

%%Below function would collect erlang:memory statistics of the erlang VM to a file
%%at every mentioned interval with Int in microseconds,termination of this 
%%not good as of now. This is triggered from start function.

erl_mem(Int,IoDev) ->
    {ok,Tref} = timer:apply_interval(Int,data,apply_erl_mem,[IoDev]),
    Tref.

apply_erl_mem(IoDev) ->
	L=[integer_to_list(element(2,Tup))||Tup<-erlang:memory()],
	io:fwrite(IoDev,"~s,~s,~s,~s,~s,~s,~s,~s,~s~n",L).
%%------------------------------------------------------------------------------------------------%%
ets_memsize(Int,EtsMemDev,EtsSizeDev) ->
    {ok,Tref} = timer:apply_interval(Int,data,apply_ets_memsize,[EtsMemDev,EtsSizeDev]),
    Tref.
apply_ets_memsize(EtsMemDev,EtsSizeDev) ->
    Lmem = [ets:info(E,memory)||E<-ets:all()],
    Lsize = [ets:info(E,size)||E<-ets:all()], 
    io:fwrite(EtsMemDev,"~p~n",[[length(Lmem)|Lmem]]),
    io:fwrite(EtsSizeDev,"~p~n",[[length(Lsize)|Lsize]]).
%%------------------------------------------------------------------------------------------------%%

stop() ->
%%	gen_server:stop(data).
	gen_server:call(data,stop,infinity).
stop(Pid) ->
	gen_server:stop(Pid,normal,infinity).

terminate(Reason,State) ->
    ok.
%%	file:close(State#state.fd).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%             External functions                      %%%%%%%%%%%%%%%%%%%%%

start_rec_erlmem(Int) ->
    gen_server:call(data,{erl_mem,Int},infinity).
start_rec_etsmemsize(Int) ->
    gen_server:call(data,{ets_memsize,Int},infinity).
%%------------------------------------------------------------------------------------------------%%
stop_rec_erlmem() ->
    gen_server:call(data,stop_erl_mem,infinity).
stop_rec_etsmemsize() ->
    gen_server:call(data,stop_ets_memsize,infinity).
