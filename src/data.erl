-module(data).
-behavior(gen_server).
-export([start/0,stop/0,stop/1]).
-export([start_rec_erlmem/1,start_rec_etsmemsize/1]).
-export([stop_rec_erlmem/0,stop_rec_etsmemsize/0]).
-export([start_gc/0,start_gc_all/0,show_gc_all/0]).
-export([apply_erl_mem/1,apply_ets_memsize/2]).
-export([init/1,terminate/2,handle_call/3]).
-record(gcmem,{mem_total_bef = 0,
mem_total_aft = 0,
mem_total_max_bef = 0,
mem_total_max_aft = 0,
mem_total_max_diff = 0,
mem_proc_bef = 0,
mem_proc_aft = 0,
mem_proc_max_bef = 0,
mem_proc_max_aft = 0,
mem_proc_max_diff = 0,
mem_procu_bef = 0,
mem_procu_aft = 0,
mem_procu_max_bef = 0,
mem_procu_max_aft = 0,
mem_procu_max_diff = 0,
mem_sys_bef = 0,
mem_sys_aft = 0,
mem_sys_max_bef = 0,
mem_sys_max_aft = 0,
mem_sys_max_diff = 0,
mem_atom_bef = 0,
mem_atom_aft = 0,
mem_atom_max_bef = 0,
mem_atom_max_aft = 0,
mem_atom_max_diff = 0,
mem_atomu_bef = 0,
mem_atomu_aft = 0,
mem_atomu_max_bef = 0,
mem_atomu_max_aft = 0,
mem_atomu_max_diff = 0,
mem_bin_bef = 0,
mem_bin_aft = 0,
mem_bin_max_bef = 0,
mem_bin_max_aft = 0,
mem_bin_max_diff = 0,
mem_code_bef = 0,
mem_code_aft = 0,
mem_code_max_bef = 0,
mem_code_max_aft = 0,
mem_code_max_diff = 0,
mem_ets_bef = 0,
mem_ets_aft = 0,
mem_ets_max_bef = 0,
mem_ets_max_aft = 0,
mem_ets_max_diff = 0}).
-record(state,{tref_erlmem,
                file_erlmem,
                tref_etsmemsize,
                file_etsmem,
                file_etssize,
		gcmem = #gcmem{}}).

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
handle_call({get_count},_From,State) ->
	New = case get(etsmemsizecounter) of
		undefined ->
			put(etsmemsizecounter,0),
			0;
		Old ->
			put(etsmemsizecounter,Old+1),
			Old+1
	end,
	{reply,New,State};
handle_call({unique_ets},_From,State) ->
	%%Try to add new tables into unique_ets process_dictionary
	case get(unique_table) of
		undefined ->
			put(unique_table,[{E,ets:info(E,name)}||E<-ets:all()]);
		UniqTab ->
			NewTab = lists:foldl(fun(U,Acc) -> case lists:member(U,UniqTab) of true -> Acc;false -> [U|Acc] end end,[],[{E,ets:info(E,name)}||E<-ets:all()]),
			put(unique_table,NewTab++UniqTab)
			%%NewTab ++ UniqTab
	end,
	{reply,ok,State};
%%registers the count of connected nodes during start_gc function call
handle_call({connected_node_count,Count},_From,State) ->
	put(connected_node_count,Count),
	{reply,ok,State};
%%handling gc_stats from all the nodes when start_gc_all invoked
handle_call({gc_stats,Node,Stat,Before,After,Diff},_From,State) ->
	GcMem = State#state.gcmem,
	MaxFun = fun(L,R) -> Left = case L of 0 ->{Node,0};L1 -> L1 end,case element(2,Left)>R of true->Left;_->{Node,R} end end,
	%%io:format("node is ~p~n",[Node]),
	NewGcMem = case Stat of
		total ->
			GcMem#gcmem{mem_total_bef = GcMem#gcmem.mem_total_bef+Before,
				mem_total_aft = GcMem#gcmem.mem_total_aft+After,
				mem_total_max_bef = MaxFun(GcMem#gcmem.mem_total_max_bef,Before),
				mem_total_max_aft = MaxFun(GcMem#gcmem.mem_total_max_aft,After),
				mem_total_max_diff = MaxFun(GcMem#gcmem.mem_total_max_diff,Diff)
				};
		processes ->
			GcMem#gcmem{mem_proc_bef = GcMem#gcmem.mem_proc_bef+Before,
				mem_proc_aft = GcMem#gcmem.mem_proc_aft+After,
				mem_proc_max_bef = MaxFun(GcMem#gcmem.mem_proc_max_bef,Before),
				mem_proc_max_aft = MaxFun(GcMem#gcmem.mem_proc_max_aft,After),
				mem_proc_max_diff = MaxFun(GcMem#gcmem.mem_proc_max_diff,Diff)
				};
		processes_used ->
			GcMem#gcmem{mem_procu_bef = GcMem#gcmem.mem_procu_bef+Before,
				mem_procu_aft = GcMem#gcmem.mem_procu_aft+After,
				mem_procu_max_bef = MaxFun(GcMem#gcmem.mem_procu_max_bef,Before),
				mem_procu_max_aft = MaxFun(GcMem#gcmem.mem_procu_max_aft,After),
				mem_procu_max_diff = MaxFun(GcMem#gcmem.mem_procu_max_diff,Diff)
				};
		system ->
			GcMem#gcmem{mem_sys_bef = GcMem#gcmem.mem_sys_bef+Before,
				mem_sys_aft = GcMem#gcmem.mem_sys_aft+After,
				mem_sys_max_bef = MaxFun(GcMem#gcmem.mem_sys_max_bef,Before),
				mem_sys_max_aft = MaxFun(GcMem#gcmem.mem_sys_max_aft,After),
				mem_sys_max_diff = MaxFun(GcMem#gcmem.mem_sys_max_diff,Diff)
				};
		atom ->
			GcMem#gcmem{mem_atom_bef = GcMem#gcmem.mem_atom_bef+Before,
				mem_atom_aft = GcMem#gcmem.mem_atom_aft+After,
				mem_atom_max_bef = MaxFun(GcMem#gcmem.mem_atom_max_bef,Before),
				mem_atom_max_aft = MaxFun(GcMem#gcmem.mem_atom_max_aft,After),
				mem_atom_max_diff = MaxFun(GcMem#gcmem.mem_atom_max_diff,Diff)
				};
		atom_used ->
			GcMem#gcmem{mem_atomu_bef = GcMem#gcmem.mem_atomu_bef+Before,
				mem_atomu_aft = GcMem#gcmem.mem_atomu_aft+After,
				mem_atomu_max_bef = MaxFun(GcMem#gcmem.mem_atomu_max_bef,Before),
				mem_atomu_max_aft = MaxFun(GcMem#gcmem.mem_atomu_max_aft,After),
				mem_atomu_max_diff = MaxFun(GcMem#gcmem.mem_atomu_max_diff,Diff)
				};
		binary ->
			GcMem#gcmem{mem_bin_bef = GcMem#gcmem.mem_bin_bef+Before,
				mem_bin_aft = GcMem#gcmem.mem_bin_aft+After,
				mem_bin_max_bef = MaxFun(GcMem#gcmem.mem_bin_max_bef,Before),
				mem_bin_max_aft = MaxFun(GcMem#gcmem.mem_bin_max_aft,After),
				mem_bin_max_diff = MaxFun(GcMem#gcmem.mem_bin_max_diff,Diff)
				};
		code ->
			GcMem#gcmem{mem_code_bef = GcMem#gcmem.mem_code_bef+Before,
				mem_code_aft = GcMem#gcmem.mem_code_aft+After,
				mem_code_max_bef = MaxFun(GcMem#gcmem.mem_code_max_bef,Before),
				mem_code_max_aft = MaxFun(GcMem#gcmem.mem_code_max_aft,After),
				mem_code_max_diff = MaxFun(GcMem#gcmem.mem_code_max_diff,Diff)
				};
		ets ->
			GcMem#gcmem{mem_ets_bef = GcMem#gcmem.mem_ets_bef+Before,
				mem_ets_aft = GcMem#gcmem.mem_ets_aft+After,
				mem_ets_max_bef = MaxFun(GcMem#gcmem.mem_ets_max_bef,Before),
				mem_ets_max_aft = MaxFun(GcMem#gcmem.mem_ets_max_aft,After),
				mem_ets_max_diff = MaxFun(GcMem#gcmem.mem_ets_max_diff,Diff)
				}
	end,
	{reply,ok,State#state{gcmem=NewGcMem}};
handle_call({show_gcstats},_From,State) ->
	io:format("~p~n",[State#state.gcmem]),
	{reply,ok,State};
				
handle_call(stop_erl_mem,_From,State) ->
    timer:cancel(State#state.tref_erlmem),
    file:close(State#state.file_erlmem),
    {reply,ok,State#state{tref_erlmem = undefined,file_erlmem = undefined}};
handle_call(stop_ets_memsize,_From,State) ->    
	case file:open("uniqtables",[write]) of
		{error,Reason} ->
			io:format("file write to uniqtables, ~p~n",[Reason]),
			io:format("Current uniqtabel list is ~n~p",[get(unique_table)]);
		{ok,Dev} ->
			io:fwrite(Dev,"~p~n",[get(unique_table)])
	end, 
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
	put(etsmemsizecounter,0),
    {ok,Tref} = timer:apply_interval(Int,data,apply_ets_memsize,[EtsMemDev,EtsSizeDev]),
    Tref.
apply_ets_memsize(EtsMemDev,EtsSizeDev) ->
	New = gen_server:call(data,{get_count},infinity),
	gen_server:call(data,{unique_ets},infinity),
	%%io:fwrite(UniqTabFile,"~p~n",[UniqtabList]),
    Lmem = [{E,ets:info(E,name),ets:info(E,memory)}||E<-ets:all()],
    Lsize = [{E,ets:info(E,name),ets:info(E,size)}||E<-ets:all()], 
    io:fwrite(EtsMemDev,"~p: ~p;~n",[New,[length(Lmem)|Lmem]]),
    io:fwrite(EtsSizeDev,"~p: ~p;~n",[New,[length(Lsize)|Lsize]]).
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


%%To garbage collect on the current node
start_gc() ->
	MemSnap1 = erlang:memory(),
	[erlang:garbage_collect(P)||P<-erlang:processes()],
	MemSnap2 = erlang:memory(),
	io:format("Following are the memory gain stattistics after gc:~n"),
	io:format("Total gain on this node:~p~n~n",[element(2,hd(MemSnap2))-element(2,hd(MemSnap1))]),
	Fun = fun(A,B,Fun) -> case {A,B} of 
					{[],[]} -> ok;
					{[H1|T1],[H2|T2]} -> io:format("~p:~p~n",[element(1,H1),(element(2,H2)-element(2,H1))]),
							Fun(T1,T2,Fun)
				end
				end,
	Fun(MemSnap1,MemSnap2,Fun).
%%To garbage collect on all the connected nodes
start_gc_all() ->
	gen_server:call(data,{connected_node_count,length(nodes())},infinity),
	Node = node(),
	rpc:multicall(nodes(),erlang,spawn,[fun() ->
						MemSnap1 = erlang:memory(),
						[erlang:garbage_collect(P)||P<-erlang:processes()],
						MemSnap2 = erlang:memory(),
						%%io:format("Following are the memory gain statistics after gc:~n"),
						%%io:format("Total gain on this node:~p~n~n",[element(2,hd(MemSnap2))-element(2,hd(MemSnap1))]),
						Fun = fun(A,B,Fun) -> case {A,B} of 
										{[],[]} -> ok;
										{[H1|T1],[H2|T2]} -> rpc:call(Node,gen_server,call,[data,{gc_stats,node(),element(1,H1),element(2,H1),element(2,H2),element(2,H2)-element(2,H1)},infinity]),
												Fun(T1,T2,Fun)
										end
						end,Fun(MemSnap1,MemSnap2,Fun) end]).

show_gc_all() ->
	gen_server:call(data,{show_gcstats},infinity).
%%------------------------------------------------------------------------------------------------%%
stop_rec_erlmem() ->
    gen_server:call(data,stop_erl_mem,infinity).
stop_rec_etsmemsize() ->
    gen_server:call(data,stop_ets_memsize,infinity).
