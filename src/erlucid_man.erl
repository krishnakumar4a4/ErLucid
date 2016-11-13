%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 12 Nov 2016 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(erlucid_man).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([notify_x/1]).
-export([t_m/1,t_m/2,t_mf/1,t_mfa/1,trace_on/0]).

-define(SERVER, ?MODULE).

-record(state, {event_manager}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    %%Start the real trace buddy
    trace_on().

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %%!-------------------------------------------------------------------
    %% start_link -- Start the event manager.
    %%
    %% start_link() -> {ok, Pid} | {error, Reason}
    %%   Reason = {already_started, Pid} | term()
    %%--------------------------------------------------------------------
    case gen_event:start_link({local, erlucid_event}) of
	{ok,Pid} ->
	    %%Below is for turning on system_profile with 
	    %%scheduler etc.
	    %% case erlang:system_profile() of
	    %% 	undefined ->
	    %% 	    io:format("setting system profile ~n"),
	    %% 	    erlang:system_profile(Pid,ProfileArgs);    
	    %% 	{Pid,ProfileArgs} ->
	    %% 	    ok
	    %% end,
	    %%erlucid_han:add_handler(Pid),

	    %%Starting the xref server
	    case xref:start(cs) of
		{ok,Pid} ->
		    io:format("Started xref server cs with Pid
~p ~n",[Pid]);
		Reason1 ->
		    io:format("xref failed with reason ~p~n",
			      [Reason1])
	    end,
	    io:format("Event manager with Pid: ~p~n",[Pid]),
	    case file:open("erlangTrace",[write]) of
		{error,Reason} ->
		    io:format("Error opening file ~p~n",[Reason]),
		    {stop,Reason};
		{ok,IoDev} ->
		    erlucid_han:add_handler([Pid,IoDev]),
		    io:format("ErLucid_han event handler added~n"),
		    erlang:send(Pid,{file,IoDev}),
		    io:format("Trace file created with dev ~p~n",[IoDev]),
		    {ok,#state{event_manager = Pid}}
	    end;
	{error,{already_started,Pid}} ->
	    io:format("Error already started~n"),
	    {ok,#state{event_manager = Pid}};
	Reason ->
	    io:format("Tool is not running,reason ~p~n",[Reason]),
	    {stop,Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({trace_on}, _From, State) ->
    case erlang:trace(all,true,[{tracer,State#state.event_manager},call,return_to,arity,set_on_spawn,set_on_link]) of
	Num when is_integer(Num) ->
	    {reply,Num,State};
	Reason1 ->
	    io:format("Trace failed with reason ~p~n",[Reason1]),
	    {reply,Reason1,State}
    end;
handle_call({t_m,MList},_From,State) ->
    Fun = fun(MFA) ->
		  case erlang:trace_pattern(MFA,true,[local,call_count,call_time]) of
		      Num1 when is_integer(Num1) ->
			  {MFA,true};
		      PatReason ->
			  io:format("Trace pattern failed with reason ~p~n",[PatReason]),
			  {MFA,false}
		  end end,
    Result = [Fun({EachM,'_','_'})||EachM<-MList],
{reply,Result,State};
handle_call({t_mf,MList},_From,State) ->
    Fun = fun(MFA) ->
		  case erlang:trace_pattern(MFA,true,[local,call_count,call_time]) of
		      Num1 when is_integer(Num1) ->
			  {MFA,true};
		      PatReason ->
			  io:format("Trace pattern failed with reason ~p~n",[PatReason]),
			  {MFA,false}
		  end end,
    Result = [Fun({EachM,'_','_'})||EachM<-MList],
    {reply,Result,State};
handle_call({t_mfa,MList},_From,State) ->
    Fun = fun(MFA) ->
		  case erlang:trace_pattern(MFA,true,[local,call_count,call_time]) of
		      Num1 when is_integer(Num1) ->
			  {MFA,true};
		      PatReason ->
			  io:format("Trace pattern failed with reason ~p~n",[PatReason]),
			  {MFA,false}
		  end end,
    Result = [Fun({EachM,'_','_'})||EachM<-MList],
{reply,Result,State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%====================================================================
%% API
%%====================================================================

t_m(ModList) ->
    gen_server:call(?SERVER,{t_m,ModList},infinity).
t_m(ModList,all) ->
    io:format("adding OTP modules might bring end up infinite,infinite recursiveness"),
    %%Suppresses verbosity while adding otp files
    xref:set_default(s, [{verbose,false}, {warnings,false}]),
    case xref:add_release(cs,code:lib_dir(),{name,otp}) of
	{ok,otp} ->
	    [io:format("Tracing ~p~n:~p",[M,element(1,xref:q(cs,atom_to_list(M)++": Mod"))])||M<-ModList],
	    gen_server:call(?SERVER,{t_m,ModList},infinity);
	Reason ->
	    io:format("Validating module names failed ~p~n",[Reason]),
	    gen_server:call(?SERVER,{t_m,ModList},infinity)
    end.
    
t_mf(MFList) ->
    gen_server:call(?SERVER,{t_mf,MFList},infinity).
t_mfa(MFAList) ->
    gen_server:call(?SERVER,{t_mfa,MFAList},infinity).

trace_on() ->
    gen_server:call(?SERVER,{trace_on},infinity).
%%!-------------------------------------------------------------------
%% notify_x -- Send a notification to all event handlers.
%%
%% notify_x(X) -> void()
%%   Reason = {already_started, Pid} | term()
%%--------------------------------------------------------------------
notify_x(X) ->
    gen_event:notify(?SERVER, X).
