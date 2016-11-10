-module(erlucid_man).

-rcs('$Id$').

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([start_link/2]).
-export([notify_x/1]).
-export([t_m/1,t_mf/1,t_mfa/1]).
%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% API
%%====================================================================

t_m(ModList) ->
    start_link(ModList,1).
t_mf(MFList) ->
    start_link(MFList,2).
t_mfa(MFAList) ->
    start_link(MFAList,3).
%%!-------------------------------------------------------------------
%% start_link -- Start the event manager.
%%
%% start_link() -> {ok, Pid} | {error, Reason}
%%   Reason = {already_started, Pid} | term()
%%--------------------------------------------------------------------
start_link(MFAList,Select) ->
    case gen_event:start_link({local, ?SERVER}) of
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
	    case erlang:trace(all,true,[{tracer,Pid},call,return_to,arity,set_on_spawn,set_on_link]) of
		Num when is_integer(Num) ->
		    Fun = fun(MFA) ->
		    case erlang:trace_pattern(MFA,true,[local,call_count,call_time]) of
			Num1 when is_integer(Num1) ->
			    ok;
			PatReason ->
			    io:format("Trace pattern failed with reason ~p~n",[PatReason])
		    end end,
		    case Select of
			1 ->
			    [Fun({EachM,'_','_'})||EachM<-MFAList];
			2 -> 
			    [Fun({EachM,EachF,'_'})||{EachM,EachF}<-MFAList];
			3 ->
			    [Fun({EachM,EachF,EachA})||{EachM,EachF,EachA}<-MFAList]
		    end;
		Reason1 ->
		    io:format("Trace failed with reason ~p~n",[Reason1])
	    end,
	    %%erlucid_han:add_handler(Pid),
	    case file:open("perception_1",[write]) of
		{error,Reason} ->
		    io:format("error opening file ~p~n",[Reason]),
		    undefined;
		{ok,IoDev} ->
		    erlucid_han:add_handler([Pid,IoDev]),
		    erlang:send(Pid,{file,IoDev})
	    end,
	    {ok,Pid};
	{error,{already_started,Pid}} ->
	    io:format("Error already started~n"),
	    {ok,Pid};
	Reason ->
	    io:format("Tool is not running,reason ~p~n",[Reason]),
	    Reason
    end.
 


%%!-------------------------------------------------------------------
%% notify_x -- Send a notification to all event handlers.
%%
%% notify_x(X) -> void()
%%   Reason = {already_started, Pid} | term()
%%--------------------------------------------------------------------
notify_x(X) ->
    gen_event:notify(?SERVER, X).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
