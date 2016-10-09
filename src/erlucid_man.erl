-module(erlucid_man).

-rcs('$Id$').

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([start_link/1]).
-export([notify_x/1]).

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

%%!-------------------------------------------------------------------
%% start_link -- Start the event manager.
%%
%% start_link() -> {ok, Pid} | {error, Reason}
%%   Reason = {already_started, Pid} | term()
%%--------------------------------------------------------------------
start_link(ProfileArgs) ->
    case gen_event:start_link({local, ?SERVER}) of
	{ok,Pid} ->
	    %% case erlang:system_profile() of
	    %% 	undefined ->
	    %% 	    io:format("setting system profile ~n"),
	    %% 	    erlang:system_profile(Pid,ProfileArgs);		       
	    %% 	{Pid,ProfileArgs} ->
	    %% 	    ok
	    %% end,
	    case erlang:trace(all,true,[{tracer,Pid},call,return_to,arity,set_on_spawn,set_on_link]) of
		Num when is_integer(Num) ->
		    case erlang:trace_pattern({'_','_','_'},true,[local,call_count,call_time]) of
			Num1 when is_integer(Num1) ->
			    ok;
			PatReason ->
			    io:format("Trace pattern failed with reason ~p~n",[PatReason])
		    end;
		Reason1 ->
		    io:format("Trace failed with reason ~p~n",[Reason1])
	    end,
	    erlucid_han:add_handler(Pid),
	    case file:open("perception_1",[write]) of
		{error,Reason} ->
		    io:format("error opening file ~p~n",[Reason]),
		    undefined;
		{ok,IoDev} ->
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
