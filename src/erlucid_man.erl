-module(mprof_man).

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
	    case erlang:system_profile() of
		undefined ->
		    io:format("setting system profile ~n"),
		    erlang:system_profile(Pid,ProfileArgs);		       
		{Pid,ProfileArgs} ->
		    ok
	    end,
	    mprof_han:add_handler(Pid),
	    case file:open("data",[write]) of
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
