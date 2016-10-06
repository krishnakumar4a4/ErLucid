-module(mprof_han).

-rcs('$Id$').
-behaviour(gen_event).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([add_handler/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {}).

%%====================================================================
%% API
%%====================================================================

%%!-------------------------------------------------------------------
%% add_handler -- Add this module as an event handler.
%%
%% add_handler(EventMgr) -> ok | ErrorRet 
%%--------------------------------------------------------------------
add_handler(EventMgr) ->
    gen_event:add_handler(EventMgr, ?MODULE, []).

%%====================================================================
%% Handler callback functions
%%====================================================================

%%!-------------------------------------------------------------------
%% Func: init -- Initialize the event handler.
%%
%% init(ArgList) - > Return
%%   Return = {ok, State} |
%%            Other
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%!-------------------------------------------------------------------
%% handle_event -- Handle an event.
%%
%% handle_event(Event, State) -> Return
%%   Return =  {ok, State}                                |
%%             {swap_handler, Args1, State1, Mod2, Args2} |
%%             remove_handler
%%--------------------------------------------------------------------
handle_event({profile,Pid,Status,MFA,Ts}, State) ->
    io:format("Pid ~p state ~p MFA ~p Ts ~p~n",[Pid,Status,MFA,Ts]),
    {ok, State};
handle_event(Event, State) ->
    io:format("Event not handled ~p~n",[Event]),
    {ok, State}.

%%!-------------------------------------------------------------------
%% handle_call -- Handle a call.
%%
%% handle_call(Request, State) -> Return
%%   Return = {ok, Reply, State}                                |
%%            {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%            {remove_handler, Reply}
%%--------------------------------------------------------------------
handle_call(Request, State) ->
    io:format("Request not handled ~p~n",[Request]),
    Reply = ok,
    {ok, Reply, State}.

%%!-------------------------------------------------------------------
%% handle_info -- Handle all non event/call messages.
%%
%% handle_info(Info, State) -> Return
%%   Return = {ok, State}                                |
%%            {swap_handler, Args1, State1, Mod2, Args2} |
%%            remove_handler
%%--------------------------------------------------------------------
handle_info({file,IoDev},_State) ->
    {ok,IoDev};
handle_info({profile,Pid,active,MFA,Ts}, State) ->
    %%io:format("Profile Pid ~p state ~p MFA ~p Ts ~p~n",[Pid,Status,MFA,Ts]),
    io:format("~p~n",[[S||S<-[{P,erlang:process_info(P,status)}||P<-erlang:processes()],element(2,S)=/={status,waiting}]]),
     case erlang:process_info(Pid) of
     	[{current_function,CurFun},_,{_,Status},_,_,_,_,_,_,_,_,{_,TotHeap},{_,Heap},_,_,_,_] ->
     	    io:format("Pid ~p CurrFun ~p Status ~p TotHeap ~p Heap ~p; MFA ~p Ts ~p~n",[Pid,CurFun,Status,TotHeap,Heap,MFA,Ts]);
     	[_,{current_function,CurFun},_,{_,Status},_,_,_,_,_,_,_,_,{_,TotHeap},{_,Heap},_,_,_,_] ->
     	    io:format("Pid ~p CurrFun ~p Status ~p TotHeap ~p Heap ~p; MFA ~p Ts ~p~n",[Pid,CurFun,Status,TotHeap,Heap,MFA,Ts])
     end,
    {ok, State};
handle_info({profile,scheduler,Id,active,Count,Ts}, State) ->
    io:format("Scheduler withId ~p is active,rem count ~p at ~p~n",[Id,Count,Ts]),
    {ok,State};
handle_info({profile,scheduler,Id,inactive,Count,Ts}, State) ->
    io:format("Scheduler withId ~p is active,rem count ~p at ~p~n",[Id,Count,Ts]),
    {ok,State};
handle_info({profile,Pid,inactive,MFA,Ts}, State) ->
    case erlang:process_info(Pid) of
 	[{current_function,CurFun},_,{_,Status},_,_,_,_,_,_,_,_,{_,TotHeap},{_,Heap},_,_,_,_] ->
 	    io:format("Pid ~p CurrFun ~p Status ~p TotHeap ~p Heap ~p; MFA ~p Ts ~p~n",[Pid,CurFun,Status,TotHeap,Heap,MFA,Ts]);
 	[_,{current_function,CurFun},_,{_,Status},_,_,_,_,_,_,_,_,{_,TotHeap},{_,Heap},_,_,_,_] ->
 	    io:format("Pid ~p CurrFun ~p Status ~p TotHeap ~p Heap ~p; MFA ~p Ts ~p~n",[Pid,CurFun,Status,TotHeap,Heap,MFA,Ts])
    end,
    {ok, State};
handle_info(Info, State) ->
    io:format("Got following Info for gen_event ~p~n",[Info]),
    {ok, State}.

%%!-------------------------------------------------------------------
%% terminate -- Terminate the handler.
%%
%% terminate(Arg, State) -> void()
%%   Arg = Args |
%%             %% if the event handler is deleted due to a call
%%             %% to gen_event:delete_handler, gen_event:swap_handler/3
%%             %% or gen_event:swap_sup_handler/3
%%         {stop, Reason} |
%%             %% if the event handler has a supervised connection
%%             %% to a process which has terminated with reason Reason.
%%         stop |
%%             %% if the event handler is deleted because
%%             %% the event manager is terminating.
%%         remove_handler |
%%             %% if the event handler is deleted because another
%%             %% callback function has returned remove_handler
%%             %% or {remove_handler,Reply}
%%         {error, {'EXIT', Reason}} |
%%         {error, Term}
%%             %% if the event handler is deleted because a
%%             %% callback function returned an unexpected value, Term,
%%             %% Arg={error,{'EXIT',Reason}} if a callback failed.
%%--------------------------------------------------------------------
terminate(_Arg, _State) ->
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
