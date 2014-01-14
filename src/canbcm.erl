-module(canbcm).

-behaviour(gen_server).

%% API
-export([start_link/1,
         release/1,
	 send/3,
         add_send_job/4,
	 update_send_job/3,
	 delete_send_job/2,
         filter/4,
	 subscribe/3,
	 unsubscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        { device            :: string(),
          port              :: port()
        }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom()) ->
                    {ok, pid()} | ignore | {error, _}.
start_link(Device) ->
    gen_server:start_link(?MODULE, [Device], []).

-spec release(atom()) -> ok.
release(Device) ->
    gen_server:cast(Device, release).

-spec send(atom(), non_neg_integer(), binary()) -> ok | {error, _}.
send(Device, CanId, Data) ->
    gen_server:call(Device, {send, CanId, Data}).

%% @doc
%% Schedule a send job. The Period is specified in seconds.
-spec add_send_job(atom(), float(), non_neg_integer(), binary()) -> ok.
add_send_job(Device, Period, CanId, Data) ->
    gen_server:call(Device, {add_send_job, Period, CanId, Data}).

-spec update_send_job(atom(), non_neg_integer(), binary()) -> ok.
update_send_job(Device, CanId, Data) ->
    gen_server:call(Device, {update_send_job, CanId, Data}).

-spec delete_send_job(atom(), non_neg_integer()) -> ok.
delete_send_job(Device, CanId) ->
    gen_server:call(Device, {delete_send_job, CanId}).

-spec filter(atom(), float(), non_neg_integer(), binary()) -> ok.
filter(Device, Period, CanId, Data) ->
    gen_server:call(Device, {filter, Period, CanId, Data}).

-spec subscribe(atom(), float(), non_neg_integer()) -> ok.
subscribe(Device, Period, CanId) ->
    gen_server:call(Device, {subscribe, Period, CanId}).

-spec unsubscribe(atom(), non_neg_integer()) -> ok.
unsubscribe(Device, CanId) ->
    gen_server:call(Device, {unsubscribe, CanId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Device]) ->
    PortPath = code:priv_dir(erlang_ale) ++ "/canbcm",
    Port = erlang:open_port({spawn, PortPath}, [{packet, 2}, binary]),
    State = #state{device=Device,
		   port=Port},
    send_to_port(Port, {open, Device}),
    {ok, State}.

handle_call(release, _From, State) ->
    {stop, normal, ok, State};
handle_call({send, CanId, Data}, _From, #state{port=Port}=State) ->
    send_to_port(Port, {send, CanId, Data}),
    {ok,  State};
handle_call({add_send_job, Period, CanId, Data}, _From, #state{port=Port}=State) ->
    send_to_port(Port, {add_send_job, Period, CanId, Data}),
    {ok,  State};
handle_call({update_send_job, CanId, Data}, _From, #state{port=Port}=State) ->
    send_to_port(Port, {update_send_job, CanId, Data}),
    {ok,  State};
handle_call({delete_send_job, CanId}, _From, #state{port=Port}=State) ->
    send_to_port(Port, {delete_send_job, CanId}),
    {ok,  State};
handle_call({filter, Period, CanId, Data}, _From, #state{port=Port}=State) ->
    send_to_port(Port, {filter, Period, CanId, Data}),
    {ok,  State};
handle_call({subscribe, Period, CanId}, _From, #state{port=Port}=State) ->
    send_to_port(Port, {subscribe, Period, CanId}),
    {ok,  State};
handle_call({unsubscribe, CanId}, _From, #state{port=Port}=State) ->
    send_to_port(Port, {unsubscribe, CanId}),
    {ok,  State};
handle_call({from_port, Msg}, _From, State) ->
    io:format("Got a message from the port: ~p~n", [Msg]),
    Reply = ok,
    {reply, Reply, State};
handle_call(Msg, _From, State) ->
    io:format("handle_call got: ~p~n", [Msg]),
    {reply, huh, State}.

handle_cast(Msg, State) ->
    io:format("cast got: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("handle_info got: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{port=Port}=_State) ->
    send_to_port(Port, {release, Port}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_to_port(Port, Msg) ->
    Port ! {self(), {command, term_to_binary(Msg)}}.
