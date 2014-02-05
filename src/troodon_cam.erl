-module(troodon_cam).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 stop/0,
	 get_next_picture/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        { port         :: port(),
          pending = [] :: [term()]
        }).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() ->
                    {ok, pid()} | ignore | {error, _}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

get_next_picture() ->
    gen_server:call(?SERVER, get_next_picture).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    PortPath = code:priv_dir(troodon_cam) ++ "/troodon_cam",
    Port = erlang:open_port({spawn, PortPath}, [{packet, 2}, binary, exit_status]),
    State = #state{port=Port},

    % Take the imager out of reset
    gpio:write(48, 1),
    {ok, State}.

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
handle_call(get_next_picture, From, #state{pending=Pending} = State) ->
    NewPending = [From | Pending],
    {noreply,  State#state{pending=NewPending}};
handle_call(stop, _From, #state{port=Port}=State) ->
    Port ! {self(), close},
    {reply, ok, State}.
%    {stop, normal, ok, State}.

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
handle_cast(Msg, State) ->
    io:format("Unexpected handle_cast: ~p~n", [Msg]),
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
handle_info({Port, {data, RawMsg}}, #state{port=Port,pending=Pending}=State) ->
    %io:format("Got frame: ~p bytes~n", [byte_size(RawMsg)]),
    [ gen_server:reply(To, RawMsg) || To <- Pending ],
    {noreply, State#state{pending=[]}};
handle_info({Port, {exit_status, Status}}, #state{port=Port}=State) ->
    io:format("Port exited with status ~p~n", [Status]),
    {stop, port_crashed, State};
handle_info(Info, State) ->
    io:format("Got unexpected message: ~p~n", [Info]),
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
    % Put the imager back into reset
    gpio:write(48, 0),
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
