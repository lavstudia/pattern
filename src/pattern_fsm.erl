%%%-------------------------------------------------------------------
%%% @author andrey-vl
%%% @copyright (C) 2015, < Triumph >
%%% @doc
%%%
%%% @end
%%% Created : 21. Сент. 2015 10:53
%%%-------------------------------------------------------------------
-module(pattern_fsm).
-author("andrey-vl").

-behaviour(gen_fsm).
-include("pattern.hrl").

%% API
-export([
    start_link/0,
    get_pattern_tree/1,
    get_pattern_text/1
]).

%% gen_fsm callbacks
-export([
    init/1,
    awaiting/2,
    check_pattern/2,
    action/2,
    move_in_tree/2,
    state_name/2,
    state_name/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% get_pattern_tree
%%
%% @end
%%--------------------------------------------------------------------
get_pattern_tree(PatternTree) ->
    gen_fsm:send_all_state_event(?SERVER, {get_pattern_tree, PatternTree}).

%%--------------------------------------------------------------------
%% @doc
%% get_pattern_text
%%
%% @end
%%--------------------------------------------------------------------
get_pattern_text(PatternText) ->
    gen_fsm:send_all_state_event(?SERVER, {get_pattern_text, PatternText}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: #state{}} |
    {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    State = #pattern_state{},

    {ok, awaiting, State}.

%% trigger(Pid) ->
%%     gen_fsm:send_event(Pid, time_is_over).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
awaiting({get_awaiting_pattern, PatternText}, State) ->
    pattern_api:log({?MODULE, awaiting,
        {get_awaiting_pattern, PatternText}}),

    NewState = State#pattern_state{text_cur = PatternText},

    gen_fsm:send_event(?SERVER, get_pattern),
    {next_state, check_pattern, NewState};
awaiting({with_timeout, TimeOut}, State) ->
    pattern_api:log({?MODULE, awaiting, {with_timeout, TimeOut}}),

    if
        TimeOut =/= 0 ->
            TimeoutRef = gen_fsm:send_event_after(TimeOut, time_is_over),
            put(timeout_ref, TimeoutRef),
            {next_state, awaiting, State};
        true ->
            gen_fsm:send_event(?SERVER, start),
            {next_state, awaiting, State}
    end;
awaiting(time_is_over, State) ->
    pattern_api:log({?MODULE, awaiting, time_is_over}),

    gen_fsm:send_event(?SERVER, time_is_over),
    {next_state, action, State};
awaiting(_Event, State) ->
    pattern_api:log({?MODULE, awaiting, event}),

    TimeoutRef = get(timeout_ref),
    if
        TimeoutRef =/= undefined ->
            gen_fsm:cancel_timer(TimeoutRef),
            erase(timeout_ref);
        true -> ok
    end,

    NewState = State#pattern_state{
        text_cur = undefined,
        el_tree_cur_id = undefined,
        el_tree_cur = undefined
    },

    {next_state, awaiting, NewState}.

check_pattern(get_pattern, State) ->
    pattern_api:log({?MODULE, check_pattern, get_pattern}),

    PatternTree = State#pattern_state.pattern_tree,
    PatternText = State#pattern_state.text_cur,
    ElTreeCurID = State#pattern_state.el_tree_cur_id,

    if
        PatternTree =/= [] ->
            SubTree = [ElTree || ElTree <- PatternTree,
                pattern_api:element_tree_keymath(parent_id, ElTreeCurID, ElTree)],
            PatternMathList = [ElSubTree || ElSubTree <- SubTree,
                pattern_api:element_tree_regexp(pattern, PatternText, ElSubTree)],
            LPatternList = length(PatternMathList),

            if
                LPatternList > 0 ->
                    ElementMath = hd(PatternMathList),

                    NewState = State#pattern_state{el_tree_cur = ElementMath},

                    gen_fsm:send_event(?SERVER, found_pattern),
                    {next_state, move_in_tree, NewState};
                true ->
                    gen_fsm:send_event(?SERVER, unfound_pattern),
                    {next_state, action, State}
            end;
        true ->
            gen_fsm:send_event(?SERVER, unfound_pattern),
            {next_state, action, State}
    end;
check_pattern(_Event, State) ->
    pattern_api:log({?MODULE, check_pattern, event}),

    {next_state, awaiting, State}.

action(pattern_move, State) ->
    pattern_api:log({?MODULE, action, pattern_move}),

    ElementTree = State#pattern_state.el_tree_cur,
    PatternAction =
        pattern_api:element_tree_get_value(pattern_action, ElementTree),
    if
        PatternAction =/= undefined ->
            PatternText = State#pattern_state.text_cur,
            pattern_api:executed_pattern_action(PatternAction, PatternText);
        true -> undefined
    end,

    gen_fsm:send_event(?SERVER, move_in),
    {next_state, move_in_tree, State};
action(unfound_pattern, State) ->
    pattern_api:log({?MODULE, action, unfound_pattern}),

    ElementTree = State#pattern_state.el_tree_cur,
    TimeoutAction =
        pattern_api:element_tree_get_value(timeout_action, ElementTree),
    if
        TimeoutAction =/= undefined ->
            PatternText = State#pattern_state.text_cur,
            pattern_api:executed_pattern_action(TimeoutAction, PatternText);
        true -> undefined
    end,

    gen_fsm:send_event(?SERVER, move_in_timeout),
    {next_state, move_in_tree, State};
action(time_is_over, State) ->
    pattern_api:log({?MODULE, action, time_is_over}),

    gen_fsm:send_event(?SERVER, unfound_pattern),
    {next_state, action, State};
action(_Event, State) ->
    pattern_api:log({?MODULE, action, event}),

    gen_fsm:send_event(?SERVER, start),
    {next_state, awaiting, State}.

move_in_tree(found_pattern, State) ->
    pattern_api:log({?MODULE, move_in_tree, found_pattern}),

    ElementTree = State#pattern_state.el_tree_cur,

    NewState =
        State#pattern_state{
            el_tree_cur_id = pattern_api:element_tree_get_value(id, ElementTree)},

    gen_fsm:send_event(?SERVER, pattern_move),
    {next_state, action, NewState};
move_in_tree(move_in, State) ->
    pattern_api:log({?MODULE, move_in_tree, move_in}),

    ElementTree = State#pattern_state.el_tree_cur,

    %% u:log({element_tree, ElementTree}),

    PatternMove = pattern_api:element_tree_get_value(pattern_move, ElementTree),

    if
        PatternMove =/= undefined ->
            PatternTree = State#pattern_state.pattern_tree,

            SubTree = [ElTree || ElTree <- PatternTree,
                pattern_api:element_tree_keymath(id, PatternMove, ElTree)],
            LSubTree = length(SubTree),
            PatternMoveElementTree =
                if
                    LSubTree > 0 -> hd(SubTree);
                    true -> []
                end,

            %% u:log({pattern_move_element_tree, PatternMoveElementTree}),

            NewState = State#pattern_state{
                el_tree_cur_id = PatternMove,
                el_tree_cur = PatternMoveElementTree
            },

%%             Pattern = pattern_api:element_tree_get_value(pattern, PatternMoveElementTree),

            case pattern_api:element_tree_get_value(pattern, PatternMoveElementTree) of
                other ->
                    gen_fsm:send_event(?SERVER, pattern_move),
                    {next_state, action, NewState};
                _ ->
                    TimeOut = pattern_api:element_tree_get_value(timeout, ElementTree, 0) * 1000,

                    gen_fsm:send_event(?SERVER, {with_timeout, TimeOut}),
                    {next_state, awaiting, NewState}
            end;
        true ->
            TimeOut = pattern_api:element_tree_get_value(timeout, ElementTree, 0) * 1000,

            gen_fsm:send_event(?SERVER, {with_timeout, TimeOut}),
            {next_state, awaiting, State}
    end;
move_in_tree(move_in_timeout, State) ->
    pattern_api:log({?MODULE, move_in_tree, move_in_timeout}),

    ElementTree = State#pattern_state.el_tree_cur,

    TimeoutMove = pattern_api:element_tree_get_value(timeout_move, ElementTree),

    if
        TimeoutMove =/= undefined ->
            PatternTree = State#pattern_state.pattern_tree,

            SubTree = [ElTree || ElTree <- PatternTree,
                pattern_api:element_tree_keymath(id, TimeoutMove, ElTree)],
            LSubTree = length(SubTree),
            TimeoutMoveElementTree =
                if
                    LSubTree > 0 -> hd(SubTree);
                    true -> []
                end,

            NewState = State#pattern_state{
                el_tree_cur_id = TimeoutMove,
                el_tree_cur = TimeoutMoveElementTree
            },

%%             Pattern = pattern_api:element_tree_get_value(pattern, PatternMoveElementTree),

            case pattern_api:element_tree_get_value(pattern, TimeoutMoveElementTree) of
                other ->
                    gen_fsm:send_event(?SERVER, pattern_move),
                    {next_state, action, NewState};
                _ ->
                    TimeOut = pattern_api:element_tree_get_value(timeout, TimeoutMoveElementTree, 0) * 1000,

                    gen_fsm:send_event(?SERVER, {with_timeout, TimeOut}),
                    {next_state, awaiting, NewState}
            end;
        true ->
            gen_fsm:send_event(?SERVER, start),
            {next_state, awaiting, State}
    end;
move_in_tree(_Event, State) ->
    pattern_api:log({?MODULE, move_in_tree, event}),

    {next_state, awaiting, State}.

-spec(state_name(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), From :: {pid(), term()},
        State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewState :: #state{}} |
    {stop, Reason :: normal | term(), Reply :: term(),
        NewState :: #state{}}).
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
        StateData :: #state{}) ->
    {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
    {next_state, NextStateName :: atom(), NewStateData :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event({get_pattern_tree, PatternTree}, awaiting, State) ->
    pattern_api:log({?MODULE, {get_pattern_tree, PatternTree}}),

    CurPatternTree = State#pattern_state.pattern_tree,
    NewPatternTree = CurPatternTree ++ PatternTree,
    NewState = State#pattern_state{pattern_tree = NewPatternTree},

    gen_fsm:send_event(?SERVER, start),
    {next_state, awaiting, NewState};
handle_event({get_pattern_text, PatternText}, awaiting, State) ->
    pattern_api:log({?MODULE, {get_pattern_text, PatternText}}),

    gen_fsm:send_event(?SERVER, {get_awaiting_pattern, PatternText}),
    {next_state, awaiting, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
        StateName :: atom(), StateData :: term()) ->
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
    {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event({get_pattern_text, PatternText}, _From, awaiting, State) ->
    pattern_api:log({?MODULE, {get_pattern_text, PatternText}}),

    gen_fsm:send_event(?SERVER, {get_awaiting_pattern, PatternText}),
    {reply, ok, awaiting, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
        StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
        StateData :: #state{}, Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
