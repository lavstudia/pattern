%%%-------------------------------------------------------------------
%%% @author andrey-vl
%%% @copyright (C) 2015, < Triumph >
%%% @doc
%%%
%%% @end
%%% Created : 21. Сент. 2015 10:53
%%%-------------------------------------------------------------------
-module(pattern_api).
-author("andrey-vl").

-include("pattern.hrl").

%% API pattern_fsm
-export([
    set_pattern_tree/1,
    set_pattern_text/1
]).

%% API utility
-export([
    log/1, log/2, log/3,
    unicode_convert/1
]).

%% API tree
-export([
    element_tree_get_value/2, element_tree_get_value/3,
    element_tree_keyfind/2,
    element_tree_keymember/2,
    element_tree_keymath/3,
    element_tree_regexp/3
]).

%% API action
-export([
    executed_pattern_action/1, executed_pattern_action/2
]).

%%--------------------------------------------------------------------
%% @doc
%% set_pattern_tree
%%
%% @end
%%--------------------------------------------------------------------
set_pattern_tree(PatternTree) ->
    pattern_fsm:get_pattern_tree(PatternTree).

%%--------------------------------------------------------------------
%% @doc
%% set_pattern_text
%%
%% @end
%%--------------------------------------------------------------------
set_pattern_text(PatternText) ->
    pattern_fsm:get_pattern_text(PatternText).

%%--------------------------------------------------------------------
%% @doc
%% Log
%%
%% @end
%%--------------------------------------------------------------------
log(X) ->
    ?DBG(X).
log(X, Y) ->
    ?DBG(X), ?DBG(Y).
log(X, Y, Z) ->
    ?DBG(X), ?DBG(Y), ?DBG(Z).

%%--------------------------------------------------------------------
%% @doc
%% element_tree_get_value
%%
%% @end
%%--------------------------------------------------------------------
element_tree_get_value(Key, List) when is_list(List)->
    proplists:get_value(Key, List);
element_tree_get_value(_Key, _List) -> undefined.
element_tree_get_value(Key, List, Default) when is_list(List) ->
    proplists:get_value(Key, List, Default);
element_tree_get_value(_Key, _List, _Default) -> undefined.

%%--------------------------------------------------------------------
%% @doc
%% element_tree_keyfind
%%
%% @end
%%--------------------------------------------------------------------
element_tree_keyfind(Key, ElementTree) when is_list(ElementTree)->
    lists:keyfind(Key, 1, ElementTree);
element_tree_keyfind(_Key, _ElementTree) -> false.

%%--------------------------------------------------------------------
%% @doc
%% element_tree_keymember
%%
%% @end
%%--------------------------------------------------------------------
element_tree_keymember(Key, ElementTree) when is_list(ElementTree)->
    lists:keymember(Key, 1, ElementTree);
element_tree_keymember(_Key, _ElementTree) -> false.

%%--------------------------------------------------------------------
%% @doc
%% element_tree_keymath
%%
%% @end
%%--------------------------------------------------------------------
element_tree_keymath(Key, ValueMath, ElementTree) when is_list(ElementTree)->
    TupleFind = lists:keyfind(Key, 1, ElementTree),
    TupleFind =:= {Key, ValueMath};
element_tree_keymath(_Key, _ValueMath, _ElementTree) -> false.

%%--------------------------------------------------------------------
%% @doc
%% element_tree_regexp
%%
%% @end
%%--------------------------------------------------------------------
element_tree_regexp(Key, ValueMath, ElementTree) when is_list(ElementTree)->
    TupleFind = lists:keyfind(Key, 1, ElementTree),
    {Key, Value} = TupleFind,

    RegextMath =
        try
            {ok, P} = re:compile(unicode_convert(Value), [unicode]),
            re:run(unicode_convert(ValueMath), P, [global])
        catch
            _:Reason -> {error, Reason}
        end,
    case RegextMath of
        {match, _Captured} -> true;
        match -> true;
        nomatch -> false;
        {error, _ErrType} -> false;
        _ -> false
    end;
element_tree_regexp(_Key, _ValueMath, _ElementTree) -> false.

%%--------------------------------------------------------------------
%% @doc
%% executed_pattern_action
%%
%% @end
%%--------------------------------------------------------------------
executed_pattern_action(PatternAction) ->
    if
        PatternAction =/= undefined ->
            try
                eval(PatternAction)
            catch
                _:Reason -> log(Reason), error
%%                 TODO ADD SEND ERROR BUS
            end;
        true -> false
    end.
executed_pattern_action(PatternAction, PatternText) ->
    Bind = erl_eval:add_binding('PatternText',
        PatternText, erl_eval:new_bindings()),

    if
        PatternAction =/= undefined ->
            try
                eval(PatternAction, Bind)
            catch
                _:Reason -> log(Reason), error
%%                 TODO ADD SEND ERROR BUS
            end;
        true -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% "Отъевалить" строку, задать значения в области видимости. Вернуть
%% значение результата исполнения.
%%
%% @end
%%--------------------------------------------------------------------
eval(S) -> eval(S, []).
eval(S, Environ) ->
%%     ES = binary_to_list(S),

    {ok, Scanned, _} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed, Environ).

unicode_convert(Text) when is_list(Text) ->
    unicode:characters_to_binary(Text);
unicode_convert(Text) -> Text.