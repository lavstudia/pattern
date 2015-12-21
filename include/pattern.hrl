%%%-------------------------------------------------------------------
%%% @author andrey-vl
%%% @copyright (C) 2015, < Triumph >
%%% @doc
%%%
%%% @end
%%% Created : 08. Окт. 2015 11:43
%%%-------------------------------------------------------------------
-author("andrey-vl").

%%%%%%%%%%%%%%%%%%%%%
%% TESTs 'n DEBUGs %%
%%%%%%%%%%%%%%%%%%%%%

-define(TEST, true).

-ifdef(TEST).

-define(DUMP(FName, Data),
    file:write_file("trash~/" ++ atom_to_list(FName) ++ ".erl",
        io_lib:fwrite("~p.\n", [Data]))).
-define(DUMP(Data), file:write_file("trash~/dump.erl",
    io_lib:fwrite("(~w:~w:~b) ~p~n", [self(), ?MODULE, ?LINE, Data]),
    [append])).
-define(LOG(F, A), io:format("(~w:~w:~b) " ++ F ++ "~n",
    [self(), ?MODULE, ?LINE | A]), ?DUMP(A)).
-define(DBG(A), io:format("(~w:~w:~b) ~tp~n",
    [self(), ?MODULE, ?LINE, A]), ?DUMP(A)).
-define(tester(PROC, X), PROC ! X).

-else.

-define(LOG(F, A), []).
-define(DBG(F), []).
-define(DUMP(A, B), []).
-define(DUMP(A), []).
-define(tester(PROC, X), []).

-endif.

-record(pattern_state, {
    pattern_tree_test = [],
    pattern_tree = [
        [
            {id, 1},
            {parent_id, undefined},
            {pattern, "^(погод).*"},
            {pattern_action, "pattern_api:log(\"Погода где?\")."},
            {timeout, 30},
            {timeout_action, "pattern_api:log(\"Я не могу вам помочь!(\")."}
        ], [
            {id, 2},
            {parent_id, 1},
            {pattern, ".*(москв).*"},
            {pattern_action, "pattern_api:log(\"Вас интересует конкретный район?\")."},
            {timeout, 30},
            {timeout_move, 2}
        ], [
            {id, 3},
            {parent_id, 1},
            {pattern, ".*(минск).*"},
            {pattern_action, "pattern_api:log(\"Сейчас в Минске ...\")."}
        ], [
            {id, 4},
            {parent_id, 2},
            {pattern, "^(да|подтвержд.*)"},
            {pattern_action, "pattern_api:log(\"Какой?\")."},
            {timeout, 30},
            {timeout_move, 40}
        ], [
            {id, 5},
            {parent_id, 2},
            {pattern, "^(нет|не надо.*)"},
            {pattern_action, "pattern_api:log(\"Сейчас в Москве ...\")."}
        ], [
            {id, 6},
            {parent_id, 4},
            {pattern, ".*(таган).*"},
            {pattern_action, "pattern_api:log(\"Сейчас в районе Таганской ...\")."}
        ], [
            {id, 7},
            {parent_id, 4},
            {pattern, ".*(любой).*"},
            {pattern_move, 4}
        ], [
            {id, 40},
            {parent_id, 1},
            {pattern, other},
            {pattern_action, "pattern_api:log(\"Я не могу вам помочь!(\")."}
        ], [
            {id, 100},
            {parent_id, undefined},
            {pattern, "^(вкл.* )"},
            {pattern_action, "object_api:regexp_switch_on(\"^вкл.* (.*)\", PatternText)."}
        ], [
            {id, 101},
            {parent_id, undefined},
            {pattern, "^(выкл.* )"},
            {pattern_action, "object_api:regexp_switch_off(\"^выкл.* (.*)\", PatternText)."}
        ]
    ],
    text_cur, el_tree_cur_id, el_tree_cur
}).
