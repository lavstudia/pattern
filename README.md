===========
PATTERN_FSM
===========

A valid FSM definition contains a list of states, symbols 
and transitions, the initial state and the accepting states. 
States and symbols are alphanumeric character strings and can not overlap. 
Transitions have the format: stateA:symbol>stateB,stateC.

#states
awaiting
check_pattern
action
move_in_tree
#initial
awaiting
#accepting
awaiting
check_pattern
move_in_tree
#alphabet
start
get_pattern
unfound_pattern
found_pattern
{with_timeout|$TO}
time_is_over
move_in
move_in_timeout
pattern_move
#transitions
awaiting:get_pattern>check_pattern
awaiting:time_is_over>awaiting
awaiting:time_is_over>action
check_pattern:found_pattern>move_in_tree
check_pattern:unfound_pattern>action
action:unfound_pattern>action
action:move_in>move_in_tree
action:move_in_timeout>move_in_tree
move_in_tree:pattern_move>action
move_in_tree:start>awaiting
move_in_tree:{with_timeout|$TO}>awaiting

FOR FSM SIMULATOR - http://ivanzuzak.info/noam/webapps/fsm_simulator/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
----------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

===================
Синтаксис паттернов
===================

В паттернах используются следующие конструкции:

------------
Альтернативы
------------

Определяет наличие в данной позиции текста одной из перечисленных конструкций или последовательности символов.

(первый|второй|первый или второй)
Символ | используется для разделения альтернативных конструкций. В данном примере указано, что в тексте должно присутствовать либо слово первый, либо слово второй, либо фраза целиком первый или второй, либо число в любом формате.

Альтернативы необходимо заключать в круглые скобки.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
----------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%