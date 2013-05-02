:- use_module(library(ansi_term)).

:- dynamic action/3, state/1, env/1, last_action/1, condition/3.

% read an environment

% an environment consists of objects

% food is an thing

% a thing can be picked up

% food can be eaten

% three things can occur: punish, reinforce or nothing
%   punish will lead to a decrease in the behavior
%   reinforce will lead to an increase in the behavior
%   neutral will lead to a decrease in behavior, but not as much as punish



% ACTIONS - Things the bot can do.  Each action consists of a name, state preconditions and state outcome.
action(eat(X), [hungry,food(X)], []).
action(drop(X), [item(X),holding(X)], []).
action(grab(X), [item(X)], []).
action(press(X), [object(X)], []).

% STATE - The current state of the environment, including the bot.  States can include:
%   hungry, holding(X)  
state([food(cheese),item(box)]).

% BEHAVIORAL ADJUSTMENT - Behavior is adjusted by adding the adjustment value to the bot's current
% condition value
adjust_behavior(Action,Stimulus) :-
    condition(Action,Old_Value,Response),
    New_Value is Old_Value+Stimulus,
    retract(condition(Action,Old_Value,Response)),
    assert(condition(Action,New_Value,Response)).

adjust_behavior(Action,Stimulus) :-
    assert(condition(Action,Stimulus,Response)).

select_action(State) :- 
    % select the action which best fits the current state
    action(Name, Preconditions),
    conditions_met(Preconditions,State),
    write(Name), nl,
    set_last_action(Name).

conditions_met(P,S) :- subset(P,S).

check_behavior(Action) :-
    condition(Action,S,R),
    Ratio is S/R.


set_last_action(Action) :-
    last_action(Prev_Action),
    retract(last_action(Prev_Action)),
    assert(last_action(Action)).
set_last_action(Action) :-
    assert(last_action(Action)).



% trigger select action when there is a change in STATE or ENVIRONMENT

% set hunger timer

/*
go_init([A|L]) :-
    go([A|L],A).
go([A|[]],X) :-
    times_called(A,N),
    times_called(X,M),
    N >= M,
    write(A).
go([A|[]],X) :-
    times_called(A,N),
    times_called(X,M),
    N < M,
    write(X).
go([A|L],X) :-
    times_called(A,N),
    times_called(X,M),
    N >= M,
    go(L,A).
go([A|L],X) :-
    times_called(A,N),
    times_called(X,M),
    N < M,
    go(L,X).
*/

% Determining preference
%   Any action should be preferred if:
%   - The environmental conditions for the action are met
%   - The action has a positive value greater than the standard deviation
%   Any action should be avoided if:
%   - The environmental conditions are not met
%   - The action has a negative value greater than the standard deviation
/*
get_random :-
    actions(L),
    random_member(X,L),
    increment_action(X),
    retract(actions(L)),
    assert(actions([X|L])),
    write(X),!.

get_random :-
    write('no valid action').

run_test :-
    actions(A),
    go_init(A).

report_times([A|[]]) :-
    times_called(A,N),
    write(A),write(' called '),write(N),write(' times.'),nl.
report_times([A|L]) :-
    times_called(A,N),
    write(A),write(' called '),write(N),write(' times.'),nl,
    report_times(L).

main :- 
    actions(A),
    instantiate_actions(A).

:- main.

*/




/*
input :-
    read(Stimulus),
    Stimulus = quit,
    stop.

get_input :-
    read(Stimulus),
    ansi_format([bold,fg(red)], 'You input ~w', [Stimulus]),
    get_input.

stop :-
    write('You have stopped.'),nl.

main :-
    get_input.
*/