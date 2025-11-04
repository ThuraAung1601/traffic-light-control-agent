% =============================================================================
% Centralized Pattern Selection Algorithm for Traffic Light Control
% =============================================================================

:- dynamic current_state/1.
:- dynamic state_counter/1.
:- dynamic state_greentime/1.
:- dynamic queue_length/2.           % queue_length(Direction, Count)
:- dynamic incoming_rate/2.          % incoming_rate(Direction, veh_per_step)
:- dynamic queue_service_rate/2.     % queue_service_rate(Direction, veh_per_step)
:- dynamic downstream_size/2.        % downstream_size(Direction, Vehicles)
:- dynamic downstream_maxsize/2.     % downstream_maxsize(Direction, Capacity)
:- dynamic working_memory/1.         % used by supervisor helpers
:- dynamic turning_cars/2.           % turning_cars(Direction, Count)
:- dynamic turn_demand/3.            % turn_demand(Direction, TurnCount, QueueLength)
:- dynamic intersection_occupancy/2. % intersection_occupancy(Junction, Count)
:- dynamic rule_fired/2.             % rule_fired(RuleNumber, Timestamp)

% Timing parameters (frames)
max_greentime(600).
greentime_extension(60).
pattern_duration(240).

% =============================================================================
% RULE TRACKING SYSTEM
% =============================================================================

% Track when rules are fired
track_rule(RuleNumber) :-
    get_time(Timestamp),
    assertz(rule_fired(RuleNumber, Timestamp)),
    format('🔥 RULE ~w FIRED at time ~3f~n', [RuleNumber, Timestamp]).

% Get recently fired rules
get_recent_rules(Rules) :-
    get_time(Now),
    findall(R, (rule_fired(R, T), Now - T < 10), Rules).

% Clear old rule tracking
cleanup_old_rules :-
    get_time(Now),
    retractall(rule_fired(_, T)),
    T < Now - 60.  % Remove rules older than 60 seconds

% =============================================================================
% PATTERN DEFINITIONS (12-pattern system)
% Directions: north | south | east | west
% PAPER RULE 1: Pattern-based Traffic Light Control
% =============================================================================

% Which directions are served (any movement) by a pattern
% PAPER RULE 1a: Basic pattern definitions for intersection control
serves_dirs(1,  [north, south]).        % PAPER RULE 1a-1: N/S straight corridor
serves_dirs(2,  [east, west]).          % PAPER RULE 1a-2: E/W straight corridor  
serves_dirs(3,  [north]).               % PAPER RULE 1a-3: North straight/turns
serves_dirs(4,  [south]).               % PAPER RULE 1a-4: South straight/turns
serves_dirs(5,  [east]).                % PAPER RULE 1a-5: East straight/turns
serves_dirs(6,  [west]).                % PAPER RULE 1a-6: West straight/turns
serves_dirs(7,  [north, east]).         % PAPER RULE 1a-7: Combo (non-conflicting)
serves_dirs(8,  [north, west]).         % PAPER RULE 1a-8: Combo (non-conflicting)
serves_dirs(9,  [south, east]).         % PAPER RULE 1a-9: Combo (non-conflicting)
serves_dirs(10, [south, west]).         % PAPER RULE 1a-10: Combo (non-conflicting)
serves_dirs(11, [north, south]).        % PAPER RULE 1a-11: All N/S
serves_dirs(12, [east, west]).          % PAPER RULE 1a-12: All E/W

% PAPER RULE 1b: Movement permissions for each pattern (used by "best pattern" search)
pattern_allows(3,  north, straight).  pattern_allows(3,  north, left).  pattern_allows(3,  north, right).
pattern_allows(4,  south, straight).  pattern_allows(4,  south, left).  pattern_allows(4,  south, right).
pattern_allows(5,  east,  straight).  pattern_allows(5,  east,  left).  pattern_allows(5,  east,  right).
pattern_allows(6,  west,  straight).  pattern_allows(6,  west,  left).  pattern_allows(6,  west,  right).
pattern_allows(7,  north, straight).  pattern_allows(7,  north, left).  pattern_allows(7,  north, right).
pattern_allows(7,  east,  straight).  pattern_allows(7,  east,  left).  pattern_allows(7,  east,  right).
pattern_allows(8,  north, straight).  pattern_allows(8,  north, left).  pattern_allows(8,  north, right).
pattern_allows(8,  west,  straight).  pattern_allows(8,  west,  left).  pattern_allows(8,  west,  right).
pattern_allows(9,  south, straight).  pattern_allows(9,  south, left).  pattern_allows(9,  south, right).
pattern_allows(9,  east,  straight).  pattern_allows(9,  east,  left).  pattern_allows(9,  east,  right).
pattern_allows(10, south, straight).  pattern_allows(10, south, left).  pattern_allows(10, south, right).
pattern_allows(10, west,  straight).  pattern_allows(10, west,  left).  pattern_allows(10, west,  right).
pattern_allows(11, north, straight).  pattern_allows(11, north, left).  pattern_allows(11, north, right).
pattern_allows(11, south, straight).  pattern_allows(11, south, left).  pattern_allows(11, south, right).
pattern_allows(12, east,  straight).  pattern_allows(12, east,  left).  pattern_allows(12, east,  right).
pattern_allows(12, west,  straight).  pattern_allows(12, west,  left).  pattern_allows(12, west,  right).

% =============================================================================
% SAFE DEFAULTS AND UTILITY PREDICATES
% =============================================================================

% Safe accessor predicates with default values (defensive programming)
queue_for_direction(Dir, Q) :- queue_length(Dir, Q), !.
queue_for_direction(_, 0).

service_rate(Dir, S) :- queue_service_rate(Dir, S), !.
service_rate(_, 0).

in_rate(Dir, I) :- incoming_rate(Dir, I), !.
in_rate(_, 0).

% Extract primary and secondary directions from pattern (used in time extension logic)
pick_dirs(P, D1, D2) :-
    serves_dirs(P, Dirs),
    ( Dirs = [A,B|_] -> D1 = A, D2 = B
    ; Dirs = [A]    -> D1 = A, D2 = A
    ; D1 = north, D2 = south % fallback, should not happen
    ).

% Check if direction is served by pattern
is_served(Direction, Pattern) :-
    serves_dirs(Pattern, Dirs),
    member(Direction, Dirs).

% downstream free space helper
downstream_space(Dir, Space) :-
    downstream_maxsize(Dir, Max),
    downstream_size(Dir, Size), !,
    Space is max(0, Max - Size).
downstream_space(_, 9999).

% =============================================================================
% CORE DECISION LOGIC
% PAPER RULE 2: Intelligent Pattern Selection Based on Traffic Demand
% =============================================================================

% PAPER RULE 2a: Cycle through patterns when no intelligent choice available
cycle_pattern(P, Next) :- 
    track_rule(2.1),  % Track rule firing
    (P >= 12 -> Next = 1 ; Next is P + 1).

% PAPER RULE 3: Turn-Heavy Traffic Management
% PAPER RULE 3a: Select dedicated turn patterns for high turn demand
select_turn_pattern(Queue, TurnRatio, Dir, Pattern) :-
    Queue > 8,
    TurnRatio > 0.35,
    track_rule(3.1),  % Track rule firing
    turn_pattern_for_direction(Dir, Pattern).

turn_pattern_for_direction(north, 3).
turn_pattern_for_direction(south, 4).
turn_pattern_for_direction(east,  5).
turn_pattern_for_direction(west,  6).

% PAPER RULE 4: Balanced Perpendicular Traffic Management
% PAPER RULE 4a: Combination patterns for balanced cross-traffic
select_combination_pattern(QN, QS, QE, QW, Pattern) :-
    track_rule(4.1),  % Track rule firing
    (QN >= QS, QE >= QW -> Pattern = 7
    ; QN >= QS, QW >= QE -> Pattern = 8
    ; QS >= QN, QE >= QW -> Pattern = 9
    ; QS >= QN, QW >= QE -> Pattern = 10
    ; Pattern = 11).

% PAPER RULE 5: Main Intelligent Pattern Selection Algorithm
% This is the core algorithm from the paper for traffic-responsive control
select_next_pattern(CurrentPattern, QN, QS, QE, QW, TN, TS, TE, TW, NextPattern) :-
    track_rule(5.0),  % Track main rule firing
    TotalNS is QN + QS,
    TotalEW is QE + QW,
    TotalAll is TotalNS + TotalEW,

    (QN > 0 -> RN is TN / QN ; RN = 0),
    (QS > 0 -> RS is TS / QS ; RS = 0),
    (QE > 0 -> RE is TE / QE ; RE = 0),
    (QW > 0 -> RW is TW / QW ; RW = 0),

    % PAPER RULE 5a: Priority for turn-heavy directions
    ( select_turn_pattern(QN, RN, north, NextPattern) -> track_rule(5.1)
    ; select_turn_pattern(QS, RS, south, NextPattern) -> track_rule(5.2)
    ; select_turn_pattern(QE, RE, east,  NextPattern) -> track_rule(5.3)
    ; select_turn_pattern(QW, RW, west,  NextPattern) -> track_rule(5.4)
    % PAPER RULE 5b: Balanced traffic gets combination patterns
    ; (TotalAll > 8, abs(TotalNS - TotalEW) < 8) ->
        (select_combination_pattern(QN, QS, QE, QW, NextPattern), track_rule(5.5))
    ; (TotalNS > 5, TotalEW > 5) ->
        (select_combination_pattern(QN, QS, QE, QW, NextPattern), track_rule(5.6))
    % PAPER RULE 5c: Axis dominance gets full axis patterns
    ; TotalNS > TotalEW * 1.5, TotalNS > 10 -> (NextPattern = 11, track_rule(5.7))
    ; TotalEW > TotalNS * 1.5, TotalEW > 10 -> (NextPattern = 12, track_rule(5.8))
    % PAPER RULE 5d: Moderate dominance gets straight patterns
    ; TotalNS > TotalEW -> (NextPattern = 1, track_rule(5.9))
    ; TotalEW > TotalNS -> (NextPattern = 2, track_rule(5.10))
    % PAPER RULE 5e: Fallback to round-robin
    ; (cycle_pattern(CurrentPattern, NextPattern), track_rule(5.11))
    ).

% Wrapper that reads facts asserted from Python
decide_next_pattern(CurrentPattern, NextPattern) :-
    track_rule(5.12),  % Track wrapper call
    queue_for_direction(north, QN),
    queue_for_direction(south, QS),
    queue_for_direction(east,  QE),
    queue_for_direction(west,  QW),
    (turn_demand(north, TN, _) -> true ; TN = 0),
    (turn_demand(south, TS, _) -> true ; TS = 0),
    (turn_demand(east,  TE, _) -> true ; TE = 0),
    (turn_demand(west,  TW, _) -> true ; TW = 0),
    select_next_pattern(CurrentPattern, QN, QS, QE, QW, TN, TS, TE, TW, NextPattern).

% PAPER RULE 6: Early Pattern Termination
% PAPER RULE 6a: Switch early when no demand in served directions
should_change_early(Pattern) :-
    track_rule(6.1),  % Track rule firing
    serves_dirs(Pattern, Dirs),
    forall(member(D, Dirs), (queue_for_direction(D, Q), Q =:= 0)).

% PAPER RULE 7: Performance-Based Pattern Extension
% PAPER RULE 7a: Pattern is serving demand well - keep it longer
pattern_serving_well(Pattern) :-
    track_rule(7.1),  % Track rule firing
    serves_dirs(Pattern, Dirs),
    member(D, Dirs),
    queue_for_direction(D, Q), Q > 5,
    in_rate(D, I), service_rate(D, S),
    S > I * 0.7, !.

% PAPER RULE 8: Time Extension Logic
% PAPER RULE 8a: Extend green time when queue is growing faster than service
time_extension_decision(Pattern, ElapsedFrames, ExtendBy) :-
    track_rule(8.1),  % Track rule firing
    ElapsedFrames > 0, ElapsedFrames =< 60,
    pick_dirs(Pattern, D1, _),
    queue_for_direction(D1, Q1), Q1 > 8,
    in_rate(D1, I1), service_rate(D1, S1),
    I1 > S1 * 0.8,
    state_greentime(Tg), max_greentime(Tmax), Tg < Tmax,
    greentime_extension(ExtendBy).

% PAPER RULE 9: Maximum Time Safety Override
% PAPER RULE 9a: Force pattern change when maximum time exceeded
max_time_override :- 
    track_rule(9.1),  % Track rule firing
    state_greentime(Tg), max_greentime(Tmax), Tg > Tmax.

% Find best pattern that serves a direction
find_best_pattern_for_direction(Dir, Pattern) :-
    ( pattern_allows(Pattern, Dir, straight),
      pattern_allows(Pattern, Dir, left),
      pattern_allows(Pattern, Dir, right), !
    ; pattern_allows(Pattern, Dir, straight), !
    ; pattern_allows(Pattern, Dir, _), !
    ).

% Pick any pattern that avoids a problematic direction
find_alternative_pattern(AvoidDir, Pattern) :-
    serves_dirs(Pattern, Dirs),
    \+ member(AvoidDir, Dirs).

% =============================================================================
% SUPERVISOR HELPERS (deadlock detection and recommendation)
% =============================================================================

all_current_full_flows(Flow_List) :-
    findall([Junction, Pattern, Occupancy],
            ( working_memory(junction_state(Junction, Pattern, _, _, _)),
              working_memory(junction_occupancy(Junction, Occupancy)),
              Occupancy > 0.6),
            Flow_List),
    length(Flow_List, N), N >= 3.

possible_loops(Flow_List, [Flow_List]) :-
    length(Flow_List, N), N >= 3.

% Choose weakest (min occupancy) junction
weakest_junction([[J,O]|Rest], BestJ, BestO) :-
    ( Rest = [] -> BestJ = J, BestO = O
    ; weakest_junction(Rest, RJ, RO),
      (O =< RO -> BestJ = J, BestO = O ; BestJ = RJ, BestO = RO)
    ).

% Recommend a pattern change for weakest junction (pure, no asserts)
supervisor_recommendation(WeakestJunction, NewPattern) :-
    all_current_full_flows(Flow_List),
    possible_loops(Flow_List, _),
    findall([J,Occ],
            (member([J,_,Occ], Flow_List)),
            Pairs),
    weakest_junction(Pairs, WeakestJunction, _),
    % read current pattern from working_memory
    working_memory(junction_state(WeakestJunction, OldPattern, _, _, _)),
    select_alternative_pattern(WeakestJunction, OldPattern, NewPattern).

% Select an alternative pattern (switch axis or next)
select_alternative_pattern(_, Old, New) :-
    Old < 6, !, New is Old + 6.
select_alternative_pattern(_, Old, New) :-
    New is ((Old + 1) mod 12) + 1.