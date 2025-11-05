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
    format('RULE ~w FIRED at time ~3f~n', [RuleNumber, Timestamp]).

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
% Static pattern definitions (not tracked - just data)
% =============================================================================

% Which directions are served (any movement) by a pattern
serves_dirs(1,  [north, south]).        % N/S straight corridor
serves_dirs(2,  [east, west]).          % E/W straight corridor  
serves_dirs(3,  [north]).               % North straight/turns
serves_dirs(4,  [south]).               % South straight/turns
serves_dirs(5,  [east]).                % East straight/turns
serves_dirs(6,  [west]).                % West straight/turns
serves_dirs(7,  [north, east]).         % Combo (non-conflicting)
serves_dirs(8,  [north, west]).         % Combo (non-conflicting)
serves_dirs(9,  [south, east]).         % Combo (non-conflicting)
serves_dirs(10, [south, west]).         % Combo (non-conflicting)
serves_dirs(11, [north, south]).        % All N/S
serves_dirs(12, [east, west]).          % All E/W

% Movement permissions for each pattern (used by "best pattern" search)
pattern_allows(3,  north, straight).  
pattern_allows(3,  north, left).  
pattern_allows(3,  north, right).

pattern_allows(4,  south, straight).  
pattern_allows(4,  south, left).  
pattern_allows(4,  south, right).

pattern_allows(5,  east,  straight).  
pattern_allows(5,  east,  left).  
pattern_allows(5,  east,  right).

pattern_allows(6,  west,  straight).  
pattern_allows(6,  west,  left).  
pattern_allows(6,  west,  right).

pattern_allows(7,  north, straight).  
pattern_allows(7,  north, left).  
pattern_allows(7,  north, right).
pattern_allows(7,  east,  straight).  
pattern_allows(7,  east,  left).  
pattern_allows(7,  east,  right).

pattern_allows(8,  north, straight).  
pattern_allows(8,  north, left).  
pattern_allows(8,  north, right).
pattern_allows(8,  west,  straight).  
pattern_allows(8,  west,  left).  
pattern_allows(8,  west,  right).

pattern_allows(9,  south, straight).  
pattern_allows(9,  south, left).  
pattern_allows(9,  south, right).
pattern_allows(9,  east,  straight).  
pattern_allows(9,  east,  left).  
pattern_allows(9,  east,  right).

pattern_allows(10, south, straight).  
pattern_allows(10, south, left).  
pattern_allows(10, south, right).
pattern_allows(10, west,  straight).  
pattern_allows(10, west,  left).  
pattern_allows(10, west,  right).

pattern_allows(11, north, straight).  
pattern_allows(11, north, left).  
pattern_allows(11, north, right).
pattern_allows(11, south, straight).  
pattern_allows(11, south, left).  
pattern_allows(11, south, right).

pattern_allows(12, east,  straight).  
pattern_allows(12, east,  left).  
pattern_allows(12, east,  right).
pattern_allows(12, west,  straight).  
pattern_allows(12, west,  left).  
pattern_allows(12, west,  right).

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
% CORE DECISION LOGIC - INTELLIGENT TRAFFIC CONTROL RULES
% =============================================================================

% RULE 1: Cycle through patterns (fallback when no intelligent choice)
cycle_pattern(P, Next) :- 
    track_rule(1),
    (P >= 12 -> Next = 1 ; Next is P + 1).

% RULE 2: Turn-heavy traffic management
select_turn_pattern(Queue, TurnRatio, Dir, Pattern) :-
    Queue > 8,
    TurnRatio > 0.35,
    track_rule(2),
    turn_pattern_for_direction(Dir, Pattern).

turn_pattern_for_direction(north, 3).
turn_pattern_for_direction(south, 4).
turn_pattern_for_direction(east,  5).
turn_pattern_for_direction(west,  6).

% RULE 3: Balanced perpendicular traffic (combination patterns)
select_combination_pattern(QN, QS, QE, QW, Pattern) :-
    track_rule(3),
    (QN >= QS, QE >= QW -> Pattern = 7
    ; QN >= QS, QW >= QE -> Pattern = 8
    ; QS >= QN, QE >= QW -> Pattern = 9
    ; QS >= QN, QW >= QE -> Pattern = 10
    ; Pattern = 11).

% RULE 4: Main intelligent pattern selection algorithm
select_next_pattern(CurrentPattern, QN, QS, QE, QW, TN, TS, TE, TW, NextPattern) :-
    track_rule(4),
    TotalNS is QN + QS,
    TotalEW is QE + QW,
    TotalAll is TotalNS + TotalEW,

    (QN > 0 -> RN is TN / QN ; RN = 0),
    (QS > 0 -> RS is TS / QS ; RS = 0),
    (QE > 0 -> RE is TE / QE ; RE = 0),
    (QW > 0 -> RW is TW / QW ; RW = 0),

    % Priority for turn-heavy directions
    ( select_turn_pattern(QN, RN, north, NextPattern) -> track_rule(4.1)
    ; select_turn_pattern(QS, RS, south, NextPattern) -> track_rule(4.2)
    ; select_turn_pattern(QE, RE, east,  NextPattern) -> track_rule(4.3)
    ; select_turn_pattern(QW, RW, west,  NextPattern) -> track_rule(4.4)
    % Balanced traffic gets combination patterns
    ; (TotalAll > 8, abs(TotalNS - TotalEW) < 8) ->
        (select_combination_pattern(QN, QS, QE, QW, NextPattern), track_rule(4.5))
    ; (TotalNS > 5, TotalEW > 5) ->
        (select_combination_pattern(QN, QS, QE, QW, NextPattern), track_rule(4.6))
    % Axis dominance gets full axis patterns
    ; TotalNS > TotalEW * 1.5, TotalNS > 10 -> (NextPattern = 11, track_rule(4.7))
    ; TotalEW > TotalNS * 1.5, TotalEW > 10 -> (NextPattern = 12, track_rule(4.8))
    % Moderate dominance gets straight patterns
    ; TotalNS > TotalEW -> (NextPattern = 1, track_rule(4.9))
    ; TotalEW > TotalNS -> (NextPattern = 2, track_rule(4.10))
    % Fallback to round-robin
    ; (cycle_pattern(CurrentPattern, NextPattern), track_rule(4.11))
    ).

% RULE 5: Pattern decision wrapper (reads queue facts from Python)
decide_next_pattern(CurrentPattern, NextPattern) :-
    track_rule(5),
    queue_for_direction(north, QN),
    queue_for_direction(south, QS),
    queue_for_direction(east,  QE),
    queue_for_direction(west,  QW),
    (turn_demand(north, TN, _) -> true ; TN = 0),
    (turn_demand(south, TS, _) -> true ; TS = 0),
    (turn_demand(east,  TE, _) -> true ; TE = 0),
    (turn_demand(west,  TW, _) -> true ; TW = 0),
    select_next_pattern(CurrentPattern, QN, QS, QE, QW, TN, TS, TE, TW, NextPattern).

% RULE 6: Early pattern termination (no demand in served directions)
should_change_early(Pattern) :-
    track_rule(6),
    serves_dirs(Pattern, Dirs),
    forall(member(D, Dirs), (queue_for_direction(D, Q), Q =:= 0)).

% RULE 7: Performance-based pattern extension (pattern serving well)
pattern_serving_well(Pattern) :-
    track_rule(7),
    serves_dirs(Pattern, Dirs),
    member(D, Dirs),
    queue_for_direction(D, Q), Q > 5,
    in_rate(D, I), service_rate(D, S),
    S > I * 0.7, !.

% RULE 8: Time extension logic (queue growing faster than service)
time_extension_decision(Pattern, ElapsedFrames, ExtendBy) :-
    track_rule(8),
    ElapsedFrames > 0, ElapsedFrames =< 60,
    pick_dirs(Pattern, D1, _),
    queue_for_direction(D1, Q1), Q1 > 8,
    in_rate(D1, I1), service_rate(D1, S1),
    I1 > S1 * 0.8,
    state_greentime(Tg), max_greentime(Tmax), Tg < Tmax,
    greentime_extension(ExtendBy).

% RULE 9: Maximum time safety override (force pattern change)
max_time_override :- 
    track_rule(9),
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