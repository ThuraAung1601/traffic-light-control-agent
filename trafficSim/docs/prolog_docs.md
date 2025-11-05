# Traffic Signal Control System - Prolog Rules Documentation

## Overview

This document explains the Prolog-based intelligent traffic signal control system. The system uses **dynamic facts** (data that changes during simulation) and **static rules** (decision logic) to select optimal traffic light patterns for a 4-way intersection.

---

## Table of Contents

1. [Dynamic Facts (Real-time Data)](#dynamic-facts)
2. [Static Facts (Configuration)](#static-facts)
3. [Traffic Signal Patterns](#traffic-signal-patterns)
4. [Intelligent Decision Rules](#intelligent-decision-rules)
5. [Rule Firing Sequence](#rule-firing-sequence)
6. [Quick Reference Tables](#quick-reference-tables)

---

## Dynamic Facts (Real-time Data)

These facts are continuously updated by the Python simulation and represent the current state of traffic.

### `current_state(PatternNumber)`
**What it stores:** The currently active traffic light pattern (1-12)  
**Example:** `current_state(3)` means Pattern 3 is currently active  
**Plain English:** "Which traffic light pattern is running right now?"

### `state_counter(Frames)`
**What it stores:** How long the current pattern has been active (in frames)  
**Example:** `state_counter(120)` means 120 frames have passed  
**Plain English:** "How many frames has this green light been on?"

### `state_greentime(Frames)`
**What it stores:** Total green time accumulated for the current pattern  
**Example:** `state_greentime(450)` means 450 frames of green time  
**Plain English:** "How long has this direction had green light in total?"

### `queue_length(Direction, Count)`
**What it stores:** Number of vehicles waiting in each direction  
**Example:** `queue_length(north, 15)` means 15 cars waiting northbound  
**Plain English:** "How many cars are waiting in each direction?"  
**Directions:** `north`, `south`, `east`, `west`

### `incoming_rate(Direction, VehiclesPerSecond)`
**What it stores:** Rate of vehicles arriving at each direction  
**Example:** `incoming_rate(east, 0.5)` means 0.5 vehicles/second arriving from east  
**Plain English:** "How fast are cars arriving from each direction?"

### `queue_service_rate(Direction, VehiclesPerSecond)`
**What it stores:** Rate at which vehicles are leaving the queue (being served)  
**Example:** `queue_service_rate(north, 0.8)` means 0.8 vehicles/second departing  
**Plain English:** "How fast are cars leaving the queue when they have green light?"

### `downstream_size(Direction, Count)`
**What it stores:** Number of vehicles currently in the downstream road section  
**Example:** `downstream_size(south, 5)` means 5 cars on the outbound road  
**Plain English:** "How many cars are on the exit road?"

### `downstream_maxsize(Direction, Capacity)`
**What it stores:** Maximum capacity of downstream road section  
**Example:** `downstream_maxsize(west, 10)` means west exit can hold 10 cars  
**Plain English:** "What's the maximum number of cars the exit road can hold?"

### `turning_cars(Direction, Count)`
**What it stores:** Number of vehicles wanting to turn from each direction  
**Example:** `turning_cars(north, 8)` means 8 northbound cars want to turn  
**Plain English:** "How many cars want to turn instead of going straight?"

### `turn_demand(Direction, TurnCount, QueueLength)`
**What it stores:** Complete turn demand information for each direction  
**Example:** `turn_demand(east, 6, 12)` means 6 out of 12 eastbound cars want to turn  
**Plain English:** "What's the turn demand ratio for each direction?"

### `intersection_occupancy(Junction, Count)`
**What it stores:** How many vehicles are currently inside the intersection  
**Example:** `intersection_occupancy(center, 3)` means 3 cars in intersection  
**Plain English:** "How crowded is the intersection right now?"

### `rule_fired(RuleNumber, Timestamp)`
**What it stores:** Which rules have been triggered and when  
**Example:** `rule_fired(4.1, 1762314408.203)` means Rule 4.1 fired at that time  
**Plain English:** "Which decision rules were used recently?"

---

## Static Facts (Configuration)

These facts define the system's behavior and don't change during simulation.

### Timing Parameters

```prolog
max_greentime(600).          % Maximum green time: 600 frames
greentime_extension(60).     % Extension amount: 60 frames
pattern_duration(240).       % Default pattern duration: 240 frames
```

**Plain English:**
- "Maximum green light time is 600 frames (about 10 seconds)"
- "When extending, add 60 frames (1 second)"
- "Default pattern runs for 240 frames (4 seconds)"

---

## Traffic Signal Patterns

### Pattern Definitions: `serves_dirs(Pattern, Directions)`

Each pattern serves specific directions with green lights:

| Pattern | Directions Served | Type | Description |
|---------|-------------------|------|-------------|
| 1 | `[north, south]` | Corridor | North and South straight through |
| 2 | `[east, west]` | Corridor | East and West straight through |
| 3 | `[north]` | Single | North all movements (straight, left, right) |
| 4 | `[south]` | Single | South all movements |
| 5 | `[east]` | Single | East all movements |
| 6 | `[west]` | Single | West all movements |
| 7 | `[north, east]` | Combo | North + East (non-conflicting) |
| 8 | `[north, west]` | Combo | North + West (non-conflicting) |
| 9 | `[south, east]` | Combo | South + East (non-conflicting) |
| 10 | `[south, west]` | Combo | South + West (non-conflicting) |
| 11 | `[north, south]` | Full Axis | All North-South movements |
| 12 | `[east, west]` | Full Axis | All East-West movements |

**Plain English:** "Each pattern number corresponds to which directions get green lights"

### Movement Permissions: `pattern_allows(Pattern, Direction, Movement)`

Defines exactly which movements are allowed in each pattern.

**Example:**
```prolog
pattern_allows(3, north, straight).  % Pattern 3 allows north straight
pattern_allows(3, north, left).      % Pattern 3 allows north left turn
pattern_allows(3, north, right).     % Pattern 3 allows north right turn
```

**Plain English:** "Pattern 3 allows northbound cars to go straight, turn left, or turn right"

---

## Intelligent Decision Rules

### **Rule 1: Cycle Pattern (Fallback)**

```prolog
cycle_pattern(P, Next) :- 
    track_rule(1),
    (P >= 12 -> Next = 1 ; Next is P + 1).
```

**When it's used:** When no intelligent decision can be made  
**What it does:** Moves to the next pattern in sequence (1→2→3...→12→1)  
**Plain English:** "If you don't know what to do, just rotate through all patterns in order"

---

### **Rule 2: Turn-Heavy Traffic Management**

```prolog
select_turn_pattern(Queue, TurnRatio, Dir, Pattern) :-
    Queue > 8,
    TurnRatio > 0.35,
    track_rule(2),
    turn_pattern_for_direction(Dir, Pattern).
```

**When it's used:** When a direction has many cars wanting to turn  
**Conditions:**
- Queue length > 8 vehicles
- More than 35% want to turn (TurnRatio > 0.35)

**What it does:** Activates that direction's dedicated turn pattern (3, 4, 5, or 6)  
**Plain English:** "If more than 8 cars are waiting and over 35% want to turn, give them a dedicated turn phase"

**Direction → Pattern Mapping:**
- North → Pattern 3
- South → Pattern 4
- East → Pattern 5
- West → Pattern 6

---

### **Rule 3: Balanced Cross Traffic (Combination Patterns)**

```prolog
select_combination_pattern(QN, QS, QE, QW, Pattern) :-
    track_rule(3),
    (QN >= QS, QE >= QW -> Pattern = 7
    ; QN >= QS, QW >= QE -> Pattern = 8
    ; QS >= QN, QE >= QW -> Pattern = 9
    ; QS >= QN, QW >= QE -> Pattern = 10
    ; Pattern = 11).
```

**When it's used:** When traffic is balanced between perpendicular directions  
**What it does:** Selects a combination pattern that serves two non-conflicting directions  
**Plain English:** "If traffic is fairly even on both axes, serve two directions at once"

**Selection Logic:**
- If North ≥ South AND East ≥ West → Pattern 7 (North + East)
- If North ≥ South AND West ≥ East → Pattern 8 (North + West)
- If South ≥ North AND East ≥ West → Pattern 9 (South + East)
- If South ≥ North AND West ≥ East → Pattern 10 (South + West)
- Otherwise → Pattern 11 (All North-South)

---

### **Rule 4: Main Intelligent Pattern Selection Algorithm**

```prolog
select_next_pattern(CurrentPattern, QN, QS, QE, QW, TN, TS, TE, TW, NextPattern)
```

**When it's used:** This is the **core decision engine** called every time a pattern change is needed  
**Inputs:**
- Current pattern number
- Queue lengths for all 4 directions (QN, QS, QE, QW)
- Turn counts for all 4 directions (TN, TS, TE, TW)

**Decision Priority (in order):**

1. **Check for turn-heavy traffic** (Rule 2)
   - North heavy turns → Pattern 3 (Rule 4.1)
   - South heavy turns → Pattern 4 (Rule 4.2)
   - East heavy turns → Pattern 5 (Rule 4.3)
   - West heavy turns → Pattern 6 (Rule 4.4)

2. **Check for high balanced traffic**
   - Total traffic > 8 AND axes balanced (difference < 8) → Combination pattern (Rule 4.5)
   - Both NS > 5 AND EW > 5 → Combination pattern (Rule 4.6)

3. **Check for axis dominance**
   - NS total > 1.5× EW total AND NS > 10 → Pattern 11 (full NS) (Rule 4.7)
   - EW total > 1.5× NS total AND EW > 10 → Pattern 12 (full EW) (Rule 4.8)

4. **Check for moderate dominance**
   - NS slightly more → Pattern 1 (NS corridor) (Rule 4.9)
   - EW slightly more → Pattern 2 (EW corridor) (Rule 4.10)

5. **Fallback**
   - No clear winner → Cycle to next pattern (Rule 4.11)

**Plain English:** "Analyze all traffic data and pick the best pattern based on queue lengths, turn demands, and traffic balance"

---

### **Rule 5: Decision Wrapper (Integration with Python)**

```prolog
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
```

**When it's used:** Called by Python every time a pattern change is needed  
**What it does:** 
1. Reads all current queue and turn demand facts
2. Passes data to Rule 4 (main selection algorithm)
3. Returns the selected pattern to Python

**Plain English:** "This is the main interface that Python calls to ask 'what pattern should I use next?'"

---

### **Rule 6: Early Pattern Termination**

```prolog
should_change_early(Pattern) :-
    track_rule(6),
    serves_dirs(Pattern, Dirs),
    forall(member(D, Dirs), (queue_for_direction(D, Q), Q =:= 0)).
```

**When it's used:** Checked during pattern execution  
**Conditions:** ALL directions served by the pattern have ZERO waiting vehicles  
**What it does:** Signals that pattern can end early  
**Plain English:** "If nobody is waiting in the directions that have green light, switch to another pattern now"

**Example:** Pattern 3 serves North. If no northbound cars are waiting, end Pattern 3 early.

---

### **Rule 7: Performance-Based Pattern Extension**

```prolog
pattern_serving_well(Pattern) :-
    track_rule(7),
    serves_dirs(Pattern, Dirs),
    member(D, Dirs),
    queue_for_direction(D, Q), Q > 5,
    in_rate(D, I), service_rate(D, S),
    S > I * 0.7, !.
```

**When it's used:** Checked when considering pattern extension  
**Conditions:**
- Queue still has > 5 vehicles
- Service rate > 70% of incoming rate (S > I × 0.7)

**What it does:** Identifies patterns that are effectively clearing queues  
**Plain English:** "If this green light is helping clear a big queue efficiently, consider keeping it longer"

---

### **Rule 8: Time Extension Decision**

```prolog
time_extension_decision(Pattern, ElapsedFrames, ExtendBy) :-
    track_rule(8),
    ElapsedFrames > 0, ElapsedFrames =< 60,
    pick_dirs(Pattern, D1, _),
    queue_for_direction(D1, Q1), Q1 > 8,
    in_rate(D1, I1), service_rate(D1, S1),
    I1 > S1 * 0.8,
    state_greentime(Tg), max_greentime(Tmax), Tg < Tmax,
    greentime_extension(ExtendBy).
```

**When it's used:** During the first 60 frames of a pattern  
**Conditions:**
- Queue > 8 vehicles
- Incoming rate > 80% of service rate (queue growing)
- Current green time < maximum allowed

**What it does:** Extends green time by 60 frames  
**Plain English:** "If the queue is getting longer even with green light (cars arriving faster than leaving), give it more time"

---

### **Rule 9: Maximum Time Safety Override**

```prolog
max_time_override :- 
    track_rule(9),
    state_greentime(Tg), max_greentime(Tmax), Tg > Tmax.
```

**When it's used:** Checked every frame  
**Conditions:** Current green time exceeds maximum (600 frames)  
**What it does:** Forces immediate pattern change  
**Plain English:** "If a green light has been on too long (more than 600 frames), force it to change for fairness"

---

## Rule Firing Sequence

### Typical Decision Flow

```
1. Pattern needs to change (timer expired or early termination)
2. Python calls decide_next_pattern(CurrentPattern, NextPattern)
3. Rule 5 fires: Collect all queue and turn data
4. Rule 4 fires: Main selection algorithm
   ├─→ Rule 4.1-4.4: Check turn-heavy traffic (Rule 2)
   ├─→ Rule 4.5-4.6: Check balanced traffic (Rule 3)
   ├─→ Rule 4.7-4.8: Check axis dominance
   ├─→ Rule 4.9-4.10: Check moderate dominance
   └─→ Rule 4.11: Fallback to cycle (Rule 1)
5. NextPattern returned to Python
6. Pattern activates
7. During execution:
   ├─→ Rule 6 checks: Can we end early?
   ├─→ Rule 7 checks: Is it performing well?
   ├─→ Rule 8 checks: Should we extend time?
   └─→ Rule 9 checks: Have we exceeded max time?
```

---

## Quick Reference Tables

### Rules Summary

| Rule | Name | When Used | Action |
|------|------|-----------|--------|
| 1 | Cycle Pattern | No smart choice available | Move to next pattern |
| 2 | Turn-Heavy | Queue > 8, TurnRatio > 35% | Use turn pattern (3-6) |
| 3 | Balanced Traffic | Traffic balanced on both axes | Use combo pattern (7-10) |
| 4 | Main Selection | Every pattern change | Intelligent pattern choice |
| 5 | Wrapper | Called from Python | Interface to Rule 4 |
| 6 | Early Stop | No cars in served directions | End pattern early |
| 7 | Performance | Pattern clearing queue well | Consider extension |
| 8 | Extension | Queue growing despite green | Add 60 frames |
| 9 | Safety | Green time > 600 frames | Force change |

### Pattern Summary

| Pattern | Type | Serves | Use Case |
|---------|------|--------|----------|
| 1 | Corridor | N, S | NS traffic dominant |
| 2 | Corridor | E, W | EW traffic dominant |
| 3 | Single | N | North heavy turns |
| 4 | Single | S | South heavy turns |
| 5 | Single | E | East heavy turns |
| 6 | Single | W | West heavy turns |
| 7 | Combo | N, E | Balanced NE traffic |
| 8 | Combo | N, W | Balanced NW traffic |
| 9 | Combo | S, E | Balanced SE traffic |
| 10 | Combo | S, W | Balanced SW traffic |
| 11 | Full Axis | N, S | Heavy NS traffic |
| 12 | Full Axis | E, W | Heavy EW traffic |

### Key Thresholds

| Parameter | Value | Meaning |
|-----------|-------|---------|
| Queue threshold | 8 | "Large queue" definition |
| Turn ratio threshold | 0.35 | 35% wanting to turn |
| Axis dominance | 1.5× | One axis has 50% more traffic |
| Service efficiency | 0.7 | Serving 70% of incoming rate |
| Queue growth | 0.8 | Incoming 80% of service rate |
| Maximum green time | 600 frames | ~10 seconds |
| Extension amount | 60 frames | ~1 second |
| Default duration | 240 frames | ~4 seconds |

---

## Utility Predicates

### Safe Accessors (Defensive Programming)

```prolog
queue_for_direction(Dir, Q) :- queue_length(Dir, Q), !.
queue_for_direction(_, 0).  % Default to 0 if not found
```

**Plain English:** "Get queue length for a direction, return 0 if data is missing"

Similar safe accessors exist for:
- `service_rate(Dir, S)` - Get service rate, default 0
- `in_rate(Dir, I)` - Get incoming rate, default 0

### Helper Predicates

**`is_served(Direction, Pattern)`**  
Check if a direction is served by a pattern  
Example: `is_served(north, 3)` returns true

**`downstream_space(Dir, Space)`**  
Calculate free space in downstream road  
Example: Max 10, Current 3 → Space = 7

**`pick_dirs(Pattern, D1, D2)`**  
Extract primary and secondary directions from pattern  
Example: Pattern 7 → D1=north, D2=east

---

## Conclusion

This Prolog-based system provides intelligent, adaptive traffic signal control by:

1. **Monitoring** real-time traffic conditions (queues, flows, turns)
2. **Analyzing** patterns using 9 sophisticated rules
3. **Deciding** optimal signal patterns based on current conditions
4. **Adapting** dynamically to changing traffic demands
5. **Tracking** which rules fire for analysis and optimization

The system balances **efficiency** (clearing queues quickly), **fairness** (no direction waits too long), and **safety** (maximum time limits)
