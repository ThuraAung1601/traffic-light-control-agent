# Traffic Simulation System - Technical Documentation

## Overview

This document explains how the traffic simulation system works, from vehicle generation to traffic signal control. The system uses microscopic modeling where each vehicle is individually simulated with realistic behavior.

---

## Table of Contents

1. [System Architecture](#system-architecture)
2. [Vehicle Behavior (IDM Model)](#vehicle-behavior-idm-model)
3. [Road Network](#road-network)
4. [Vehicle Generation](#vehicle-generation)
5. [Turn Mechanics](#turn-mechanics)
6. [Traffic Signal System](#traffic-signal-system)
7. [Inductive Loop Detectors](#inductive-loop-detectors)
8. [Data Logging (ZODB)](#data-logging-zodb)
9. [Simulation Loop](#simulation-loop)

---

## System Architecture

### Core Components

```
Simulation (main.py)
    ├── Roads (road.py)
    │   └── Vehicles (vehicle.py)
    ├── Vehicle Generators (vehicle_generator.py)
    ├── Traffic Signals (traffic_signal.py)
    │   ├── Prolog Agent (prolog_agent.py)
    │   ├── Flow Estimators (inductive_loop.py)
    │   └── Pattern Rules (rules.pl)
    ├── ZODB Logger (db_logger.py)
    ├── Report Generator (report_generator.py)
    └── Visualization (window.py)
```

### Data Flow

```
1. Vehicles Generated → Added to Roads
2. Vehicles Follow IDM → Update Position/Velocity
3. Loop Detectors → Measure Flow/Turns
4. Prolog Agent → Decides Signal Pattern
5. Traffic Signals → Control Vehicle Movement
6. ZODB Logger → Records All Data
7. Report Generator → Creates Visualizations
```

---

## Vehicle Behavior (IDM Model)

### Intelligent Driver Model (IDM)

The simulation uses the **Intelligent-Driver Model**, a car-following model that produces realistic acceleration/deceleration behavior.

#### Vehicle Types

The system supports 4 vehicle types with different characteristics:

| Type | Length (m) | Width (m) | Max Speed (m/s) | Max Accel (m/s²) | Max Decel (m/s²) | Probability |
|------|-----------|-----------|-----------------|------------------|------------------|-------------|
| **Car** | 3 | 2 | 20 | 5 | 10 | 30% |
| **Truck** | 5 | 3 | 15 | 4 | 8 | 10% |
| **Bus** | 5 | 3 | 20 | 6 | 12 | 10% |
| **Motorcycle** | 2 | 1 | 25 | 7 | 20 | 50% |

**Plain English:** "Each vehicle type has different size, speed, and acceleration capabilities"

#### IDM Parameters

Each vehicle has these key parameters:

- **`s0`**: Minimum gap (safe following distance when stopped)
- **`T`**: Time headway (desired time gap to vehicle ahead)
- **`v_max`**: Maximum desired velocity
- **`a_max`**: Maximum acceleration
- **`b_max`**: Comfortable deceleration

**Plain English:** "These parameters control how aggressively or cautiously each vehicle drives"

#### IDM Acceleration Formula

The acceleration is calculated based on:

1. **Free-flow acceleration**: How fast the vehicle accelerates toward its desired speed
2. **Interaction term**: How the vehicle reacts to the car in front

```python
# Simplified logic:
if no_car_ahead:
    accelerate_toward_max_speed()
else:
    calculate_safe_following_distance()
    if too_close:
        decelerate()
    elif safe_distance:
        accelerate_moderately()
```

**Key Features:**

- **Collision prevention**: Vehicles brake hard when too close (emergency braking)
- **Smooth acceleration**: Gradual speed changes for realistic behavior
- **Gap maintenance**: Vehicles maintain safe following distances
- **Speed adaptation**: Vehicles slow down when approaching stopped traffic

**Plain English:** "Vehicles speed up when road is clear, slow down when following others, and brake hard to avoid collisions"

---

## Road Network

### Road Structure

Each road is defined by:
- **Start point**: (x, y) coordinates
- **End point**: (x, y) coordinates
- **Length**: Calculated using Euclidean distance
- **Angle**: Direction of the road (calculated from start/end)
- **Vehicles**: Queue (deque) of vehicles on that road

### Road Update Process

Each frame, roads update their vehicles:

1. **First vehicle** (front of queue):
   - Updates without a leading vehicle
   - Subject to traffic signal control

2. **Other vehicles**:
   - Update following the vehicle ahead
   - Maintain safe following distance using IDM

3. **Traffic signal interaction**:
   - **Green light**: Vehicles can pass freely
   - **Red light**: Vehicles enter "slowing zone" then "stop zone"

```
Road Layout:
[Start]----[Vehicles]----[Slowing Zone]----[Stop Zone]----[End]
                              ↓                    ↓
                         Reduce speed        Stop completely
```

**Plain English:** "Each road is a path where vehicles drive in single file, following the car ahead and obeying traffic signals"

---

## Vehicle Generation

### Generator Configuration

Vehicle generators create new vehicles at specified rates:

```python
vehicle_rate = 20  # Default: 20 vehicles per minute
```

**Adjustable via Control Panel:**
- Minimum: 60 vehicles/min (1 per second)
- Maximum: 600 vehicles/min (10 per second)

### Generation Logic

1. **Check timing**: Has enough time passed since last vehicle?
   ```python
   time_between_vehicles = 60 / vehicle_rate  # seconds
   ```

2. **Check space**: Is there room on the road?
   - Must have clear space ahead
   - Minimum gap: `s0 + vehicle_length + 2` meters

3. **Create vehicle**: Generate with random type based on probabilities

4. **Assign path**: Each vehicle gets a predefined route through the network

**Plain English:** "Vehicles are added to roads at regular intervals, but only if there's enough space"

### Vehicle Path System

Each vehicle has a **path** - a list of road indices to follow:

```python
vehicle.path = [0, 5, 10, 15]  # Drive through roads 0 → 5 → 10 → 15
vehicle.current_road_index = 0  # Currently on road 0
```

When a vehicle reaches the end of a road:
1. Check if it has more roads in its path
2. If yes: Transfer to next road
3. If no: Count as "passed" and remove from simulation

**Plain English:** "Each vehicle knows which roads to drive through, like following GPS directions"

---

## Turn Mechanics

### How Turns Work

Turns are implemented through the **path system**:

1. **Straight**: Path continues in same direction
   ```python
   path = [north_inbound, north_center, north_outbound]  # Go straight
   ```

2. **Left turn**: Path branches to left direction
   ```python
   path = [north_inbound, north_center, west_outbound]  # Turn left
   ```

3. **Right turn**: Path branches to right direction
   ```python
   path = [north_inbound, north_center, east_outbound]  # Turn right
   ```

### Turn Detection

The system tracks turning behavior using:

#### Inductive Loop Detectors

Detectors monitor vehicles and identify their movements:

```python
# When vehicle passes detector:
1. Record vehicle ID and path
2. Analyze path to determine movement:
   - If path continues straight → count as straight
   - If path branches left → count as left turn
   - If path branches right → count as right turn
```

#### Turn Demand Calculation

Turn demand is calculated per direction:

```python
turn_ratio = turn_count / total_count
# Example: 7 turning out of 20 vehicles = 0.35 (35% turn ratio)
```

This data feeds into the Prolog decision system to optimize signal patterns.

**Plain English:** "Detectors watch vehicles and figure out which way they're going - straight, left, or right"

### Turn Impact on Signal Patterns

High turn demand triggers specific patterns:

- **North heavy turns** (> 35%) → Pattern 3 (North all movements)
- **South heavy turns** (> 35%) → Pattern 4 (South all movements)
- **East heavy turns** (> 35%) → Pattern 5 (East all movements)
- **West heavy turns** (> 35%) → Pattern 6 (West all movements)

**Plain English:** "If many cars want to turn from one direction, give that direction a dedicated green phase"

---

## Traffic Signal System

### Signal Cycle Structure

A traffic signal controls 4 groups of roads (one per direction):

```python
signal_state = (West, South, East, North)
# Example: (True, False, False, False) means only West has green
```

### Control Modes

The traffic signal system supports two control modes:

#### 1. Classic Mode (Fallback)
- Fixed cycle through 4 patterns
- Random duration (20-40 seconds)
- Simple round-robin: West → South → East → North

#### 2. Prolog Mode (Intelligent) - **Default**
- Uses 12 sophisticated patterns
- Dynamic pattern selection based on:
  - Queue lengths
  - Turn demand ratios
  - Flow rates
  - Service rates
- Pattern duration: 240 frames (4 seconds) default
- Can extend up to 600 frames (10 seconds)

### How to Change Control Modes

#### Method 1: In Code (main.py)

When creating a traffic signal, specify the control mode:

```python
# For Prolog mode (intelligent, default):
sim.create_signal(roads=[[0,1,2], [3,4,5], [6,7,8], [9,10,11]], 
                  config={'control': 'prolog'})

# For Classic mode (simple round-robin):
sim.create_signal(roads=[[0,1,2], [3,4,5], [6,7,8], [9,10,11]], 
                  config={'control': 'rule'})
```

**Plain English:** "Add `config={'control': 'prolog'}` or `config={'control': 'rule'}` when creating the signal"

#### Method 2: In traffic_signal.py (Default Setting)

Change the default mode in the `TrafficSignal` class:

```python
def set_default_config(self):
    # ... other config ...
    
    # Change this line:
    self.control = 'prolog'  # Use 'prolog' or 'rule'
```

**Location:** `trafficSim/traffic_signal.py`, line ~48

#### Method 3: At Runtime (Dynamic Change)

You can change modes during simulation:

```python
# Access the traffic signal
signal = sim.traffic_signals[0]

# Switch to Classic mode
signal.control = 'rule'

# Switch to Prolog mode
signal.control = 'prolog'
signal._init_control_mode()  # Reinitialize Prolog agent
```

**Note:** Switching to Prolog mode at runtime requires reinitialization of the agent.

### Control Mode Selection Logic

The system automatically selects the appropriate mode:

```python
# Priority order:
1. If Prolog is available AND 4+ road groups → Use 'prolog'
2. If Prolog not available → Fallback to 'rule'
3. If fewer than 4 road groups → Use 'rule'
```

**Plain English:** "Prolog mode is used automatically if SWI-Prolog is installed and you have a 4-way intersection"

### Verifying Current Mode

Check which mode is active:

```python
# In your code:
signal = sim.traffic_signals[0]
current_mode = signal.control_type
print(f"Current control mode: {current_mode}")

# Returns: 'prolog' or 'rule'
```

### When to Use Each Mode

**Use Prolog Mode When:**
- You want intelligent, adaptive signal control
- You have complex traffic patterns
- Turn demand varies significantly
- You want to optimize for different traffic conditions
- You need detailed rule tracking and analysis

**Use Classic Mode When:**
- SWI-Prolog is not available
- You want simple, predictable behavior
- Testing baseline performance
- Debugging other system components
- You have a simple intersection with uniform traffic

### Troubleshooting Control Mode Issues

**Issue: Prolog mode not working**

1. Check if SWI-Prolog is installed:
   ```bash
   swipl --version
   ```

2. Check if pyswip is installed:
   ```bash
   pip list | grep pyswip
   ```

3. Check for errors in console:
   ```
   RuntimeError: pyswipl/SWI-Prolog not available
   ```

4. Verify rules.pl file exists:
   ```bash
   ls trafficSim/rules.pl
   ```

**Issue: Mode switches back to 'rule' automatically**

This happens when:
- Prolog initialization fails
- Road groups < 4
- Exception in Prolog agent creation

**Solution:** Check console output for error messages and verify Prolog installation.

### Control Mode Comparison

| Feature | Classic Mode | Prolog Mode |
|---------|-------------|-------------|
| Patterns | 4 fixed | 12 intelligent |
| Duration | Random (20-40s) | Adaptive (4-10s) |
| Turn handling | No | Yes (optimized) |
| Queue awareness | No | Yes (queue-based) |
| Flow rates | Not used | Monitored |
| Rule tracking | No | Yes (9 rules) |
| Extension logic | No | Yes (dynamic) |
| Early termination | No | Yes (when no demand) |
| Complexity | Low | High |
| Performance | Basic | Optimized |
| Setup required | None | SWI-Prolog + pyswip |

### Signal Zones

Each approach has three zones:

#### 1. Free Zone
- Normal driving
- Vehicles maintain desired speed

#### 2. Slowing Zone (last 50m)
- Activated when signal is RED
- Vehicles reduce speed to 40% of max speed
- Gradual deceleration

#### 3. Stop Zone (last 12m)
- Activated when signal is RED
- Vehicles must stop completely
- Wait for green light

```
[Free Zone (green) or Slowing (red)]----[Stop Zone (red)]----[Intersection]
         Normal speed                    Slow to 40%         Stop completely
```

**Plain English:** "When light is red, vehicles slow down gradually, then stop at the line"

### Pattern Application

When a pattern is selected:

1. **Prolog agent** determines which directions are served
   ```python
   pattern = 7  # Serves North + East
   served_directions = ['north', 'east']
   ```

2. **Convert to signal state**
   ```python
   signal_state = (False, False, True, True)  # E and N are True
   ```

3. **Apply to all roads** in those groups
   - Served roads: Green light (vehicles can pass)
   - Other roads: Red light (vehicles must stop)

---

## Inductive Loop Detectors

### What Are They?

Inductive loop detectors simulate real-world electromagnetic sensors embedded in road surfaces. They detect when vehicles pass over them.

### How They Work

#### 1. Detection Zone
```
Road: [========|DETECTOR|========]
              ↑ 2m zone ↑
```

Each detector has:
- **Position**: Usually at 80% of road length (0.8)
- **Detection zone**: 2 meters wide
- **Purpose**: Detect all passing vehicles

#### 2. Vehicle Detection

When a vehicle enters the detection zone:
```python
1. Record vehicle ID (unique identifier)
2. Record timestamp (when it passed)
3. Record speed (current velocity)
4. Record path (planned route)
5. Increment total count
```

#### 3. Flow Measurement

Detectors calculate:

**Flow Rate (vehicles/second):**
```python
flow_rate = vehicles_detected / time_window
# Example: 12 vehicles in 30 seconds = 0.4 veh/s
```

**Incoming Rate:** Vehicles entering the queue
**Service Rate:** Vehicles leaving the queue (during green)

### Turn Counting

Detectors analyze vehicle paths to count turns:

```python
def count_turns(vehicle_passages):
    for passage in last_30_seconds:
        path = passage['path']
        current_index = passage['current_road_index']
        
        if continuing_straight(path):
            straight_count += 1
        elif turning_left(path):
            left_turn_count += 1
        elif turning_right(path):
            right_turn_count += 1
```

**Direction Analysis:**
- Compare current road direction with next road direction
- If angle ≈ 0°: Straight
- If angle ≈ -90°: Left turn
- If angle ≈ +90°: Right turn

### Flow Estimator System

The `FlowEstimator` class coordinates multiple detectors:

```python
flow_estimator = FlowEstimator()
flow_estimator.add_detector('north', north_road)
flow_estimator.add_detector('south', south_road)
flow_estimator.add_detector('east', east_road)
flow_estimator.add_detector('west', west_road)
```

**Provides real-time data:**
- `get_incoming_rates()`: Vehicles arriving per direction
- `get_service_rates()`: Vehicles departing per direction
- `get_turn_counts()`: Number of vehicles turning
- `get_turn_ratios()`: Percentage wanting to turn

**Plain English:** "Detectors are like traffic counters on the road that measure how many cars pass and which way they're going"

---

## Data Logging (ZODB)

### Why ZODB?

The system uses **Zope Object Database (ZODB)** instead of CSV files because:

1. **Object-oriented**: Store Python objects directly
2. **Queryable**: Search and filter data efficiently
3. **Structured**: Organized in sessions with metadata
4. **Transactions**: ACID properties ensure data integrity

### Data Structure

#### Session Organization
```
traffic_simulation.fs (Database)
├── Session 20251105_040536
│   ├── Metadata (vehicle_rate, description, etc.)
│   ├── Simulation Records (every 60 frames)
│   └── Signal Records (pattern changes)
├── Session 20251105_105926
│   ├── Metadata
│   ├── Simulation Records
│   └── Signal Records
└── ... more sessions
```

### What Gets Logged?

#### 1. Simulation Records (every 1 second)
```python
{
    'time': 45.2,
    'vehicles_passed': 123,
    'vehicles_present': 45,
    'traffic_density': 0.0234,
    'speed_multiplier': 1.0,
    'vehicle_rate': 400,
    'pattern': 3,
    'queues': {'north': 8, 'south': 5, 'east': 12, 'west': 3},
    'flow_rates': {'north': 0.4, 'south': 0.3, 'east': 0.5, 'west': 0.2},
    'turn_ratios': {'north': 0.35, 'south': 0.20, 'east': 0.42, 'west': 0.15}
}
```

#### 2. Signal Records (every pattern change)
```python
{
    'timestamp': datetime.now(),
    'pattern': 3,
    'queues': {'north': 8, 'south': 5, 'east': 12, 'west': 3},
    'turn_counts': {'north': 3, 'south': 1, 'east': 5, 'west': 1},
    'flow_rates': {'north': 0.4, 'south': 0.3, 'east': 0.5, 'west': 0.2},
    'rules_fired': ['Rule 4', 'Rule 4.3', 'Rule 5']
}
```

### Session Lifecycle

1. **Start Simulation**
   ```python
   sim.start_logging(db_path='traffic_simulation.fs', 
                     vehicle_rate=400,
                     description='4-way intersection test')
   ```

2. **During Simulation**
   - Automatic logging every 60 frames
   - Pattern changes logged immediately
   - All data stored in memory (fast)

3. **End Simulation**
   ```python
   sim.stop_logging()  # Returns session_id
   ```
   - Data written to disk
   - Transaction committed
   - Session finalized

4. **Generate Report**
   ```python
   generate_html_report(db_logger)
   ```

**Plain English:** "All simulation data is saved to a database that you can query later, like a flight recorder for traffic"

---

## Simulation Loop

### Main Update Cycle

The simulation runs at **60 frames per second** with this sequence:

```python
def update():
    # 1. Update time
    effective_dt = dt * speed_multiplier  # Apply speed control
    t += effective_dt
    frame_count += 1
    
    # 2. Update all roads
    for road in roads:
        road.update(effective_dt)
        # Each road updates its vehicles using IDM
    
    # 3. Generate new vehicles
    for generator in generators:
        generator.update()
        # Add vehicles if conditions met
    
    # 4. Update traffic signals
    for signal in traffic_signals:
        signal.update(self)
        # Decide pattern, control lights
    
    # 5. Check for road transitions
    for road in roads:
        vehicle = road.vehicles[0]  # Front vehicle
        if vehicle.x >= road.length:
            if has_next_road:
                transfer_to_next_road()
            else:
                vehicles_passed += 1
                remove_vehicle()
    
    # 6. Log data (every 60 frames)
    if frame_count % log_interval == 0:
        log_current_state()
```

### Time Scaling

Users can control simulation speed:
- **Speed = 0.1x**: Slow motion (1 real second = 0.1 sim seconds)
- **Speed = 1.0x**: Real-time (1 real second = 1 sim second)
- **Speed = 3.0x**: Fast forward (1 real second = 3 sim seconds)

**Implementation:**
```python
effective_dt = base_dt * speed_multiplier
# All physics calculations use effective_dt
```

**Plain English:** "The simulation can run slower or faster than real-time while keeping physics accurate"

---

## Performance Considerations

### Optimization Techniques

1. **Deque for vehicles**: Fast append/pop operations
   ```python
   vehicles = deque()  # O(1) add/remove at ends
   ```

2. **Early termination**: Skip unnecessary updates
   ```python
   if len(road.vehicles) == 0:
       continue  # Skip empty roads
   ```

3. **Batch updates**: Update all vehicles in one pass
   ```python
   for i in range(1, n):
       vehicles[i].update(vehicles[i-1], dt)
   ```

4. **Efficient collision detection**:
   - Only check adjacent vehicles (car-following)
   - No all-pairs collision checks needed

5. **Lazy evaluation**: Flow rates calculated only when needed

### Typical Performance

- **60 FPS**: Smooth animation
- **100-200 vehicles**: No lag
- **4-way intersection**: Real-time processing
- **ZODB logging**: Minimal overhead (batched writes)

---

## Common Scenarios

### Scenario 1: Vehicle Journey

```
1. Generated at north_inbound with path [0, 5, 10, 15]
2. Accelerates to max speed using IDM
3. Approaches intersection, enters slowing zone
4. Traffic signal is RED → stops at stop line
5. Signal turns GREEN → accelerates
6. Crosses intersection (road 5)
7. Continues to road 10 (turn executed via path)
8. Exits intersection (road 15)
9. Reaches end of path → counted as "passed"
```

### Scenario 2: Queue Formation

```
1. Traffic signal turns RED
2. First vehicle stops at stop line
3. Second vehicle follows using IDM
   - Calculates safe following distance
   - Decelerates to maintain gap
4. Third vehicle follows second vehicle
5. Queue grows: V1 ← V2 ← V3 ← V4 ← V5
6. Inductive loop counts queue length = 5
7. Prolog agent notes high queue
8. May extend green time or prioritize this direction
```

### Scenario 3: Turn-Heavy Traffic

```
1. 15 vehicles approach from north
2. 8 want to turn (53% turn ratio)
3. Inductive loop detects high turn demand
4. Prolog agent receives turn data:
   - north: 8 turns, 15 total → ratio = 0.53
5. Rule 2 fires: "Turn-heavy traffic detected"
6. Rule 4.1 fires: "North direction selected"
7. Pattern 3 activated: North all movements
8. Turning vehicles get dedicated green phase
9. Queue clears efficiently
```

---

## Debugging Tips

### Common Issues

1. **Vehicles not moving**:
   - Check traffic signal state
   - Verify vehicle is not stopped
   - Check for collision with lead vehicle

2. **Pattern not changing**:
   - Verify Prolog agent is active
   - Check queue data is being updated
   - Ensure rules are firing

3. **High queue buildup**:
   - Increase vehicle service rate
   - Check signal patterns are optimal
   - Verify turn ratios are accurate

### Debug Output

The system prints useful information:

```
RULE 4.3 FIRED at time 1762314408.203
[Traffic Signal] Pattern 4 -> 3
  Queues: N= 9, S=14, E=40, W=29
  Turns:  N=10, S=10, E= 7, W= 1
  TurnRatio: N=0.53, S=0.77, E=0.47, W=0.17
  InFlow: N=0.321, S=0.217, E=0.250, W=0.100 veh/s
```

---

## Conclusion

This traffic simulation system combines:

1. **Realistic vehicle physics** (IDM model)
2. **Intelligent signal control** (Prolog rules)
3. **Accurate measurements** (inductive loops)
4. **Comprehensive logging** (ZODB)
5. **Interactive visualization** (Pygame)
6. **Rich reporting** (HTML + Chart.js)

The result is a sophisticated tool for studying traffic flow patterns, testing signal control strategies, and analyzing intersection performance.

**Key Innovations:**
- Turn behavior detection and optimization
- Multi-agent decision system with 9 intelligent rules
- Real-time flow measurement with simulated sensors
- Session-based data organization
- Interactive control panel for live adjustments

**Use Cases:**
- Traffic engineering education
- Signal timing optimization research
- Traffic pattern analysis
- What-if scenario testing
- Algorithm validation
