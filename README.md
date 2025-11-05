# Traffic-Simulation

An intelligent traffic flow simulation system using microscopic modeling with the Intelligent-Driver Model (IDM), enhanced with a Prolog-based multi-agent decision system for adaptive traffic signal control. The simulation features real-time traffic flow analysis, inductive loop detectors, and comprehensive data logging to ZODB for post-simulation analysis.

# Team Members
For **Artificial Intelligence** course, Software Engineering Program, KMITL. 
- Ei Myat Nwe 66011534
- Nay Chi Shunn Lei 66011090
- Thura Aung 66011606


## Introduction

This traffic simulation system models a 4-way intersection with intelligent traffic signal control powered by Prolog rules. The system dynamically adjusts traffic light patterns based on:

- **Queue lengths** at each direction (North, South, East, West)
- **Turn demand ratios** (percentage of vehicles wanting to turn)
- **Flow rates** measured by inductive loop detectors
- **Traffic density** and vehicle generation rates

### Key Features

- **Real-time visualization** using Pygame with interactive controls
- **Intelligent traffic signal control** with 12 different light patterns
- **Prolog-based multi-agent system** with 9 sophisticated decision rules
- **Inductive loop detectors** for accurate flow measurement
- **ZODB persistent storage** for session-based data organization
- **HTML report generation** with interactive Chart.js visualizations
- **Interactive control panel** with sliders for:
  - Vehicle generation rate
  - Simulation speed multiplier
  - Vehicle type probabilities (car, truck, bus, motorbike)

### Traffic Signal Patterns

The system uses 12 patterns to optimize traffic flow:
- Patterns 1-2: Main corridor patterns (N/S, E/W)
- Patterns 3-6: Single direction with all movements
- Patterns 7-10: Combination patterns (non-conflicting directions)
- Patterns 11-12: Full axis patterns (all N/S or E/W movements)

## Installation

### Prerequisites

- Python 3.7 or higher
- SWI-Prolog (for the traffic signal decision system)

### Install SWI-Prolog

**macOS:**
```bash
brew install swi-prolog
```

**Ubuntu/Debian:**
```bash
sudo apt-get install swi-prolog
```

**Windows:**
Download from [SWI-Prolog official website](https://www.swi-prolog.org/Download.html)

### Install Python Dependencies

```bash
# Clone the repository
git clone https://github.com/ThuraAung1601/traffic-light-control-agent.git

# Change directory to the repository
cd traffic-light-control-agent

# Install required packages
pip install pygame pyswip ZODB BTrees persistent transaction
-OR-
pip install -r requirements.txt
```

### Required Packages

- `pygame` - For visualization and UI
- `pyswip` - Python interface to SWI-Prolog
- `ZODB` - Zope Object Database for persistent storage
- `BTrees` - Scalable persistent components
- `persistent` - Persistent object support
- `transaction` - Transaction management

## Running the Simulation

### Start the Simulation

```bash
python main.py
```

### Interactive Controls

Once the simulation window opens:

1. **Control Panel (Left side):**
   - **Vehicle Rate Slider**: Adjust vehicle generation frequency (60-600 vehicles/min)
   - **Speed Multiplier Slider**: Control simulation speed (0.1x - 3.0x)
   - **Vehicle Type Sliders**: Set probabilities for cars, trucks, buses, and motorbikes
   - **End Simulation Button**: Stop simulation and generate HTML report

2. **Visualization:**
   - Watch vehicles move through the intersection
   - Traffic signals change color based on Prolog rules
   - Real-time pattern numbers displayed
   - Queue formations visible at each direction

### End Simulation & View Report

Click the **"End Simulation"** button to:
1. Stop the simulation
2. Save all data to ZODB
3. Generate an HTML report with visualizations
4. Automatically open the report in your browser

### Query Simulation Data

Use the query tool to explore saved sessions:

```bash
# List all saved sessions
python query_db.py list

# View details of a specific session
python query_db.py view <session_id>

# Export session to CSV
python query_db.py export <session_id>

# Regenerate HTML report for a session
python query_db.py report <session_id>
```

## Generated Reports

HTML reports include:
- **Traffic Flow Over Time**: Vehicles passed and present
- **Queue Lengths by Direction**: N, S, E, W queue analysis
- **Flow Rates by Direction**: Traffic flow measurements
- **Signal Pattern Distribution**: Usage statistics for each pattern
- **Rules Fired Statistics**: Which Prolog rules were triggered and how often

## Project Structure

```
.
├── main.py                          # Main entry point
├── query_db.py                      # ZODB query tool
├── trafficSim/
│   ├── simulation.py                # Core simulation engine
│   ├── window.py                    # Pygame visualization
│   ├── traffic_signal.py            # Signal control with Prolog
│   ├── vehicle.py                   # Vehicle behavior (IDM)
│   ├── road.py                      # Road network
│   ├── inductive_loop.py            # Flow detection
│   ├── db_logger.py                 # ZODB logging system
│   ├── report_generator.py          # HTML report generation
│   ├── rules.pl                     # Prolog decision rules
│   └── rule_docs.md                 # Rule documentation
├── traffic_simulation.fs            # ZODB database file
└── traffic_simulation_report_*.html # Generated reports
```

## Technical Details

### Intelligent-Driver Model (IDM)

The simulation uses IDM for realistic vehicle behavior including:
- Acceleration/deceleration dynamics
- Safe following distances
- Speed adaptation
- Lane-specific behavior

### Prolog Decision System

The traffic signal controller uses 9 intelligent rules:
1. **Cycle Pattern**: Fallback rotation
2. **Turn-Heavy Traffic**: Priority for high turn demand
3. **Balanced Traffic**: Combination patterns
4. **Main Selection Algorithm**: Core decision logic
5. **Decision Wrapper**: Integration interface
6. **Early Termination**: End green when no demand
7. **Performance Extension**: Keep effective patterns longer
8. **Time Extension**: Extend for growing queues
9. **Safety Override**: Maximum time enforcement

See `trafficSim/rule_docs.md` for detailed rule explanations.

## Future Enhancements
- [ ] Multi-agent Intelligent System
- [ ] Ambulance Priotization
- [ ] Machine Learning based parameter optimization

## Documentation
For detailed explanation of the Prolog rules and traffic control logic, see:
- `trafficSim/rule_docs.md`

## License
Open source - feel free to use and modify for educational and research purposes.

## References
- [1] Visit Hirankitti and Jaturapith Krohkaew,  "An Agent Approach for Intelligent Traffic-Light Control," First Asia International Conference on Modelling & Simulation (AMS'07), Phyket, Thailand, 2007, pp. 496-501, doi: 10.1109/AMS.2007.11.
- [2] Visit Hirankitti, Jaturapith Krohkaew, and Chris Hogger, “A Multi-Agent Approach for Intelligent Traffic-Light Control,” Proceedings of the World Congress on Engineering 2007, London, U.K.
- For simulation: https://muddy-vulture-d01.notion.site/The-Modelling-of-Simpang-Empat-Pingit-Crossroad-a7f1a8adf0d44317aebff998149494b9 
