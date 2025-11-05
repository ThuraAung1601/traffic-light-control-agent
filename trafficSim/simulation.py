from .road import Road
from copy import deepcopy
from .vehicle_generator import VehicleGenerator
from .traffic_signal import TrafficSignal
from .db_logger import ZODBLogger

class Simulation:
    vehiclesPassed = 0;
    vehiclesPresent = 0;
    vehicleRate = 0;
    isPaused = False;

    def __init__(self, config={}):
        # Set default configuration
        self.set_default_config()

        # Update configuration
        for attr, val in config.items():
            setattr(self, attr, val)

    def set_default_config(self):
        self.t = 0.0            # Time keeping
        self.frame_count = 0    # Frame count keeping
        self.dt = 1/60          # Simulation time step
        self.roads = []         # Array to store roads
        self.generators = []
        self.traffic_signals = []
        self.iteration = 0      # n-th iteration (of sampling, if enabled)
        self.speed_multiplier = 1.0  # Speed control for simulation
        
        # Initialize ZODB logger
        self.db_logger = None
        self.enable_logging = True  # Flag to enable/disable logging
        self.log_interval = 60  # Log every 60 frames (1 second at 60 FPS)

    def create_road(self, start, end):
        road = Road(start, end)
        self.roads.append(road)
        return road

    def create_roads(self, road_list):
        for road in road_list:
            self.create_road(*road)

    def create_gen(self, config={}):
        gen = VehicleGenerator(self, config)
        self.generators.append(gen)
        Simulation.vehicleRate = gen.vehicle_rate
        return gen

    def create_signal(self, roads, config={}):
        roads = [[self.roads[i] for i in road_group] for road_group in roads]
        sig = TrafficSignal(roads, config)
        self.traffic_signals.append(sig)
        return sig

    def update(self):
        # Apply speed multiplier to effective dt
        effective_dt = self.dt * self.speed_multiplier
        
        # Update every road
        for road in self.roads:
            road.update(effective_dt)

        # Add vehicles
        for gen in self.generators:
            gen.update()

        for signal in self.traffic_signals:
            signal.update(self)

        # Check roads for out of bounds vehicle
        for road in self.roads:
            # If road has no vehicles, continue
            if len(road.vehicles) == 0: continue
            # If not
            vehicle = road.vehicles[0]
            # If first vehicle is out of road bounds
            if vehicle.x >= road.length:
                # If vehicle has a next road
                if vehicle.current_road_index + 1 < len(vehicle.path):
                    # Update current road to next road
                    vehicle.current_road_index += 1
                    # Create a copy and reset some vehicle properties
                    new_vehicle = deepcopy(vehicle)
                    new_vehicle.x = 0
                    # Add it to the next road
                    next_road_index = vehicle.path[vehicle.current_road_index]
                    self.roads[next_road_index].vehicles.append(new_vehicle)
                else:
                    Simulation.vehiclesPassed += 1
                # In all cases, remove it from its road
                road.vehicles.popleft() 

                # if vehicle reached the end of the path
                # if vehicle.current_road_index + 1 == len(vehicle.path):
                #     Simulation.vehiclesPassed += 1
                    # print("Vehicle passed: " + str(Simulation.vehiclesPassed))

        # Check for the number of vehicles present
        Simulation.vehiclesPresent = 0
        for road in self.roads:
            Simulation.vehiclesPresent += len(road.vehicles)

        # Increment time
        self.t += self.dt
        self.frame_count += 1
        
        # Log data periodically to ZODB
        if self.enable_logging and self.db_logger and self.frame_count % self.log_interval == 0:
            self._log_current_state()

        # Stop at certain time in seconds (for sampling purposes. Comment out if not needed)
        self.time_limit = 300
        if self.t >= self.time_limit:
            print("Traffic Signal Cycle Length: " + str(self.traffic_signals[0].cycle_length))
            print("Time: " + str(self.t))
            print("Vehicles Passed: " + str(Simulation.vehiclesPassed))
            print("Vehicles Present: " + str(Simulation.vehiclesPresent))
            print("Vehicle Rate: " + str(Simulation.vehicleRate))
            print("Traffic Density: " + str(Simulation.vehiclesPresent / (len(self.roads) * self.roads[0].length)))
            print("Iteration: " + str(self.iteration))

            # Log to ZODB instead of CSV
            if self.enable_logging and self.db_logger:
                self.db_logger.log_simulation_data(
                    time=self.t,
                    iteration=self.iteration,
                    cycle_length=self.traffic_signals[0].cycle_length if self.traffic_signals else 0,
                    vehicles_passed=Simulation.vehiclesPassed,
                    vehicles_present=Simulation.vehiclesPresent,
                    vehicle_rate=Simulation.vehicleRate,
                    traffic_density=Simulation.vehiclesPresent / (len(self.roads) * self.roads[0].length) if self.roads else 0
                )

            # Reset time and vehicles passed
            self.t = 0.001
            gen.delete_all_vehicles()
            Simulation.vehiclesPassed = 0
            Simulation.vehiclesPresent = 0
            self.iteration += 1
            if self.iteration % 5 == 0:
                # Set all traffic signals to +1
                for signal in self.traffic_signals:
                    signal.cycle_length += 1


    def run(self, steps):
        for _ in range(steps):
            self.update()

    def pause(self):
        self.isPaused = True

    def resume(self):
        self.isPaused = False
    
    def start_logging(self, db_path='traffic_simulation.fs', **metadata):
        """
        Start ZODB logging session.
        
        Args:
            db_path: Path to ZODB file storage
            **metadata: Additional session metadata (e.g., vehicle_rate, description)
        """
        if self.db_logger is None:
            self.db_logger = ZODBLogger(db_path)
        
        session_id = self.db_logger.start_session(**metadata)
        print(f"Started ZODB logging session: {session_id}")
        return session_id
    
    def stop_logging(self):
        """Stop current logging session"""
        if self.db_logger:
            session_id = self.db_logger.end_session()
            print(f"Stopped ZODB logging session: {session_id}")
            return session_id
        return None
    
    def close_logger(self):
        """Close ZODB logger connection"""
        if self.db_logger:
            self.db_logger.close()
            self.db_logger = None
    
    def _log_current_state(self):
        """Log current simulation state to ZODB"""
        if not self.db_logger:
            return
        
        try:
            # Collect traffic signal data
            signal_data = {}
            if self.traffic_signals:
                signal = self.traffic_signals[0]
                signal_data['pattern'] = signal.current_pattern if hasattr(signal, 'current_pattern') else 0
                signal_data['control_type'] = signal.control_type if hasattr(signal, 'control_type') else 'unknown'
                
                # Get flow estimation data if available
                if hasattr(signal, '_flow_estimator') and signal._flow_estimator:
                    try:
                        signal_data['flow_rates'] = signal._flow_estimator.get_incoming_rates()
                        signal_data['turn_counts'] = signal._flow_estimator.get_turn_counts()
                        signal_data['turn_ratios'] = signal._flow_estimator.get_turn_ratios()
                    except Exception:
                        pass
                
                # Get queue data
                try:
                    queues = signal._measure_queues()
                    signal_data['queues'] = queues
                except Exception:
                    pass
            
            # Log comprehensive state
            self.db_logger.log_simulation_data(
                time=self.t,
                frame_count=self.frame_count,
                vehicles_passed=Simulation.vehiclesPassed,
                vehicles_present=Simulation.vehiclesPresent,
                vehicle_rate=Simulation.vehicleRate,
                speed_multiplier=self.speed_multiplier,
                **signal_data
            )
        except Exception as e:
            print(f"Warning: Failed to log state: {e}")
    
    def log_signal_pattern_change(self, pattern, queues=None, turn_counts=None, 
                                   flow_rates=None, rules_fired=None):
        """
        Log a traffic signal pattern change event.
        
        Args:
            pattern: New pattern number
            queues: Queue lengths by direction
            turn_counts: Turn counts by direction
            flow_rates: Flow rates by direction
            rules_fired: List of rules that fired
        """
        if self.db_logger:
            try:
                self.db_logger.log_signal_state(
                    pattern=pattern,
                    queues=queues,
                    turn_counts=turn_counts,
                    flow_rates=flow_rates,
                    rules_fired=rules_fired
                )
            except Exception as e:
                print(f"Warning: Failed to log signal change: {e}")
    
    def set_speed(self, multiplier):
        """Set simulation speed multiplier. 1.0 = normal, 0.5 = half speed, 2.0 = double speed"""
        self.speed_multiplier = max(0.1, min(10.0, multiplier))  # Clamp between 0.1x and 10x
    
    def speed_up(self):
        """Increase simulation speed"""
        self.set_speed(self.speed_multiplier * 1.5)
    
    def slow_down(self):
        """Decrease simulation speed"""
        self.set_speed(self.speed_multiplier / 1.5)