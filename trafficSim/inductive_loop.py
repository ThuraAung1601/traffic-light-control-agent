"""
Simulated inductive loop detectors for traffic flow measurement.
Mimics electromagnetic sensors embedded in road surface.
"""
from typing import Dict, List, Tuple, Optional
from collections import deque
import time


class InductiveLoopDetector:
    """
    Simulates an inductive loop detector embedded in a road.
    Detects vehicles passing over a specific position on the road.
    """
    
    def __init__(self, road, position: float = 0.8, detection_zone: float = 2.0):
        """
        Args:
            road: Road object where detector is installed
            position: Relative position on road (0.0 = start, 1.0 = end)
            detection_zone: Length of detection zone in meters
        """
        self.road = road
        self.position = position  # Relative position (0-1)
        self.detection_zone = detection_zone
        
        # Detection state
        self.vehicles_in_zone = set()  # Currently detected vehicle IDs
        self.vehicle_passages = []  # List of (vehicle_id, timestamp, speed, path)
        
        # Statistics
        self.total_count = 0
        self.last_passage_time = None
        
    @property
    def absolute_position(self) -> float:
        """Get absolute position on road in meters"""
        return self.position * self.road.length
    
    def update(self, current_time: float):
        """
        Update detector state by checking vehicles in detection zone.
        
        Args:
            current_time: Current simulation time in seconds
        """
        currently_detected = set()
        abs_pos = self.absolute_position
        
        # Check each vehicle on the road
        for vehicle in self.road.vehicles:
            # Check if vehicle is in detection zone
            if abs(vehicle.x - abs_pos) <= self.detection_zone / 2:
                vehicle_id = id(vehicle)
                currently_detected.add(vehicle_id)
                
                # New detection (vehicle just entered zone)
                if vehicle_id not in self.vehicles_in_zone:
                    self._record_passage(vehicle, current_time)
        
        # Update tracking set
        self.vehicles_in_zone = currently_detected
    
    def _record_passage(self, vehicle, timestamp: float):
        """Record a vehicle passage event"""
        self.total_count += 1
        self.last_passage_time = timestamp
        
        # Store passage data: (vehicle_id, timestamp, speed, path)
        passage_data = {
            'vehicle_id': id(vehicle),
            'timestamp': timestamp,
            'speed': vehicle.v,
            'path': vehicle.path.copy() if hasattr(vehicle, 'path') else [],
            'current_road_index': vehicle.current_road_index if hasattr(vehicle, 'current_road_index') else 0
        }
        self.vehicle_passages.append(passage_data)
    
    def get_count_in_window(self, duration: float, current_time: float) -> int:
        """
        Get number of vehicles detected in the last N seconds.
        
        Args:
            duration: Time window in seconds
            current_time: Current simulation time
        
        Returns:
            Count of vehicles in time window
        """
        cutoff_time = current_time - duration
        return sum(1 for p in self.vehicle_passages if p['timestamp'] >= cutoff_time)
    
    def get_flow_rate(self, duration: float, current_time: float) -> float:
        """
        Calculate flow rate in vehicles per hour.
        
        Args:
            duration: Time window in seconds
            current_time: Current simulation time
        
        Returns:
            Flow rate in vehicles/hour
        """
        count = self.get_count_in_window(duration, current_time)
        if duration > 0:
            # Convert to vehicles per hour
            return (count / duration) * 3600
        return 0.0
    
    def get_average_speed(self, duration: float, current_time: float) -> float:
        """
        Get average speed of vehicles in time window.
        
        Args:
            duration: Time window in seconds
            current_time: Current simulation time
        
        Returns:
            Average speed in m/s
        """
        cutoff_time = current_time - duration
        recent_passages = [p for p in self.vehicle_passages if p['timestamp'] >= cutoff_time]
        
        if recent_passages:
            return sum(p['speed'] for p in recent_passages) / len(recent_passages)
        return 0.0
    
    def get_time_gaps(self, duration: float, current_time: float) -> List[float]:
        """
        Get time gaps between consecutive vehicles (headways).
        
        Args:
            duration: Time window in seconds
            current_time: Current simulation time
        
        Returns:
            List of time gaps in seconds
        """
        cutoff_time = current_time - duration
        recent_passages = [p for p in self.vehicle_passages if p['timestamp'] >= cutoff_time]
        
        gaps = []
        for i in range(1, len(recent_passages)):
            gap = recent_passages[i]['timestamp'] - recent_passages[i-1]['timestamp']
            gaps.append(gap)
        
        return gaps
    
    def clear_old_data(self, retention_time: float, current_time: float):
        """
        Remove old passage data to prevent memory growth.
        
        Args:
            retention_time: How long to keep data in seconds
            current_time: Current simulation time
        """
        cutoff_time = current_time - retention_time
        self.vehicle_passages = [p for p in self.vehicle_passages if p['timestamp'] >= cutoff_time]


class FlowEstimator:
    """
    Estimates traffic flow and turn movements using data from inductive loop detectors.
    Provides flow rates, turn ratios, and service rates for traffic signal control.
    """
    
    def __init__(self, alpha: float = 0.3, measurement_window: float = 60.0):
        """
        Args:
            alpha: EMA smoothing factor (0-1, higher = more responsive)
            measurement_window: Time window for flow calculations in seconds
        """
        self.alpha = alpha
        self.measurement_window = measurement_window
        
        # Detectors by direction
        self.detectors: Dict[str, InductiveLoopDetector] = {}
        
        # Smoothed estimates
        self.incoming_flow = {'north': 0.0, 'south': 0.0, 'east': 0.0, 'west': 0.0}
        self.service_rate = {'north': 0.0, 'south': 0.0, 'east': 0.0, 'west': 0.0}
        self.turn_counts = {'north': 0, 'south': 0, 'east': 0, 'west': 0}
        
        # Turn movement tracking (for turn ratio calculation)
        self.movement_counts = {
            'north': {'straight': 0, 'left': 0, 'right': 0},
            'south': {'straight': 0, 'left': 0, 'right': 0},
            'east': {'straight': 0, 'left': 0, 'right': 0},
            'west': {'straight': 0, 'left': 0, 'right': 0}
        }
    
    def add_detector(self, direction: str, road, position: float = 0.8):
        """
        Add an inductive loop detector to a road.
        
        Args:
            direction: Direction name (north, south, east, west)
            road: Road object
            position: Relative position on road (0.0-1.0)
        """
        self.detectors[direction] = InductiveLoopDetector(road, position)
    
    def update(self, current_time: float, signal_state: Dict[str, bool]):
        """
        Update all detectors and calculate flow estimates.
        
        Args:
            current_time: Current simulation time in seconds
            signal_state: Dict of {direction: is_green}
        """
        # Update all detectors
        for direction, detector in self.detectors.items():
            detector.update(current_time)
            detector.clear_old_data(self.measurement_window * 2, current_time)
        
        # Calculate flow rates using EMA
        for direction, detector in self.detectors.items():
            # Incoming flow (always measured)
            raw_flow = detector.get_flow_rate(self.measurement_window, current_time)
            self.incoming_flow[direction] = self._ema_update(
                self.incoming_flow[direction], 
                raw_flow / 3600.0  # Convert to vehicles per second
            )
            
            # Service rate (only when green)
            if signal_state.get(direction, False):
                self.service_rate[direction] = self._ema_update(
                    self.service_rate[direction],
                    raw_flow / 3600.0
                )
        
        # Update turn movement counts
        self._update_turn_movements(current_time)
    
    def _ema_update(self, old_value: float, new_value: float) -> float:
        """Exponential moving average update"""
        return self.alpha * new_value + (1 - self.alpha) * old_value
    
    def _update_turn_movements(self, current_time: float):
        """
        Analyze vehicle paths to count turn movements.
        Classifies each passage as straight, left, or right turn.
        """
        # Reset counts
        for dir_counts in self.movement_counts.values():
            dir_counts['straight'] = 0
            dir_counts['left'] = 0
            dir_counts['right'] = 0
        
        self.turn_counts = {'north': 0, 'south': 0, 'east': 0, 'west': 0}
        
        # Analyze recent passages from each detector
        for direction, detector in self.detectors.items():
            cutoff_time = current_time - self.measurement_window
            recent_passages = [p for p in detector.vehicle_passages if p['timestamp'] >= cutoff_time]
            
            for passage in recent_passages:
                movement = self._classify_movement(passage, direction)
                if movement:
                    self.movement_counts[direction][movement] += 1
                    if movement in ('left', 'right'):
                        self.turn_counts[direction] += 1
    
    def _classify_movement(self, passage: dict, direction: str) -> Optional[str]:
        """
        Classify vehicle movement as straight, left, or right.
        
        Args:
            passage: Passage data dict
            direction: Approach direction
        
        Returns:
            'straight', 'left', 'right', or None
        """
        path = passage.get('path', [])
        current_idx = passage.get('current_road_index', 0)
        
        if len(path) <= current_idx + 1:
            return None
        
        # Get next road in path
        next_road = path[current_idx + 1] if current_idx + 1 < len(path) else None
        
        if next_road is None:
            return None
        
        # Road numbering from main.py:
        # 0-3: inbound lane 1, 4-7: outbound lane 1, 8-11: straight lane 1
        # 12-15: inbound lane 2, 16-19: outbound lane 2, 20-23: straight lane 2
        # 24-27: inbound lane 3, 28-31: outbound lane 3, 32-35: straight lane 3
        # 36+: turn roads (20 segments each)
        
        # Straight roads: 8-11 (lane 1), 20-23 (lane 2), 32-35 (lane 3)
        if next_road in list(range(8, 12)) + list(range(20, 24)) + list(range(32, 36)):
            return 'straight'
        elif next_road >= 36:
            # Turn road - need to determine left vs right
            # This is a simplified heuristic based on road numbering
            return 'left' if (next_road - 36) % 40 >= 20 else 'right'
        
        return None
    
    def get_incoming_rates(self) -> Dict[str, float]:
        """Get smoothed incoming flow rates (vehicles/sec)"""
        return self.incoming_flow.copy()
    
    def get_service_rates(self) -> Dict[str, float]:
        """Get smoothed service rates (vehicles/sec)"""
        return self.service_rate.copy()
    
    def get_turn_counts(self) -> Dict[str, int]:
        """Get turn vehicle counts for each direction"""
        return self.turn_counts.copy()
    
    def get_turn_ratios(self) -> Dict[str, float]:
        """
        Get turn ratio for each direction (turns / total).
        
        Returns:
            Dict of {direction: turn_ratio}
        """
        ratios = {}
        for direction in ['north', 'south', 'east', 'west']:
            total = sum(self.movement_counts[direction].values())
            turns = self.turn_counts[direction]
            ratios[direction] = turns / total if total > 0 else 0.0
        return ratios
