import random
from typing import List, Dict

try:
    # Local optional import; falls back to classic mode if missing
    from .prolog_agent import PrologTrafficAgent
    _PROLOG_AVAILABLE = True
except Exception:
    PrologTrafficAgent = None  # type: ignore
    _PROLOG_AVAILABLE = False

class TrafficSignal:
    def __init__(self, roads, config={}):
        # Initialize roads
        self.roads = roads
        # Set default configuration
        self.set_default_config()

        # Update configuration
        for attr, val in config.items():
            setattr(self, attr, val)
        # Calculate properties
        self.init_properties()
        # Init control mode
        self._init_control_mode()

    def set_default_config(self):
        self.cycle = [(False, False, False, True), (False, False, True, False), (False, True, False, False), (True, False, False, False)]
        self.slow_distance = 50
        self.slow_factor = 0.4
        self.stop_distance = 12
        self.cycle_length = 1

        self.current_cycle_index = 0

        self.last_t = 0
        # control: 'rule' or 'prolog'
        self.control = 'prolog' if _PROLOG_AVAILABLE else 'rule'
        # internal state for prolog control
        self._prolog_agent = None
        self._pattern = 1
        self._frames_left = 0
        self._elapsed_in_pattern = 0
        self._current_cycle = (True,) * max(1, len(self.roads))

    def init_properties(self):
        for i in range(len(self.roads)):
            for road in self.roads[i]:
                road.set_traffic_signal(self, i)

    def _init_control_mode(self):
        # Only meaningful for 4-group signals
        if self.control == 'prolog' and _PROLOG_AVAILABLE and len(self.roads) >= 4:
            try:
                self._prolog_agent = PrologTrafficAgent("junction")
                self._pattern = 1
                # read prolog pattern duration
                self._frames_left = self._prolog_agent.get_pattern_duration()
                self._elapsed_in_pattern = 0
                # initialize cycle according to pattern
                self._apply_pattern(self._pattern)
            except Exception:
                # fallback to rule mode if prolog fails
                self.control = 'rule'

    @property
    def current_cycle(self):
        if self.control == 'prolog' and len(self.roads) >= 4:
            return self._current_cycle
        return self.cycle[self.current_cycle_index]
    
    @property
    def current_pattern(self):
        """Get the current traffic pattern number"""
        return self._pattern
    
    @property 
    def control_type(self):
        """Get the control type (rule, prolog, etc.)"""
        return self.control
        
    @property
    def current_cycle_time(self):
        """Get current cycle time in seconds"""
        if self.control == 'prolog' and self._frames_left > 0:
            return self._frames_left / 60.0  # Convert frames to seconds (assuming 60 FPS)
        return self.cycle_length
    
    @property
    def prolog_agent(self):
        """Get the Prolog agent for rule tracking"""
        return self._prolog_agent
    
    def update(self, sim):
        if self.control == 'prolog' and len(self.roads) >= 4 and self._prolog_agent is not None:
            # Update queues from roads and step prolog-driven control
            queues = self._measure_queues()
            self._prolog_agent.update_queues(queues)
            # simple: zero rates unless you add estimation
            self._prolog_agent.update_rates({d: 0.0 for d in queues}, {d: 0.0 for d in queues})

            self._frames_left -= 1
            self._elapsed_in_pattern += 1

            change_early = False
            # trigger change if no demand in served dirs
            try:
                change_early = self._prolog_agent.should_change_early(self._pattern)
            except Exception:
                change_early = False

            if self._frames_left <= 0 or change_early:
                # decide next pattern
                try:
                    next_p = self._prolog_agent.decide_next_pattern()
                except Exception:
                    next_p = self._pattern % 12 + 1
                self._pattern = int(next_p)
                # reset timers
                self._frames_left = self._prolog_agent.get_pattern_duration()
                self._elapsed_in_pattern = 0
                self._prolog_agent.set_current_state(self._pattern, pattern_duration=self._frames_left, greentime=0)
                # apply to current cycle
                self._apply_pattern(self._pattern)
            # else keep current cycle
            return

        # fallback classic behavior
        cycle_length = self.cycle_length
        # randomize the cycle length after every cycle
        if(sim.t % cycle_length == 0):
            cycle_length = random.randint(20, 40)
        k = (sim.t // cycle_length) % 4
        self.current_cycle_index = int(k)
        if(len(self.roads) < 4):
            self.current_cycle_index = 3

    # --- helpers for prolog control --------------------------------------
    def _measure_queues(self) -> Dict[str, int]:
        # mapping groups -> directions (based on main.py create_signal order)
        # 0: west inbound, 1: south inbound, 2: east inbound, 3: north inbound
        dir_map = {0: 'west', 1: 'south', 2: 'east', 3: 'north'}
        q: Dict[str, int] = {"north": 0, "south": 0, "east": 0, "west": 0}
        for gi, group in enumerate(self.roads[:4]):
            dname = dir_map.get(gi)
            if not dname:
                continue
            total = 0
            for rd in group:
                total += len(rd.vehicles)
            q[dname] = total
        return q

    def _apply_pattern(self, pattern: int):
        # build boolean tuple per group from served dirs
        try:
            dirs = self._prolog_agent.serves_dirs(pattern) if self._prolog_agent else []
        except Exception:
            dirs = []
        served = set(dirs)
        # mapping back to group indices
        dir_to_idx = {'west': 0, 'south': 1, 'east': 2, 'north': 3}
        states: List[bool] = [False, False, False, False]
        for d in served:
            idx = dir_to_idx.get(d)
            if idx is not None:
                states[idx] = True
        self._current_cycle = tuple(states)
