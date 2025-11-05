from __future__ import annotations
from typing import Dict, Optional, Tuple, List
import os

try:
    from pyswip import Prolog
except Exception:  # pragma: no cover - optional dependency
    Prolog = None  # type: ignore

# Get the directory where this module is located
MODULE_DIR = os.path.dirname(os.path.abspath(__file__))
RULES_PATH_DEFAULT = os.path.join(MODULE_DIR, "rules.pl")


class PrologTrafficAgent:
    """
    Prolog-based traffic signal control agent for a single intersection.
    
    Uses SWI-Prolog rules (rules.pl) for intelligent pattern selection based on:
    - Queue lengths per direction (north, south, east, west)
    - Turn demand ratios
    - Incoming vs service rates
    - Current pattern and timing state
    
    Rule System (9 core rules):
    - Rule 1: Fallback pattern cycling
    - Rule 2: Turn-heavy traffic management
    - Rule 3: Balanced perpendicular traffic
    - Rule 4: Main intelligent pattern selection (with 11 sub-rules)
    - Rule 5: Pattern decision wrapper
    - Rule 6: Early pattern termination
    - Rule 7: Performance-based extension
    - Rule 8: Time extension logic
    - Rule 9: Maximum time safety override
    
    Public Methods:
    State Management:
    - set_current_state(pattern, pattern_duration, greentime)
    - update_queues({north, south, east, west})
    - update_rates(incoming, service)
    - update_turn_demand(turn_counts)
    
    Decision Queries:
    - decide_next_pattern() -> int
    - should_change_early(pattern?) -> bool
    - time_extension(elapsed_frames) -> int
    - max_time_override() -> bool
    - serves_dirs(pattern) -> List[str]
    
    Rule Tracking:
    - get_fired_rules() -> List[Tuple[rule_id, count, description]]
    - get_rule_summary() -> str
    - clear_fired_rules()
    """

    def __init__(self, name: str, rules_path: Optional[str] = None):
        if Prolog is None:
            raise RuntimeError("pyswipl/SWI-Prolog not available")
        self.name = name
        self.prolog = Prolog()
        self.rules_path = rules_path or RULES_PATH_DEFAULT
        self.prolog.consult(self.rules_path)
        # init defaults
        self.set_current_state(1, pattern_duration=240, greentime=0)

        # Rule tracking
        self.fired_rules = []
        self.rule_descriptions = {
            # Rule 1: Fallback pattern cycling
            1: "Cycle pattern (fallback round-robin)",
            
            # Rule 2: Turn-heavy traffic management
            2: "Turn-heavy traffic detected (Queue>8, TurnRatio>35%)",
            
            # Rule 3: Balanced perpendicular traffic
            3: "Balanced cross-traffic (combination pattern)",
            
            # Rule 4: Main intelligent pattern selection
            4: "Main pattern selection algorithm",
            4.1: "Turn-heavy: North direction selected",
            4.2: "Turn-heavy: South direction selected",
            4.3: "Turn-heavy: East direction selected",
            4.4: "Turn-heavy: West direction selected",
            4.5: "Balanced traffic: combination pattern (TotalAll>8, balanced NS/EW)",
            4.6: "Moderate balanced: combination pattern (NS>5, EW>5)",
            4.7: "Strong N-S dominance: Full N-S pattern (11)",
            4.8: "Strong E-W dominance: Full E-W pattern (12)",
            4.9: "Moderate N-S dominance: Straight pattern (1)",
            4.10: "Moderate E-W dominance: Straight pattern (2)",
            4.11: "No clear dominance: Fallback to round-robin",
            
            # Rule 5: Pattern decision wrapper
            5: "Pattern decision wrapper (reads queue facts from Python)",
            
            # Rule 6: Early termination
            6: "Early pattern termination (no demand in served directions)",
            
            # Rule 7: Performance-based extension
            7: "Pattern serving well (service > 70% of incoming rate)",
            
            # Rule 8: Time extension
            8: "Time extension (queue growing, incoming > 80% of service)",
            
            # Rule 9: Safety override
            9: "Maximum time safety override (greentime exceeded max)"
        }

    # --- internal helpers -------------------------------------------------
    def _retractall(self, functor_arity: str):
        list(self.prolog.query(f"retractall({functor_arity})."))

    def _get_single_int(self, query: str, var: str) -> Optional[int]:
        res = list(self.prolog.query(query))
        if res:
            v = res[0].get(var)
            try:
                return int(v)
            except Exception:
                return None
        return None

    # --- state setters ----------------------------------------------------
    def set_current_state(self, pattern: int, pattern_duration: Optional[int] = None, greentime: Optional[int] = None):
        self._retractall("current_state(_)")
        self.prolog.assertz(f"current_state({int(pattern)})")
        if pattern_duration is not None:
            self._retractall("state_counter(_)")
            self.prolog.assertz(f"state_counter({int(pattern_duration)})")
        if greentime is not None:
            self._retractall("state_greentime(_)")
            self.prolog.assertz(f"state_greentime({int(greentime)})")

    def update_queues(self, queues: Dict[str, int]):
        for d in ("north", "south", "east", "west"):
            self._retractall(f"queue_length({d},_)")
            q = int(queues.get(d, 0))
            self.prolog.assertz(f"queue_length({d},{q})")

    def update_rates(self, incoming: Dict[str, float], service: Dict[str, float]):
        for d in ("north", "south", "east", "west"):
            self._retractall(f"incoming_rate({d},_)")
            self._retractall(f"queue_service_rate({d},_)")
            self.prolog.assertz(f"incoming_rate({d},{float(incoming.get(d,0.0))})")
            self.prolog.assertz(f"queue_service_rate({d},{float(service.get(d,0.0))})")

    def update_turn_demand(self, turns: Dict[str, int]):
        for d in ("north", "south", "east", "west"):
            self._retractall(f"turn_demand({d},_,_)")
            # mirror queue length if available
            qRes = list(self.prolog.query(f"queue_length({d},Q)"))
            q = int(qRes[0]["Q"]) if qRes else 0
            self.prolog.assertz(f"turn_demand({d},{int(turns.get(d,0))},{q})")

    # --- decisions --------------------------------------------------------
    def should_change_early(self, pattern: Optional[int] = None) -> bool:
        pat = pattern if pattern is not None else list(self.prolog.query("current_state(P)"))[0]["P"]
        result = bool(list(self.prolog.query(f"should_change_early({int(pat)})")))
        self._collect_fired_rules()
        return result

    def time_extension(self, elapsed_frames: int) -> int:
        pat = list(self.prolog.query("current_state(P)"))[0]["P"]
        ext = list(self.prolog.query(f"time_extension_decision({int(pat)},{int(elapsed_frames)},E)"))
        self._collect_fired_rules()
        return int(ext[0]["E"]) if ext else 0

    def decide_next_pattern(self) -> int:
        pat = list(self.prolog.query("current_state(P)"))[0]["P"]
        res = list(self.prolog.query(f"decide_next_pattern({int(pat)},NP)"))
        self._collect_fired_rules()
        return int(res[0]["NP"]) if res else int(pat)

    def max_time_override(self) -> bool:
        result = bool(list(self.prolog.query("max_time_override")))
        self._collect_fired_rules()
        return result

    # --- rule tracking ----------------------------------------------------
    def _collect_fired_rules(self):
        """Collect all rules that fired during the last query"""
        try:
            fired = list(self.prolog.query("rule_fired(Rule, Count)"))
            for rule_info in fired:
                rule_id = rule_info["Rule"]
                count = rule_info["Count"]
                if rule_id not in [r[0] for r in self.fired_rules]:
                    self.fired_rules.append((rule_id, count))
            # Clear the fired rules from Prolog
            list(self.prolog.query("retractall(rule_fired(_, _))"))
        except Exception:
            pass  # Rule tracking is optional

    def get_fired_rules(self) -> List[Tuple[float, int, str]]:
        """Get list of (rule_id, count, description) for fired rules"""
        return [(rule_id, count, self.rule_descriptions.get(rule_id, f"Rule {rule_id}")) 
                for rule_id, count in self.fired_rules]

    def clear_fired_rules(self):
        """Clear the list of fired rules"""
        self.fired_rules = []
        list(self.prolog.query("retractall(rule_fired(_, _))"))

    def get_rule_summary(self) -> str:
        """Get a summary of recently fired rules"""
        if not self.fired_rules:
            return "No rules fired"
        
        summary = "Fired Rules: "
        rule_strs = []
        for rule_id, count in self.fired_rules[-5:]:  # Last 5 rules
            desc = self.rule_descriptions.get(rule_id, f"Rule {rule_id}")
            rule_strs.append(f"{rule_id} ({desc})")
        return summary + ", ".join(rule_strs)

    def get_pattern_duration(self) -> int:
        v = self._get_single_int("pattern_duration(T)", "T")
        return v if v is not None else 240

    def serves_dirs(self, pattern: int) -> List[str]:
        res = list(self.prolog.query(f"serves_dirs({int(pattern)}, Dirs)"))
        if not res:
            return []
        dirs = res[0].get("Dirs")
        # pyswipl returns Python lists of atoms already in most cases
        if isinstance(dirs, list):
            return [str(d) for d in dirs]
        # fallback: single atom
        return [str(dirs)]
