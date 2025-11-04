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
    Minimal wrapper around SWI-Prolog rules for a single intersection.

    Exposes:
    - set_current_state(pattern, pattern_duration, greentime)
    - update_queues({north,south,east,west})
    - update_rates(incoming, service)
    - update_turn_demand(turn_counts)
    - decide_next_pattern() -> int
    - should_change_early(pattern?) -> bool
    - time_extension(elapsed_frames) -> int (0 = no extension)
    - max_time_override() -> bool
    - serves_dirs(pattern) -> [dir,...]
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
            1.1: "High-demand pattern selection (N-S)",
            1.2: "High-demand pattern selection (E-W)", 
            1.3: "High-demand pattern selection (single direction)",
            1.4: "Moderate-demand pattern selection",
            1.5: "Low-demand pattern selection",
            2.1: "Best pattern search by movement type",
            2.2: "Movement priority scoring",
            3.1: "Balanced N-S corridor selection",
            3.2: "Balanced E-W corridor selection", 
            3.3: "Balanced multi-direction selection",
            4.1: "Fallback: round-robin pattern cycling",
            5.12: "Main pattern decision wrapper",
            6.1: "Early pattern termination (no demand)",
            7.1: "Pattern serving well (performance check)",
            8.1: "Time extension decision (growing queue)",
            9.1: "Maximum time safety override"
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


class SupervisorAgent:
    def __init__(self, rules_path: Optional[str] = None):
        if Prolog is None:
            raise RuntimeError("pyswipl/SWI-Prolog not available")
        self.prolog = Prolog()
        self.rules_path = rules_path or RULES_PATH_DEFAULT
        self.prolog.consult(self.rules_path)

    def post_junction_state(self, junction_id: str, pattern: int, occupancy: float,
                            counter: int = 0, greentime: int = 0, now: int = 0):
        self.prolog.assertz(
            f"working_memory(junction_state({junction_id},{int(pattern)},{int(counter)},{int(greentime)},{int(now)}))"
        )
        self.prolog.assertz(
            f"working_memory(junction_occupancy({junction_id},{float(occupancy)}))"
        )

    def recommend(self) -> Optional[Tuple[str,int]]:
        res = list(self.prolog.query("supervisor_recommendation(J,NP)"))
        if res:
            return str(res[0]["J"]), int(res[0]["NP"])
        return None

    def clear(self):
        list(self.prolog.query("retractall(working_memory(_))."))
