"""
ZODB-based logging system for traffic simulation data.
Provides persistent storage of simulation metrics using Zope Object Database.
"""

import ZODB
import ZODB.FileStorage
import transaction
from persistent import Persistent
from persistent.list import PersistentList
from BTrees.OOBTree import OOBTree
import datetime


class SimulationRecord(Persistent):
    """A single simulation data record"""
    
    def __init__(self, timestamp=None, **kwargs):
        self.timestamp = timestamp or datetime.datetime.now()
        self.data = OOBTree()
        for key, value in kwargs.items():
            self.data[key] = value
    
    def __setitem__(self, key, value):
        self.data[key] = value
        self._p_changed = True
    
    def __getitem__(self, key):
        return self.data.get(key)
    
    def get(self, key, default=None):
        return self.data.get(key, default)
    
    def to_dict(self):
        """Convert record to dictionary"""
        result = {'timestamp': self.timestamp}
        result.update(dict(self.data))
        return result
    
    def __repr__(self):
        return f"SimulationRecord(timestamp={self.timestamp}, data={dict(self.data)})"


class TrafficSignalRecord(Persistent):
    """Record for traffic signal state changes"""
    
    def __init__(self, timestamp, pattern, queues, turn_counts, flow_rates, rules_fired):
        self.timestamp = timestamp
        self.pattern = pattern
        self.queues = OOBTree(queues) if queues else OOBTree()
        self.turn_counts = OOBTree(turn_counts) if turn_counts else OOBTree()
        self.flow_rates = OOBTree(flow_rates) if flow_rates else OOBTree()
        self.rules_fired = PersistentList(rules_fired) if rules_fired else PersistentList()
    
    def to_dict(self):
        return {
            'timestamp': self.timestamp,
            'pattern': self.pattern,
            'queues': dict(self.queues),
            'turn_counts': dict(self.turn_counts),
            'flow_rates': dict(self.flow_rates),
            'rules_fired': list(self.rules_fired)
        }
    
    def __repr__(self):
        return f"TrafficSignalRecord(timestamp={self.timestamp}, pattern={self.pattern})"


class SimulationSession(Persistent):
    """A complete simulation session with all its records"""
    
    def __init__(self, session_id=None, start_time=None):
        self.session_id = session_id or datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        self.start_time = start_time or datetime.datetime.now()
        self.end_time = None
        self.records = PersistentList()
        self.signal_records = PersistentList()
        self.metadata = OOBTree()
    
    def add_record(self, record):
        """Add a simulation record"""
        self.records.append(record)
        self._p_changed = True
    
    def add_signal_record(self, record):
        """Add a traffic signal record"""
        self.signal_records.append(record)
        self._p_changed = True
    
    def set_metadata(self, key, value):
        """Set session metadata"""
        self.metadata[key] = value
        self._p_changed = True
    
    def finalize(self):
        """Mark session as complete"""
        self.end_time = datetime.datetime.now()
        self._p_changed = True
    
    def get_summary(self):
        """Get session summary statistics"""
        return {
            'session_id': self.session_id,
            'start_time': self.start_time,
            'end_time': self.end_time,
            'total_records': len(self.records),
            'total_signal_records': len(self.signal_records),
            'metadata': dict(self.metadata)
        }
    
    def __repr__(self):
        return f"SimulationSession(id={self.session_id}, records={len(self.records)})"


class ZODBLogger:
    """
    Main logger class for storing simulation data in ZODB.
    Handles connection management and data persistence.
    """
    
    def __init__(self, db_path='traffic_simulation.fs'):
        """
        Initialize ZODB logger.
        
        Args:
            db_path: Path to the ZODB file storage
        """
        self.db_path = db_path
        self.storage = ZODB.FileStorage.FileStorage(db_path)
        self.db = ZODB.DB(self.storage)
        self.connection = self.db.open()
        self.root = self.connection.root()
        
        # Initialize root structure if needed
        if not hasattr(self.root, 'sessions'):
            self.root.sessions = OOBTree()
            transaction.commit()
        
        self.current_session = None
    
    def start_session(self, session_id=None, **metadata):
        """
        Start a new simulation session.
        
        Args:
            session_id: Optional session identifier
            **metadata: Additional metadata for the session
        
        Returns:
            session_id of the new session
        """
        session = SimulationSession(session_id=session_id)
        for key, value in metadata.items():
            session.set_metadata(key, value)
        
        self.current_session = session
        self.root.sessions[session.session_id] = session
        transaction.commit()
        
        return session.session_id
    
    def log_simulation_data(self, **data):
        """
        Log simulation data to current session.
        
        Args:
            **data: Key-value pairs of simulation metrics
        """
        if self.current_session is None:
            raise RuntimeError("No active session. Call start_session() first.")
        
        record = SimulationRecord(**data)
        self.current_session.add_record(record)
        transaction.commit()
    
    def log_signal_state(self, pattern, queues=None, turn_counts=None, 
                         flow_rates=None, rules_fired=None):
        """
        Log traffic signal state change.
        
        Args:
            pattern: Current signal pattern number
            queues: Dictionary of queue lengths by direction
            turn_counts: Dictionary of turn counts by direction
            flow_rates: Dictionary of flow rates by direction
            rules_fired: List of rule names that fired
        """
        if self.current_session is None:
            raise RuntimeError("No active session. Call start_session() first.")
        
        record = TrafficSignalRecord(
            timestamp=datetime.datetime.now(),
            pattern=pattern,
            queues=queues,
            turn_counts=turn_counts,
            flow_rates=flow_rates,
            rules_fired=rules_fired
        )
        self.current_session.add_signal_record(record)
        transaction.commit()
    
    def end_session(self):
        """End the current session"""
        if self.current_session:
            self.current_session.finalize()
            transaction.commit()
            session_id = self.current_session.session_id
            self.current_session = None
            return session_id
        return None
    
    def get_session(self, session_id):
        """Get a session by ID"""
        return self.root.sessions.get(session_id)
    
    def list_sessions(self):
        """List all session IDs"""
        return list(self.root.sessions.keys())
    
    def get_all_sessions(self):
        """Get all sessions"""
        return list(self.root.sessions.values())
    
    def export_session_to_csv(self, session_id, csv_path='exported_data.csv'):
        """
        Export a session's data to CSV format (for compatibility).
        
        Args:
            session_id: Session to export
            csv_path: Output CSV file path
        """
        import csv
        
        session = self.get_session(session_id)
        if not session:
            raise ValueError(f"Session {session_id} not found")
        
        with open(csv_path, 'w', newline='') as csvfile:
            if not session.records:
                return
            
            # Get all possible field names
            fieldnames = set(['timestamp'])
            for record in session.records:
                fieldnames.update(record.data.keys())
            fieldnames = sorted(fieldnames)
            
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            
            for record in session.records:
                writer.writerow(record.to_dict())
        
        print(f"Exported {len(session.records)} records to {csv_path}")
    
    def close(self):
        """Close database connection"""
        if self.current_session:
            self.end_session()
        self.connection.close()
        self.db.close()
        self.storage.close()
    
    def __enter__(self):
        """Context manager entry"""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.close()
    
    def __del__(self):
        """Cleanup on deletion"""
        try:
            self.close()
        except:
            pass


# Convenience function for quick usage
def create_logger(db_path='traffic_simulation.fs'):
    """
    Create and return a new ZODB logger instance.
    
    Args:
        db_path: Path to the ZODB file storage
    
    Returns:
        ZODBLogger instance
    """
    return ZODBLogger(db_path)
