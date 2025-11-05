"""
Query Tool for ZODB Traffic Simulation Data
Allows viewing and exporting simulation data
"""

from trafficSim.db_logger import ZODBLogger
import sys


def list_sessions(db_path='traffic_simulation.fs'):
    """List all sessions in the database"""
    with ZODBLogger(db_path) as logger:
        sessions = logger.get_all_sessions()
        
        if not sessions:
            print("No sessions found in database.")
            return
        
        print(f"\n{'='*80}")
        print(f"Found {len(sessions)} session(s) in database")
        print(f"{'='*80}\n")
        
        for session in sessions:
            summary = session.get_summary()
            print(f"Session ID: {summary['session_id']}")
            print(f"  Start: {summary['start_time']}")
            print(f"  End: {summary['end_time']}")
            print(f"  Records: {summary['total_records']}")
            print(f"  Signal Changes: {summary['total_signal_records']}")
            
            metadata = summary.get('metadata', {})
            if metadata:
                print(f"  Metadata:")
                for key, value in metadata.items():
                    print(f"    {key}: {value}")
            print()


def view_session(session_id, db_path='traffic_simulation.fs'):
    """View detailed data for a specific session"""
    with ZODBLogger(db_path) as logger:
        session = logger.get_session(session_id)
        
        if not session:
            print(f"Session '{session_id}' not found.")
            return
        
        summary = session.get_summary()
        print(f"\n{'='*80}")
        print(f"Session: {summary['session_id']}")
        print(f"{'='*80}\n")
        
        # Show sample records
        print(f"Total Records: {len(session.records)}")
        if session.records:
            print(f"\nFirst Record:")
            print(session.records[0].to_dict())
            
            if len(session.records) > 1:
                print(f"\nLast Record:")
                print(session.records[-1].to_dict())
        
        # Show signal changes
        print(f"\n\nTotal Signal Changes: {len(session.signal_records)}")
        if session.signal_records:
            print(f"\nFirst 5 Signal Changes:")
            for i, record in enumerate(session.signal_records[:5]):
                print(f"  {i+1}. Pattern {record.pattern} at {record.timestamp}")
                if record.rules_fired:
                    print(f"     Rules: {', '.join(record.rules_fired[:5])}")


def export_to_csv(session_id, csv_path=None, db_path='traffic_simulation.fs'):
    """Export session data to CSV"""
    if csv_path is None:
        csv_path = f'export_{session_id}.csv'
    
    with ZODBLogger(db_path) as logger:
        session = logger.get_session(session_id)
        
        if not session:
            print(f"Session '{session_id}' not found.")
            return
        
        logger.export_session_to_csv(session_id, csv_path)
        print(f"Exported to: {csv_path}")


def generate_report(session_id, db_path='traffic_simulation.fs'):
    """Generate HTML report for a session"""
    from trafficSim.report_generator import generate_html_report
    
    with ZODBLogger(db_path) as logger:
        session = logger.get_session(session_id)
        
        if not session:
            print(f"Session '{session_id}' not found.")
            return
        
        report_path = generate_html_report(logger)
        print(f"Report generated: {report_path}")
        
        # Open in browser
        import webbrowser
        webbrowser.open('file://' + report_path)


def main():
    if len(sys.argv) < 2:
        print("Traffic Simulation ZODB Query Tool")
        print("\nUsage:")
        print("  python query_db.py list                    - List all sessions")
        print("  python query_db.py view <session_id>       - View session details")
        print("  python query_db.py export <session_id>     - Export to CSV")
        print("  python query_db.py report <session_id>     - Generate HTML report")
        return
    
    command = sys.argv[1]
    
    if command == 'list':
        list_sessions()
    elif command == 'view' and len(sys.argv) >= 3:
        view_session(sys.argv[2])
    elif command == 'export' and len(sys.argv) >= 3:
        export_to_csv(sys.argv[2])
    elif command == 'report' and len(sys.argv) >= 3:
        generate_report(sys.argv[2])
    else:
        print("Invalid command or missing arguments.")
        print("Use 'python query_db.py' for usage information.")


if __name__ == '__main__':
    main()
