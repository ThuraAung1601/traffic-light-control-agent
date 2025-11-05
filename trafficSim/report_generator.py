"""
HTML Report Generator for Traffic Simulation
Creates visualization reports from ZODB stored data
"""

import os
import json
import datetime
from collections import defaultdict
from jinja2 import Environment, FileSystemLoader, select_autoescape


def generate_html_report(db_logger, output_path=None):
    """
    Generate an HTML report with visualizations from ZODB data.
    
    Args:
        db_logger: ZODBLogger instance with simulation data
        output_path: Optional path for output HTML file
    
    Returns:
        Path to generated HTML file
    """
    if output_path is None:
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        output_path = os.path.abspath(f"traffic_simulation_report_{timestamp}.html")
    
    # Get current session data
    if db_logger.current_session:
        session = db_logger.current_session
    else:
        # Get most recent session
        sessions = db_logger.get_all_sessions()
        if not sessions:
            raise ValueError("No simulation data found in database")
        session = sessions[-1]
    
    # Extract data for visualizations
    data = _extract_session_data(session)
    
    # Generate HTML
    html_content = _generate_html(session, data)
    
    # Write to file
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html_content)
    
    return output_path


def _extract_session_data(session):
    """Extract and organize data from session for visualization"""
    data = {
        'times': [],
        'vehicles_passed': [],
        'vehicles_present': [],
        'traffic_density': [],
        'speed_multiplier': [],
        'vehicle_rate': [],
        'patterns': [],
        'queues': defaultdict(list),
        'flow_rates': defaultdict(list),
        'turn_ratios': defaultdict(list),
        'rules_fired': defaultdict(int)
    }
    
    # Extract simulation records
    for record in session.records:
        data['times'].append(record.get('time', 0))
        data['vehicles_passed'].append(record.get('vehicles_passed', 0))
        data['vehicles_present'].append(record.get('vehicles_present', 0))
        data['traffic_density'].append(record.get('traffic_density', 0))
        data['speed_multiplier'].append(record.get('speed_multiplier', 1.0))
        data['vehicle_rate'].append(record.get('vehicle_rate', 0))
        
        # Extract pattern if available
        pattern = record.get('pattern', 0)
        data['patterns'].append(pattern)
        
        # Extract queue data
        queues = record.get('queues')
        if queues:
            for direction, length in queues.items():
                data['queues'][direction].append(length)
        
        # Extract flow rates
        flow_rates = record.get('flow_rates')
        if flow_rates:
            for direction, rate in flow_rates.items():
                data['flow_rates'][direction].append(rate)
        
        # Extract turn ratios
        turn_ratios = record.get('turn_ratios')
        if turn_ratios:
            for direction, ratio in turn_ratios.items():
                data['turn_ratios'][direction].append(ratio)
    
    # Extract signal records for pattern changes and rules
    for signal_record in session.signal_records:
        for rule in signal_record.rules_fired:
            data['rules_fired'][rule] = data['rules_fired'].get(rule, 0) + 1
    
    return data


def _generate_html(session, data):
    """Generate HTML content using Jinja2 template"""
    
    summary = session.get_summary()
    
    # Get the directory where this script is located
    script_dir = os.path.dirname(os.path.abspath(__file__))
    template_dir = os.path.join(script_dir, 'templates')
    
    # Set up Jinja2 environment
    env = Environment(
        loader=FileSystemLoader(template_dir),
        autoescape=select_autoescape(['html', 'xml'])
    )
    
    # Load template
    template = env.get_template('report.html')
    
    # Prepare stats cards
    stats_cards = _prepare_stats_cards(data)
    
    # Prepare rules fired data (sorted by count, descending)
    rules_fired = sorted(data['rules_fired'].items(), key=lambda x: x[1], reverse=True)
    
    # Convert data to JSON for JavaScript
    data_json = _data_to_json(data)
    
    # Render template
    html = template.render(
        summary=summary,
        metadata=summary.get('metadata', {}),
        stats_cards=stats_cards,
        rules_fired=rules_fired,
        data_json=data_json
    )
    
    return html


def _prepare_stats_cards(data):
    """Prepare statistics cards data"""
    cards = []
    
    if data['vehicles_passed']:
        total_passed = data['vehicles_passed'][-1] if data['vehicles_passed'] else 0
        cards.append({'label': 'Total Vehicles Passed', 'value': total_passed})
    
    if data['vehicles_present']:
        avg_present = sum(data['vehicles_present']) / len(data['vehicles_present']) if data['vehicles_present'] else 0
        cards.append({'label': 'Avg Vehicles Present', 'value': f"{avg_present:.1f}"})
    
    if data['patterns']:
        unique_patterns = len(set(data['patterns']))
        cards.append({'label': 'Unique Patterns Used', 'value': unique_patterns})
    
    return cards


def _data_to_json(data):
    """Convert data dictionary to JSON string for JavaScript"""
    import json
    
    # Convert defaultdicts to regular dicts
    json_data = {
        'times': data['times'],
        'vehicles_passed': data['vehicles_passed'],
        'vehicles_present': data['vehicles_present'],
        'traffic_density': data['traffic_density'],
        'speed_multiplier': data['speed_multiplier'],
        'vehicle_rate': data['vehicle_rate'],
        'patterns': data['patterns'],
        'queues': dict(data['queues']),
        'flow_rates': dict(data['flow_rates']),
        'turn_ratios': dict(data['turn_ratios'])
    }
    
    return json.dumps(json_data)
