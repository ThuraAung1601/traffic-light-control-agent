"""
HTML Report Generator for Traffic Simulation
Creates visualization reports from ZODB stored data
"""

import os
import datetime
from collections import defaultdict


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
    """Generate HTML content with embedded visualizations"""
    
    summary = session.get_summary()
    
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Traffic Simulation Report - {summary['session_id']}</title>
    <style>
        body {{
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background-color: #f5f5f5;
        }}
        .container {{
            max-width: 1400px;
            margin: 0 auto;
            background-color: white;
            padding: 30px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        h1 {{
            color: #333;
            border-bottom: 3px solid #4CAF50;
            padding-bottom: 10px;
        }}
        h2 {{
            color: #555;
            margin-top: 30px;
            border-bottom: 2px solid #ddd;
            padding-bottom: 8px;
        }}
        .summary {{
            background-color: #f9f9f9;
            padding: 20px;
            border-left: 4px solid #4CAF50;
            margin: 20px 0;
        }}
        .summary-item {{
            margin: 10px 0;
            font-size: 16px;
        }}
        .summary-label {{
            font-weight: bold;
            color: #333;
            display: inline-block;
            width: 200px;
        }}
        .chart-container {{
            margin: 30px 0;
            border: 1px solid #ddd;
            padding: 20px;
            background-color: #fafafa;
        }}
        .chart {{
            width: 100%;
            height: 400px;
            position: relative;
        }}
        canvas {{
            border: 1px solid #ccc;
            background-color: white;
        }}
        .stats-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin: 20px 0;
        }}
        .stat-card {{
            background-color: #f0f0f0;
            padding: 20px;
            border-radius: 8px;
            text-align: center;
        }}
        .stat-value {{
            font-size: 36px;
            font-weight: bold;
            color: #4CAF50;
        }}
        .stat-label {{
            font-size: 14px;
            color: #666;
            margin-top: 10px;
        }}
        .rules-table {{
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }}
        .rules-table th {{
            background-color: #4CAF50;
            color: white;
            padding: 12px;
            text-align: left;
        }}
        .rules-table td {{
            padding: 10px;
            border-bottom: 1px solid #ddd;
        }}
        .rules-table tr:hover {{
            background-color: #f5f5f5;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>Traffic Simulation Report</h1>
        
        <div class="summary">
            <h2>Session Summary</h2>
            <div class="summary-item">
                <span class="summary-label">Session ID:</span>
                <span>{summary['session_id']}</span>
            </div>
            <div class="summary-item">
                <span class="summary-label">Start Time:</span>
                <span>{summary['start_time']}</span>
            </div>
            <div class="summary-item">
                <span class="summary-label">End Time:</span>
                <span>{summary['end_time']}</span>
            </div>
            <div class="summary-item">
                <span class="summary-label">Total Data Points:</span>
                <span>{summary['total_records']}</span>
            </div>
            <div class="summary-item">
                <span class="summary-label">Signal Changes:</span>
                <span>{summary['total_signal_records']}</span>
            </div>
            {_generate_metadata_html(summary.get('metadata', {}))}
        </div>
        
        <div class="stats-grid">
            {_generate_stats_cards(data)}
        </div>
        
        <h2>Traffic Flow Over Time</h2>
        <div class="chart-container">
            <canvas id="vehiclesChart" class="chart"></canvas>
        </div>
        
        <h2>Queue Lengths by Direction</h2>
        <div class="chart-container">
            <canvas id="queuesChart" class="chart"></canvas>
        </div>
        
        <h2>Flow Rates by Direction</h2>
        <div class="chart-container">
            <canvas id="flowRatesChart" class="chart"></canvas>
        </div>
        
        <h2>Signal Pattern Distribution</h2>
        <div class="chart-container">
            <canvas id="patternsChart" class="chart"></canvas>
        </div>
        
        <h2>Rules Fired Statistics</h2>
        {_generate_rules_table(data['rules_fired'])}
    </div>
    
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
    <script>
        const data = {_data_to_json(data)};
        
        // Convert times to integers
        data.times = data.times.map(t => Math.floor(t));
        
        // Chart 1: Vehicles Over Time
        new Chart(document.getElementById('vehiclesChart'), {{
            type: 'line',
            data: {{
                labels: data.times,
                datasets: [
                    {{
                        label: 'Vehicles Passed',
                        data: data.vehicles_passed,
                        borderColor: 'rgb(75, 192, 192)',
                        backgroundColor: 'rgba(75, 192, 192, 0.1)',
                        tension: 0.1
                    }},
                    {{
                        label: 'Vehicles Present',
                        data: data.vehicles_present,
                        borderColor: 'rgb(255, 99, 132)',
                        backgroundColor: 'rgba(255, 99, 132, 0.1)',
                        tension: 0.1
                    }}
                ]
            }},
            options: {{
                responsive: true,
                maintainAspectRatio: false,
                plugins: {{
                    title: {{
                        display: true,
                        text: 'Vehicle Count Over Time'
                    }}
                }},
                scales: {{
                    x: {{
                        title: {{ display: true, text: 'Time (seconds)' }}
                    }},
                    y: {{
                        title: {{ display: true, text: 'Number of Vehicles' }}
                    }}
                }}
            }}
        }});
        
        // Chart 2: Queue Lengths
        const queueDatasets = [];
        const directions = ['north', 'south', 'east', 'west'];
        const directionColors = {{
            'north': 'rgb(54, 162, 235)',
            'south': 'rgb(255, 99, 132)',
            'east': 'rgb(75, 192, 192)',
            'west': 'rgb(255, 159, 64)'
        }};
        
        directions.forEach(dir => {{
            if (data.queues[dir] && data.queues[dir].length > 0) {{
                queueDatasets.push({{
                    label: dir.charAt(0).toUpperCase() + dir.slice(1),
                    data: data.queues[dir],
                    borderColor: directionColors[dir],
                    backgroundColor: directionColors[dir].replace('rgb', 'rgba').replace(')', ', 0.1)'),
                    tension: 0.1
                }});
            }}
        }});
        
        new Chart(document.getElementById('queuesChart'), {{
            type: 'line',
            data: {{
                labels: data.times,
                datasets: queueDatasets
            }},
            options: {{
                responsive: true,
                maintainAspectRatio: false,
                plugins: {{
                    title: {{
                        display: true,
                        text: 'Queue Lengths by Direction'
                    }}
                }},
                scales: {{
                    x: {{
                        title: {{ display: true, text: 'Time (seconds)' }}
                    }},
                    y: {{
                        title: {{ display: true, text: 'Queue Length' }}
                    }}
                }}
            }}
        }});
        
        // Chart 3: Flow Rates
        const flowDatasets = [];
        directions.forEach(dir => {{
            if (data.flow_rates[dir] && data.flow_rates[dir].length > 0) {{
                flowDatasets.push({{
                    label: dir.charAt(0).toUpperCase() + dir.slice(1),
                    data: data.flow_rates[dir],
                    borderColor: directionColors[dir],
                    backgroundColor: directionColors[dir].replace('rgb', 'rgba').replace(')', ', 0.1)'),
                    tension: 0.1
                }});
            }}
        }});
        
        new Chart(document.getElementById('flowRatesChart'), {{
            type: 'line',
            data: {{
                labels: data.times,
                datasets: flowDatasets
            }},
            options: {{
                responsive: true,
                maintainAspectRatio: false,
                plugins: {{
                    title: {{
                        display: true,
                        text: 'Traffic Flow Rates by Direction'
                    }}
                }},
                scales: {{
                    x: {{
                        title: {{ display: true, text: 'Time (seconds)' }}
                    }},
                    y: {{
                        title: {{ display: true, text: 'Flow Rate (vehicles/second)' }}
                    }}
                }}
            }}
        }});
        
        // Chart 4: Pattern Distribution
        const patternCounts = {{}};
        data.patterns.forEach(p => {{
            patternCounts[p] = (patternCounts[p] || 0) + 1;
        }});
        
        new Chart(document.getElementById('patternsChart'), {{
            type: 'bar',
            data: {{
                labels: Object.keys(patternCounts).sort((a, b) => a - b),
                datasets: [{{
                    label: 'Usage Count',
                    data: Object.keys(patternCounts).sort((a, b) => a - b).map(k => patternCounts[k]),
                    backgroundColor: 'rgba(75, 192, 192, 0.6)',
                    borderColor: 'rgb(75, 192, 192)',
                    borderWidth: 1
                }}]
            }},
            options: {{
                responsive: true,
                maintainAspectRatio: false,
                plugins: {{
                    title: {{
                        display: true,
                        text: 'Signal Pattern Usage Distribution'
                    }}
                }},
                scales: {{
                    x: {{
                        title: {{ display: true, text: 'Pattern Number' }}
                    }},
                    y: {{
                        title: {{ display: true, text: 'Count' }}
                    }}
                }}
            }}
        }});
    </script>
</body>
</html>
"""
    return html


def _generate_metadata_html(metadata):
    """Generate HTML for session metadata"""
    if not metadata:
        return ""
    
    html = ""
    for key, value in metadata.items():
        html += f"""
            <div class="summary-item">
                <span class="summary-label">{key.replace('_', ' ').title()}:</span>
                <span>{value}</span>
            </div>"""
    return html


def _generate_stats_cards(data):
    """Generate HTML for statistics cards"""
    cards = []
    
    if data['vehicles_passed']:
        total_passed = data['vehicles_passed'][-1] if data['vehicles_passed'] else 0
        cards.append(_stat_card("Total Vehicles Passed", total_passed))
    
    if data['vehicles_present']:
        avg_present = sum(data['vehicles_present']) / len(data['vehicles_present']) if data['vehicles_present'] else 0
        cards.append(_stat_card("Avg Vehicles Present", f"{avg_present:.1f}"))
    
    if data['traffic_density']:
        avg_density = sum(data['traffic_density']) / len(data['traffic_density']) if data['traffic_density'] else 0
        cards.append(_stat_card("Avg Traffic Density", f"{avg_density:.3f}"))
    
    if data['patterns']:
        unique_patterns = len(set(data['patterns']))
        cards.append(_stat_card("Unique Patterns Used", unique_patterns))
    
    return "\n".join(cards)


def _stat_card(label, value):
    """Generate a single stat card"""
    return f"""
        <div class="stat-card">
            <div class="stat-value">{value}</div>
            <div class="stat-label">{label}</div>
        </div>"""


def _generate_rules_table(rules_fired):
    """Generate HTML table for rules fired statistics"""
    if not rules_fired:
        return "<p>No rule firing data available.</p>"
    
    rows = ""
    for rule, count in sorted(rules_fired.items(), key=lambda x: x[1], reverse=True):
        rows += f"""
            <tr>
                <td>{rule}</td>
                <td>{count}</td>
            </tr>"""
    
    return f"""
        <table class="rules-table">
            <thead>
                <tr>
                    <th>Rule</th>
                    <th>Times Fired</th>
                </tr>
            </thead>
            <tbody>
                {rows}
            </tbody>
        </table>"""


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
