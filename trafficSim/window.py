import pygame
from pygame import gfxdraw
import numpy as np

class Slider:
    """A simple slider control for adjusting values"""
    def __init__(self, x, y, width, height, min_val, max_val, current_val, label, format_str="{:.1f}"):
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.min_val = min_val
        self.max_val = max_val
        self.value = current_val
        self.label = label
        self.format_str = format_str
        self.dragging = False
        
    def draw(self, screen, font):
        # Draw slider background
        pygame.draw.rect(screen, (200, 200, 200), (self.x, self.y, self.width, self.height))
        
        # Draw slider fill
        fill_width = int((self.value - self.min_val) / (self.max_val - self.min_val) * self.width)
        pygame.draw.rect(screen, (100, 150, 255), (self.x, self.y, fill_width, self.height))
        
        # Draw slider border
        pygame.draw.rect(screen, (100, 100, 100), (self.x, self.y, self.width, self.height), 2)
        
        # Draw handle
        handle_x = self.x + fill_width
        pygame.draw.circle(screen, (50, 50, 50), (handle_x, self.y + self.height // 2), self.height // 2 + 2)
        pygame.draw.circle(screen, (255, 255, 255), (handle_x, self.y + self.height // 2), self.height // 2 - 2)
        
        # Draw label and value
        label_text = font.render(f"{self.label}: {self.format_str.format(self.value)}", True, (50, 50, 50))
        screen.blit(label_text, (self.x, self.y - 25))
        
    def handle_event(self, event, mouse_pos):
        """Handle mouse events for the slider"""
        if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
            # Check if click is on slider
            if (self.x <= mouse_pos[0] <= self.x + self.width and 
                self.y <= mouse_pos[1] <= self.y + self.height):
                self.dragging = True
                self.update_value(mouse_pos[0])
                return True
        elif event.type == pygame.MOUSEBUTTONUP and event.button == 1:
            self.dragging = False
        elif event.type == pygame.MOUSEMOTION and self.dragging:
            self.update_value(mouse_pos[0])
            return True
        return False
    
    def update_value(self, mouse_x):
        """Update slider value based on mouse position"""
        relative_x = max(0, min(self.width, mouse_x - self.x))
        ratio = relative_x / self.width
        self.value = self.min_val + ratio * (self.max_val - self.min_val)


class Button:
    """A simple button control"""
    def __init__(self, x, y, width, height, label, color=(200, 50, 50), hover_color=(255, 100, 100)):
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.label = label
        self.color = color
        self.hover_color = hover_color
        self.is_hovered = False
        self.rect = pygame.Rect(x, y, width, height)
    
    def draw(self, screen, font):
        # Draw button background
        current_color = self.hover_color if self.is_hovered else self.color
        pygame.draw.rect(screen, current_color, self.rect)
        pygame.draw.rect(screen, (100, 100, 100), self.rect, 2)
        
        # Draw label
        label_text = font.render(self.label, True, (255, 255, 255))
        text_rect = label_text.get_rect(center=self.rect.center)
        screen.blit(label_text, text_rect)
    
    def handle_event(self, event, mouse_pos):
        """Handle mouse events for the button"""
        # Update hover state
        self.is_hovered = self.rect.collidepoint(mouse_pos)
        
        if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
            if self.is_hovered:
                return True
        return False


class Window:
    def __init__(self, sim, config={}):
        # Simulation to draw
        self.sim = sim

        # Set default configurations
        self.set_default_config()

        # Update configurations
        for attr, val in config.items():
            setattr(self, attr, val)
        
    def set_default_config(self):
        """Set default configuration"""
        self.width = 1400
        self.height = 900
        self.bg_color = (250, 250, 250)

        self.fps = 60
        self.zoom = 5
        self.offset = (0, 0)

        self.mouse_last = (0, 0)
        self.mouse_down = False

        # flip x axis
        self.flip_x = True
        
        # Control panel settings
        self.show_control_panel = True
        self.control_panel_height = 150
        self.control_panel_bg = (240, 240, 245)
        self.control_panel_border = (180, 180, 190)
        
        # Sliders for control panel
        self.sliders = []
        self.active_slider = None
        
        # End simulation button
        self.end_button = None
        self.end_button_rect = None


    def loop(self, loop=None):
        """Shows a window visualizing the simulation and runs the loop function."""
        
        # Create a pygame window
        self.screen = pygame.display.set_mode((self.width, self.height))
        pygame.display.flip()

        # Fixed fps
        clock = pygame.time.Clock()

        # To draw text
        pygame.font.init()
        self.text_font = pygame.font.SysFont('Lucida Console', 16)
        self.control_font = pygame.font.SysFont('Arial', 14)
        
        # Initialize control panel sliders
        self._init_control_panel()

        # Draw loop
        running = True
        while running:
            # Update simulation only if not paused
            if loop and not self.sim.isPaused: 
                loop(self.sim)

            # Draw simulation
            self.draw()

            # Update window
            pygame.display.update()
            clock.tick(self.fps)

            # Handle all events
            for event in pygame.event.get():
                # Quit program if window is closed
                if event.type == pygame.QUIT:
                    running = False
                    
                # Handle control panel slider and button events first
                mouse_pos = pygame.mouse.get_pos()
                slider_handled = False
                button_clicked = False
                
                if self.show_control_panel and mouse_pos[1] < self.control_panel_height:
                    # Check button click first
                    if self.end_button and self.end_button.handle_event(event, mouse_pos):
                        button_clicked = True
                        self._end_simulation()
                        running = False
                        break
                    
                    # Then check sliders
                    for slider in self.sliders:
                        if slider.handle_event(event, mouse_pos):
                            slider_handled = True
                            self._update_from_sliders()
                            break
                
                # Skip other mouse events if slider is being used
                if (slider_handled or button_clicked) and event.type in (pygame.MOUSEBUTTONDOWN, pygame.MOUSEMOTION):
                    continue
                    
                # Handle keyboard events for speed control
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_PLUS or event.key == pygame.K_EQUALS:
                        # Speed up simulation
                        self.sim.speed_up()
                        print(f"Speed: {self.sim.speed_multiplier:.1f}x")
                    elif event.key == pygame.K_MINUS:
                        # Slow down simulation
                        self.sim.slow_down()
                        print(f"Speed: {self.sim.speed_multiplier:.1f}x")
                    elif event.key == pygame.K_r:
                        # Reset speed to normal
                        self.sim.set_speed(1.0)
                        print("Speed: 1.0x (normal)")
                    elif event.key == pygame.K_SPACE:
                        # Pause/resume simulation
                        if self.sim.isPaused:
                            self.sim.resume()
                            print("Simulation resumed")
                        else:
                            self.sim.pause()
                            print("Simulation paused")
                # Handle mouse events
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    # If mouse button down
                    if event.button == 1:
                        # Left click - only for dragging if not in control panel
                        x, y = pygame.mouse.get_pos()
                        if not (self.show_control_panel and y < self.control_panel_height):
                            x0, y0 = self.offset
                            self.mouse_last = (x-x0*self.zoom, y-y0*self.zoom)
                            self.mouse_down = True
                    if event.button == 4:
                        # Mouse wheel up
                        self.zoom *=  (self.zoom**2+self.zoom/4+1) / (self.zoom**2+1)
                    if event.button == 5:
                        # Mouse wheel down 
                        self.zoom *= (self.zoom**2+1) / (self.zoom**2+self.zoom/4+1)
                elif event.type == pygame.MOUSEMOTION:
                    # Drag content
                    if self.mouse_down:
                        x1, y1 = self.mouse_last
                        x2, y2 = pygame.mouse.get_pos()
                        self.offset = ((x2-x1)/self.zoom, (y2-y1)/self.zoom)
                elif event.type == pygame.MOUSEBUTTONUP:
                    self.mouse_down = False           

    def run(self, steps_per_update=1):
        """Runs the simulation by updating in every loop."""
        def loop(sim):
            sim.run(steps_per_update)
        self.loop(loop)

    def convert(self, x, y=None):
        """Converts simulation coordinates to screen coordinates"""
        if isinstance(x, list):
            return [self.convert(e[0], e[1]) for e in x]
        if isinstance(x, tuple):
            return self.convert(*x)
        
        # Adjust vertical center to account for control panel
        vertical_offset = self.control_panel_height // 2 if self.show_control_panel else 0
        
        return (
            int(self.width/2 + (x + self.offset[0])*self.zoom),
            int((self.height + vertical_offset)/2 + (y + self.offset[1])*self.zoom)
        )

    def inverse_convert(self, x, y=None):
        """Converts screen coordinates to simulation coordinates"""
        if isinstance(x, list):
            return [self.convert(e[0], e[1]) for e in x]
        if isinstance(x, tuple):
            return self.convert(*x)
        return (
            int(-self.offset[0] + (x - self.width/2)/self.zoom),
            int(-self.offset[1] + (y - self.height/2)/self.zoom)
        )


    def _init_control_panel(self):
        """Initialize control panel sliders"""
        if not self.show_control_panel:
            return
            
        # Get initial values from simulation
        initial_speed = self.sim.speed_multiplier if hasattr(self.sim, 'speed_multiplier') else 1.0
        initial_vehicle_rate = 400  # Default from main.py
        
        # Get vehicle generator to access current vehicle rate
        if hasattr(self.sim, 'generators') and len(self.sim.generators) > 0:
            initial_vehicle_rate = self.sim.generators[0].vehicle_rate
        
        # Get vehicle type probabilities from first generator
        car_prob = 4
        truck_prob = 1
        bus_prob = 1
        motorcycle_prob = 2
        
        if hasattr(self.sim, 'generators') and len(self.sim.generators) > 0:
            vehicles = self.sim.generators[0].vehicles
            if len(vehicles) >= 4:
                car_prob = vehicles[0][0]
                truck_prob = vehicles[1][0]
                bus_prob = vehicles[2][0]
                motorcycle_prob = vehicles[3][0]
        
        # Create sliders
        slider_y = 50
        slider_spacing = 180
        slider_x = 20
        
        self.sliders = [
            Slider(slider_x, slider_y, 150, 20, 0.1, 10.0, initial_speed, 
                   "Speed", "{:.1f}x"),
            Slider(slider_x + slider_spacing, slider_y, 150, 20, 50, 1000, initial_vehicle_rate,
                   "Vehicle Rate", "{:.0f}/min"),
            Slider(slider_x + slider_spacing * 2, slider_y, 150, 20, 0, 10, car_prob,
                   "Car", "{:.0f}"),
            Slider(slider_x + slider_spacing * 3, slider_y, 150, 20, 0, 10, truck_prob,
                   "Truck", "{:.0f}"),
            Slider(slider_x + slider_spacing * 4, slider_y, 150, 20, 0, 10, bus_prob,
                   "Bus", "{:.0f}"),
            Slider(slider_x + slider_spacing * 5, slider_y, 150, 20, 0, 10, motorcycle_prob,
                   "Motorcycle", "{:.0f}"),
        ]
        
        # Create End Simulation button
        button_x = slider_x + slider_spacing * 6 + 50
        button_y = slider_y - 5
        self.end_button = Button(button_x, button_y, 150, 30, "End Simulation", 
                                  color=(200, 50, 50), hover_color=(255, 80, 80))
    
    def _update_from_sliders(self):
        """Update simulation parameters from slider values"""
        if not self.sliders:
            return
        
        # Update simulation speed
        speed_slider = self.sliders[0]
        if hasattr(self.sim, 'set_speed'):
            self.sim.set_speed(speed_slider.value)
        
        # Update vehicle rate for all generators
        rate_slider = self.sliders[1]
        if hasattr(self.sim, 'generators'):
            for gen in self.sim.generators:
                gen.vehicle_rate = rate_slider.value
        
        # Update vehicle type probabilities
        if hasattr(self.sim, 'generators') and len(self.sliders) >= 6:
            car_weight = int(self.sliders[2].value)
            truck_weight = int(self.sliders[3].value)
            bus_weight = int(self.sliders[4].value)
            motorcycle_weight = int(self.sliders[5].value)
            
            for gen in self.sim.generators:
                if len(gen.vehicles) >= 4:
                    gen.vehicles[0] = (car_weight, gen.vehicles[0][1])
                    gen.vehicles[1] = (truck_weight, gen.vehicles[1][1])
                    gen.vehicles[2] = (bus_weight, gen.vehicles[2][1])
                    gen.vehicles[3] = (motorcycle_weight, gen.vehicles[3][1])
    
    def _end_simulation(self):
        """End simulation and generate HTML report"""
        print("\n=== Ending Simulation ===")
        
        # Stop logging and close database
        if hasattr(self.sim, 'stop_logging'):
            session_id = self.sim.stop_logging()
            print(f"Session ended: {session_id}")
        
        # Generate HTML report
        if hasattr(self.sim, 'db_logger') and self.sim.db_logger:
            from .report_generator import generate_html_report
            report_path = generate_html_report(self.sim.db_logger)
            print(f"Report generated: {report_path}")
            
            # Open report in browser
            import webbrowser
            webbrowser.open('file://' + report_path)
        
        # Close logger
        if hasattr(self.sim, 'close_logger'):
            self.sim.close_logger()
        
        print("Simulation ended. Report opened in browser.")
    
    def _draw_control_panel(self):
        """Draw the control panel at the top of the screen"""
        if not self.show_control_panel:
            return
        
        # Draw panel background
        pygame.draw.rect(self.screen, self.control_panel_bg, 
                        (0, 0, self.width, self.control_panel_height))
        pygame.draw.line(self.screen, self.control_panel_border, 
                        (0, self.control_panel_height), 
                        (self.width, self.control_panel_height), 2)
        
        # Draw title
        title = self.control_font.render("Simulation Controls", True, (50, 50, 50))
        self.screen.blit(title, (self.width // 2 - title.get_width() // 2, 10))
        
        # Draw sliders
        for slider in self.sliders:
            slider.draw(self.screen, self.control_font)
        
        # Draw end button
        if self.end_button:
            mouse_pos = pygame.mouse.get_pos()
            self.end_button.handle_event(pygame.event.Event(pygame.MOUSEMOTION), mouse_pos)
            self.end_button.draw(self.screen, self.control_font)
        
        # Draw instructions
        instructions = self.control_font.render(
            "Keyboard: SPACE=Pause, +/-=Speed, R=Reset | Mouse: Drag to pan, Scroll to zoom",
            True, (100, 100, 100))
        self.screen.blit(instructions, (20, self.control_panel_height - 30))

    def background(self, r, g, b):
        """Fills screen with one color."""
        self.screen.fill((r, g, b))

    def line(self, start_pos, end_pos, color):
        gfxdraw.line(
            self.screen,
            *start_pos,
            *end_pos,
            color
        )

    def rect(self, pos, size, color):
        gfxdraw.rectangle(self.screen, (*pos, *size), color)

    def box(self, pos, size, color):
        gfxdraw.box(self.screen, (*pos, *size), color)

    def circle(self, pos, radius, color, filled=True):
        gfxdraw.aacircle(self.screen, *pos, radius, color)
        if filled:
            gfxdraw.filled_circle(self.screen, *pos, radius, color)



    def polygon(self, vertices, color, filled=True):
        gfxdraw.aapolygon(self.screen, vertices, color)
        if filled:
            gfxdraw.filled_polygon(self.screen, vertices, color)

    def rotated_box(self, pos, size, angle=None, cos=None, sin=None, centered=True, color=(0, 0, 255), filled=True):
        """Draws a rectangle center at *pos* with size *size* rotated anti-clockwise by *angle*."""
        x, y = pos
        l, h = size

        if angle:
            cos, sin = np.cos(angle), np.sin(angle)
        
        vertex = lambda e1, e2: (
            x + (e1*l*cos + e2*h*sin)/2,
            y + (e1*l*sin - e2*h*cos)/2
        )

        if centered:
            vertices = self.convert(
                [vertex(*e) for e in [(-1,-1), (-1, 1), (1,1), (1,-1)]]
            )
        else:
            vertices = self.convert(
                [vertex(*e) for e in [(0,-1), (0, 1), (2,1), (2,-1)]]
            )

        self.polygon(vertices, color, filled=filled)

    def rotated_rect(self, pos, size, angle=None, cos=None, sin=None, centered=True, color=(0, 0, 255)):
        self.rotated_box(pos, size, angle=angle, cos=cos, sin=sin, centered=centered, color=color, filled=False)

    def arrow(self, pos, size, angle=None, cos=None, sin=None, color=(150, 150, 190)):
        if angle:
            cos, sin = np.cos(angle), np.sin(angle)
        
        self.rotated_box(
            pos,
            size,
            cos=(cos - sin) / np.sqrt(2),
            sin=(cos + sin) / np.sqrt(2),
            color=color,
            centered=False
        )

        self.rotated_box(
            pos,
            size,
            cos=(cos + sin) / np.sqrt(2),
            sin=(sin - cos) / np.sqrt(2),
            color=color,
            centered=False
        )


    def draw_axes(self, color=(100, 100, 100)):
        x_start, y_start = self.inverse_convert(0, 0)
        x_end, y_end = self.inverse_convert(self.width, self.height)
        self.line(
            self.convert((0, y_start)),
            self.convert((0, y_end)),
            color
        )
        self.line(
            self.convert((x_start, 0)),
            self.convert((x_end, 0)),
            color
        )

    def draw_grid(self, unit=50, color=(150,150,150)):
        x_start, y_start = self.inverse_convert(0, 0)
        x_end, y_end = self.inverse_convert(self.width, self.height)

        n_x = int(x_start / unit)
        n_y = int(y_start / unit)
        m_x = int(x_end / unit)+1
        m_y = int(y_end / unit)+1

        for i in range(n_x, m_x):
            self.line(
                self.convert((unit*i, y_start)),
                self.convert((unit*i, y_end)),
                color
            )
        for i in range(n_y, m_y):
            self.line(
                self.convert((x_start, unit*i)),
                self.convert((x_end, unit*i)),
                color
            )

    def draw_roads(self):
        for road in self.sim.roads:
            # Draw road background
            self.rotated_box(
                road.start,
                (road.length, 3.7),
                cos=road.angle_cos,
                sin=road.angle_sin,
                color=(180, 180, 220),
                centered=False
            )
            # Draw road lines
            # self.rotated_box(
            #     road.start,
            #     (road.length, 0.25),
            #     cos=road.angle_cos,
            #     sin=road.angle_sin,
            #     color=(0, 0, 0),
            #     centered=False
            # )

            # Draw road arrow
            if road.length > 5: 
                for i in np.arange(-0.5*road.length, 0.5*road.length, 10):
                    pos = (
                        road.start[0] + (road.length/2 + i + 3) * road.angle_cos,
                        road.start[1] + (road.length/2 + i + 3) * road.angle_sin
                    )

                    self.arrow(
                        pos,
                        (-1.25, 0.2),
                        cos=road.angle_cos,
                        sin=road.angle_sin
                    )   

    def draw_vehicle(self, vehicle, road):
        l, h = vehicle.l,  vehicle.h
        sin, cos = road.angle_sin, road.angle_cos

        x = road.start[0] + cos * vehicle.x 
        y = road.start[1] + sin * vehicle.x 

        self.rotated_box((x, y), (l, h), cos=cos, sin=sin, color=vehicle.color, centered=True)

    def draw_vehicles(self):
        for road in self.sim.roads:
            # Draw vehicles
            for vehicle in road.vehicles:
                self.draw_vehicle(vehicle, road)

    def draw_signals(self):
        for signal in self.sim.traffic_signals:
            for i in range(len(signal.roads)):
                color = (0, 255, 0) if signal.current_cycle[i] else (255, 0, 0)
                for road in signal.roads[i]:
                    a = 0
                    position = (
                        (1-a)*road.end[0] + a*road.start[0],        
                        (1-a)*road.end[1] + a*road.start[1]
                    )
                    self.rotated_box(
                        position,
                        (1, 3),
                        cos=road.angle_cos, sin=road.angle_sin,
                        color=color)
                    

    def draw_status(self):
        text_fps = self.text_font.render(f't={self.sim.t:.5}', False, (0, 0, 0))
        text_frc = self.text_font.render(f'n={self.sim.frame_count}', False, (0, 0, 0))
        vehicles_passed = int(self.sim.vehiclesPassed)
        text_vehicles_passed = self.text_font.render(f'Vehicles Passed={vehicles_passed}', False, (0, 0, 0))
        text_vehicles_present = self.text_font.render(f'Vehicles Present={self.sim.vehiclesPresent}', False, (0, 0, 0))
        # Fix division by zero for timing display
        avg_rate = int(vehicles_passed/max(self.sim.t, 0.001)*60) if self.sim.t > 0 else 0
        text_average_vehicles_per_minute = self.text_font.render(f'Avg/min={avg_rate}', False, (0, 0, 0))
        text_total_vehicles = self.text_font.render(f'Total Vehicles={vehicles_passed + self.sim.vehiclesPresent}', False, (0, 0, 0))
        text_vehicle_rate = self.text_font.render(f'Vehicle Rate={self.sim.vehicleRate}', False, (0, 0, 0))
        text_speed = self.text_font.render(f'Speed={self.sim.speed_multiplier:.1f}x', False, (0, 0, 0))
        text_controls = self.text_font.render('Controls: +/- Speed, R Reset, Space Pause', False, (100, 100, 100))

        # Traffic signal status and rule tracking
        signal_status = "Signal: Unknown"
        rule_status = ""
        for signal in self.sim.traffic_signals:
            signal_status = f'Signal: Pattern {signal.current_pattern} ({signal.current_cycle_time:.0f}s) [{signal.control_type.upper()}]'
            # Show fired rules if using Prolog agent
            if hasattr(signal, 'prolog_agent') and signal.prolog_agent:
                try:
                    rule_summary = signal.prolog_agent.get_rule_summary()
                    rule_status = rule_summary[:80] + "..." if len(rule_summary) > 80 else rule_status
                except Exception:
                    rule_status = "Rule tracking unavailable"
            break  # Just show first signal

        text_signal_status = self.text_font.render(signal_status, False, (0, 0, 150))
        
        # Draw status at bottom of screen
        status_height = 100
        status_y = self.height - status_height
        
        # White rectangle at bottom for status info
        self.screen.fill((255, 255, 255), (0, status_y, 1400, status_height))
        self.screen.blit(text_fps, (0, status_y))
        self.screen.blit(text_frc, (100, status_y))
        self.screen.blit(text_vehicles_passed, (200, status_y))
        self.screen.blit(text_vehicles_present, (400, status_y))
        self.screen.blit(text_average_vehicles_per_minute, (630, status_y))
        self.screen.blit(text_total_vehicles, (0, status_y + 20))
        self.screen.blit(text_vehicle_rate, (200, status_y + 20))
        self.screen.blit(text_speed, (400, status_y + 20))
        self.screen.blit(text_signal_status, (0, status_y + 40))

        # Show rule status if available
        if rule_status:
            text_rule_status = self.text_font.render(rule_status, False, (150, 0, 150))
            self.screen.blit(text_rule_status, (0, status_y + 60))

        if self.sim.isPaused:
            text_pause = self.text_font.render(f'PAUSED - Press Space to Resume', False, (255, 0, 0))
        else:
            text_pause = self.text_font.render(f'Running', False, (0, 150, 0))
        self.screen.blit(text_pause, (900, 0))
        



    def draw(self):
        # Fill background
        self.background(*self.bg_color)

        # Major and minor grid and axes
        self.draw_grid(10, (220,220,220))
        self.draw_grid(100, (200,200,200))
        self.draw_axes()

        self.draw_roads()
        self.draw_vehicles()
        self.draw_signals()

        # Draw status info
        self.draw_status()
        
        # Draw control panel on top
        self._draw_control_panel()
        