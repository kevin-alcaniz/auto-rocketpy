### FARADAY ROCKETRY UPV ###

# Módulo con algunas funciones que permiten recalcular
# los coeficientes aerodinámicos en tiempo real durante
# la simulación del vuelo.

### Versión 1.0 (9/3/2025) ###


from rocketpy import Flight, LinearGenericSurface, Function
import numpy as np
import csv


def flight_initialization(data,rocket,environment):
    """Simula la fase del rail y proporciona las condiciones justo a la salida de este.
       En la versión actual de RocketPy no es posible iterar el vuelo en esta fase ya
       que cuando se introduce como 'initial_solution' una clase Flight automáticamente
       se considera que el cohete ha salido del rail.
       --------------------------------------------------------------------------------
       data: diccionario
       Diccionario con los datos del cohete.
       rocket: Rocket
       Objeto de la clase Rocket que se pretende simular.
       environment: Environment
       Objeto de la clase Environment en cuyas condiciones atmosféricas se pretende si-
       mular.
       --------------------------------------------------------------------------------
       results: Results
       Objeto de la clase Results que almacena los resultados del vuelo.
       flight: Flight
       Objeto de la clase Flight obtenido justo a la salida del rail.
    """

    # Se inicializa la clase de resultados
    results = Results()

    rail_test = Flight(
        rocket = rocket,
        environment = environment,
        rail_length = data['rail_longitud'],
        inclination = data['rail_inclinacion'],
        heading = data['rail_heading'],
        max_time = 5,  # El cohete debe salir del rail
    )

    flight = Flight(
        rocket = rocket,
        environment = environment,
        rail_length = data['rail_longitud'],
        inclination = data['rail_inclinacion'],
        heading = data['rail_heading'],
        max_time = rail_test.out_of_rail_time,
    )
    
    # Se almacenan los resultados del rail
    results.out_of_rail_store(flight)

    return results, flight


def flight_simulation(data,results,rocket,environment,flight,datcom,method="drag"):
    """Simulación de vuelo.
       ---------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes.
       results: Results
       Objeto de la clase Results en el que se quieren almacenar los datos del vuelo.
       rocket: Rocket
       Objeto de la clase Rocket que se pretende simular.
       environment: Environment
       Objeto de la clase Environment en cuyas condiciones atmosféricas se pretende si-
       mular el vuelo.
       flight: Flight
       Objeto de la clase Flight que contiene las condiciones iniciales del vuelo.
       datcom: DATCOM
       Objeto de la clase DATCOM que contiene las funciones necesarias para calcular los
       coeficientes aerodinámicos.
       method: str
       "drag" si se desea simular con el power on/off drag y las superficies analíticas
       de RocketPy. Este es el valor por defecto.
       "coefficients" se se desea simular directamente con los coeficientes aerodinámicos
       que proporciona DATCOM a través de la superficie genérica LinearGenericSurface.
       ----------------------------------------------------------------------------------
       results: Results
       Objeto de la clase Results que almacena los resultados del vuelo.
       flight: Flight
       Objeto de la clase Flight obtenido tras realizar la simulación de vuelo.
    """

    if method == "drag":

        # Cálculo del drag
        drag = datcom.run_datcom_for_drag(data,flight,environment)

        # Reset del drag
        rocket.power_off_drag = Function(
            drag,
            "Mach Number",
            "Drag Coefficient with Power Off",
            "linear",
            "constant",
        )
        rocket.power_on_drag = Function(
            drag,
            "Mach Number",
            "Drag Coefficient with Power On",
            "linear",
            "constant",
        )

    elif method == "coefficients":

        # Cálculo de los coeficientes
        coefficients = datcom.run_datcom_for_coefficients(data,flight,environment)

        # Se define una nueva LinearGenericSurface
        generic_surface = LinearGenericSurface(
            reference_area = np.pi * data['radius']**2,
            reference_length = 2 * data['radius'],
            coefficients = coefficients,
            name = 'Coeficientes DATCOM',
        )

        # Se eliminan las aero surfaces del rocket
        rocket.aerodynamic_surfaces.clear()
        rocket.add_surfaces(generic_surface,data['center_of_dry_mass'])

    # Duración de la simulación
    stop_time = flight.t + 1/data['frequency']
    
    # Simulación de vuelo
    flight = Flight(
        rocket = rocket,
        environment = environment,
        rail_length = data['rail_longitud'],
        inclination = data['rail_inclinacion'],
        heading = data['rail_heading'],
        initial_solution = flight,
        max_time = stop_time,
    )

    # Se almacenan los resultados
    results.store(flight)

    return results, flight


class Results:
    """Esta clase sirve para almacenar los datos del vuelo durante una simulación iterativa en
       la que se recalculan los coeficientes del cohete en tiempo real. Al final del bucle de
       la simulación únicamente se conservan los resultados de la última clase Flight ejecutada.
       Es por ello que esta clase es necesaria.
    """

    def __init__(self):
        """No es necesario definir ninguna propiedad para inicializar esta clase"""
        
        self.properties = [
            "t", "x", "y", "z", "vx", "vy", "vz", "ax", "ay", "az",
            "e0", "e1", "e2", "e3", "w1", "w2", "w3", "alpha1", "alpha2", "alpha3",
            "phi", "theta", "psi", "latitude", "longitude", "R1", "R2", "R3", "M1", "M2", "M3",
            "mach_number", "wind_velocity_x", "wind_velocity_y", "density", "pressure",
            "dynamic_viscosity", "speed_of_sound", "speed", "horizontal_speed", "acceleration",
            "path_angle", "attitude_vector_x", "attitude_vector_y", "attitude_vector_z",
            "attitude_angle", "lateral_attitude_angle", "aerodynamic_lift", "aerodynamic_drag",
            "aerodynamic_bending_moment", "aerodynamic_spin_moment",
            "rotational_energy", "translational_energy", "kinetic_energy",
            "potential_energy", "total_energy", "thrust_power", "drag_power",
            "attitude_frequency_response", "omega1_frequency_response",
            "omega2_frequency_response", "omega3_frequency_response",
            "static_margin", "stability_margin", "stream_velocity_x",
            "stream_velocity_y", "stream_velocity_z", "free_stream_speed",
            "reynolds_number", "dynamic_pressure", "total_pressure",
            "angle_of_attack", "partial_angle_of_attack", "angle_of_sideslip"
        ]
    
        for prop in self.properties:
            setattr(self, prop, [])


    def out_of_rail_store(self, flight):
        """Función que almacena los resultados del vuelo al final de la fase del rail.
           Guarda los datos en el instante inicial (t=0) y en el instante final, justo
           a la salida del rail.
           ---------------------------------------------------------------------------
           flight: Flight
           Objeto de la clase Flight cuyos datos se pretenden almacenar.
        """

        self.t.extend([0, flight.out_of_rail_time])
        
        for prop in self.properties[1:]:
            getattr(self, prop).extend([
                getattr(flight, prop)(0),
                getattr(flight, prop)(flight.out_of_rail_time)
            ])


    def store(self,flight):
        """Función genérica que almacena los resultados del vuelo. Guarda únicamente los
           datos al final del vuelo.
           -----------------------------------------------------------------------------
           flight: Flight
           Objeto de la clase Flight cuyos datos se pretenden almacenar.
        """

        self.t.append(flight.t)

        for prop in self.properties[1:]:
            getattr(self, prop).append(
                getattr(flight, prop)(flight.t)
            )


    def export(self,filename, *properties):
        """Exporta los datos almacenados en la clase Results a un archivo CSV.
           ------------------------------------------------------------------------------
           filename: str
           Nombre del archivo CSV donde se prentenden almacenar los resultados.
           properties: str
           Nombre de las propiedades (variables) que se pretenden almacenar. Se pueden in-
           troducir tantos como se desee. Se pueden revisar las variables disponibles en
           la propiedad de esta clase self.properties.
        """

        # Se comprueba que las propiedades pedidas existen
        for prop in properties:
            if not hasattr(self, prop):
                raise AttributeError(f"Variable '{prop}' no disponible")
        
        # Se adquieren los datos de las propiedades pedidas
        data = [getattr(self, prop) for prop in properties]
        
        # Se comprueba que todas las listas tengan la misma longitud
        length = len(data[0])
        if not all(len(lst) == length for lst in data):
            raise ValueError("Todas las propiedades deben tener la misma longitud (bug).")
        
        # Se escribe el CSV
        with open(filename, mode='w', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(properties)  # Encabezado con el nombre de las variables
            writer.writerows(zip(*data))  # Reesultados de la simulación

        print(f"Archivo '{filename}' creado con éxito.")