from rocketpy import Environment, SolidMotor, Rocket, Flight, NoseCone, TrapezoidalFins, GenericSurface, Function
from rocketpy.mathutils.vector_matrix import Vector,Matrix
import datetime
import numpy as np
import sys
sys.path.append(r'automatizacion_rocketpy')
import datos_skybreaker as data
import datcom


def cargar_skybreaker(delta):
    """Carga los datos del cohete SkyBreaker a partir del módulo con los parámetros.
       -----------------------------------------------------------------------------
       delta: float
       Altura normalizada de los aerofrenos. Esta es la distancia normalizada entre
       el fuselaje y la punta del aerofreno. Sus valores se encuentran entre 0 y 1.
       -----------------------------------------------------------------------------
       data: diccionario
       Proporciona un diccionario con los datos del cohete.
    """

    return {
        'fins_number':data.Skybreaker_fins_number,
        'fins_span':data.Skybreaker_fins_span,
        'fins_root_chord':data.Skybreaker_fins_root_chord,
        'fins_tip_chord':data.Skybreaker_fins_tip_chord,
        'fins_sweep_lenght':data.Skybreaker_fins_sweep_length,
        'fins_posicion':data.Skybreaker_fins_position,
        'fins_roll_position':data.Skybreaker_fins_roll_position,
        'fins_zupper':data.Skybreaker_fins_zupper,
        'fins_lmaxu':data.Skybreaker_fins_lmaxu,
        'fins_lflatu':data.Skybreaker_fins_lflatu,
        'tipo_nose_cone':data.Skybreaker_nosecone_type_DATCOM,
        'radius':data.Skybreaker_radius,
        'no_motor_mass':data.Skybreaker_no_motor_mass,
        'inercia_I11':data.Skybreaker_I11,
        'inercia_I33':data.Skybreaker_I33,
        'longitud':data.Skybreaker_length,
        'cdg_no_motor':data.Skybreaker_no_motor_cdg,
        'longitud_nose_cone':data.Skybreaker_nosecone_length,
        'RHR':data.Skybreaker_RHR,
        'center_of_dry_mass':data.Skybreaker_center_of_dry_mass_position,
        'aerofreno_posicion':data.Skybreaker_aerofreno_posicion,
        'aerofreno_n_superficies':data.Skybreaker_aerofreno_n_superficies,
        'aerofreno_espesor':data.Skybreaker_aerofreno_espesor,
        'aerofreno_altura':data.Skybreaker_aerofreno_altura *delta,
        'aerofreno_ancho':data.Skybreaker_aerofreno_ancho,
        'masa_ojiva':data.Skybreaker_ojiva_mass,
        'cdg_ojiva':data.Skybreaker_ojiva_cdg,
        'longitud_ojiva':data.Skybreaker_ojiva_length,
        'ojiva_I11':data.Skybreaker_ojiva_I11,
        'ojiva_I33':data.Skybreaker_ojiva_I33,
        'tipo_nose_cone_rocketpy':data.Skybreaker_nosecone_type,
        'thrust_source':data.Skybreaker_thrust_source,
        'burn_time':data.Skybreaker_motor_burn_time,
        'grain_number':data.Skybreaker_motor_grain_number,
        'grain_density':data.Skybreaker_motor_grain_density,
        'motor_I11':data.Skybreaker_motor_I11,
        'motor_I33':data.Skybreaker_motor_I33,
        'grain_outer_radius':data.Skybreaker_motor_outer_grain_radius,
        'grain_initial_inner_radius':data.Skybreaker_motor_initial_inner_grain_radius,
        'grain_height':data.Skybreaker_motor_height_grain,
        'cdg_grain':data.Skybreaker_motor_cdg_grain,
        'motor_dry_cdg':data.Skybreaker_motor_dry_cdg,
        'motor_nozzle_radius':data.Skybreaker_motor_nozzle_radius,
        'throat_radius':data.Skybreaker_motor_throat_radius,
        'grain_separation':data.Skybreaker_motor_grain_separation,
        'motor_dry_mass':data.Skybreaker_motor_dry_mass,
        'total_impulse':data.Skybreaker_motor_total_impulse,
        'main_cds':data.Skybreaker_main_cds,
        'main_trigger':data.Skybreaker_main_trigger,
        'main_lag':data.Skybreaker_main_lag,
        'drogue_cds':data.Skybreaker_drogue_cds,
        'drogue_trigger':data.Skybreaker_drogue_trigger,
        'drogue_lag':data.Skybreaker_drogue_lag,
        'latitud_base':data.latitud_base,
        'longitud_base':data.longitud_base,
        'elevacion_base':data.elevacion_base,
        'rail_longitud':data.rail_longitud,
        'rail_heading':data.rail_heading,
        'rail_inclinacion':data.rail_inclinacion
    }


def calcula_drag_datcom(data,u,speed_of_sound,wind_vector,p_y_T):
    """Calcula el coeficiente de fuerza axial que se corresponde con el drag power
       on/off de RocketPy.
       ------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quiere obtener el coeficiente. Este input se
       obtiene a partir de la función cargar_skybreaker.
       u: lista
       Vector de estado con los resultados de la simulación de RocketPy.
       speed_of_sound: float
       Valor de la velocidad del sonido en este instante del vuelo.
       wind_vector: lista
       Lista con las componentes de la velocidad del viento: [wx,wy,0].
       p_y_T: tupla
       Tupla con el valor de la presión y temperatura estáticas atmosféricas.
       -------------------------------------------------------------------------------
       drag: float
       Devuelve el CA del cohete.
    """

    # Ubicación de la carpeta donde se encuentra datcom.exe
    ubicacion_datcom = 'D:\\DATCOM'

    # Se obtiene el vector velocidad del flujo en ejes cuerpo
    v = Vector([u[4],u[5],u[6]])   # Velocidad del CDM en ejes locales  
    e = [u[7],u[8],u[9],u[10]]   # Vector de cuaterniones
    K = Matrix.transformation(e)   # Matriz de rotación Locales - Body
    Kt = K.transpose
    v_B = Kt @ v   # Velocidad del CDM en ejes cuerpo
    wind_vector_B = Kt @ Vector(wind_vector)   # Velocidad del viento en ejes cuerpo
    stream_velocity_vector = v_B - wind_vector_B   # Velocidad del aire relativo al cohete

    # Variables independientes para las que se pretenden calcular los coeficientes
    mach = [abs(stream_velocity_vector) / speed_of_sound]
    alpha = np.arctan2(stream_velocity_vector[1], stream_velocity_vector[2]) * 180/np.pi   # En deg
    beta = np.arctan2(stream_velocity_vector[0], stream_velocity_vector[2]) * 180/np.pi   # En deg

    # Motor on/off
    t = u[0]
    if t < data['burn_time']:
        motor_on = True
    else:
        motor_on = False

    # Ejecución de DATCOM
    texto_for005 = datcom.for005_mono_etapa(data,mach,[alpha],[beta],p_y_T=p_y_T,motor_on=motor_on,aerofreno=True)
    datcom.escribir_for005(ubicacion_datcom,texto_for005)
    datcom.ejecutar_datcom(ubicacion_datcom)
    for004_info = datcom.leer_for004(ubicacion_datcom + '\\for004.dat')
    drag = datcom.definir_drag_rocketpy(for004_info)

    return drag


def calcula_coeficientes_datcom(data,u,speed_of_sound,wind_vector,p_y_T):
    """Calcula los coeficientes de DATCOM del cohete en el formato de RocketPy a par-
       tir de las condiciones de vuelo actuales. Proporciona un único valor para cada
       uno de los 6 coeficientes.
       ------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       u: lista
       Vector de estado con los resultados de la simulación de RocketPy.
       speed_of_sound: float
       Valor de la velocidad del sonido en este instante del vuelo.
       wind_vector: lista
       Lista con las componentes de la velocidad del viento: [wx,wy,0].
       p_y_T: tupla
       Tupla con el valor de la presión y temperatura estáticas atmosféricas.
       -------------------------------------------------------------------------------
       coeficientes: diccionario
       Devuelve un diccionario con los 6 coeficientes del cohete en el formato de Ro-
       cketPy. Tiene el siguiente formato:
       coeficientes = {
            "cL": 0,
            "cQ": 0,
            "cD": 0,
            "cm": 0,
            "cn": 0,
            "cl": 0,
       }
    """

    # Ubicación de la carpeta donde se encuentra datcom.exe
    ubicacion_datcom = 'D:\\DATCOM'

    # Se obtiene el vector velocidad del flujo en ejes cuerpo
    v = Vector([u[4],u[5],u[6]])   # Velocidad del CDM en ejes locales  
    e = [u[7],u[8],u[9],u[10]]   # Vector de cuaterniones
    K = Matrix.transformation(e)   # Matriz de rotación Locales - Body
    Kt = K.transpose
    v_B = Kt @ v   # Velocidad del CDM en ejes cuerpo
    wind_vector_B = Kt @ Vector(wind_vector)   # Velocidad del viento en ejes cuerpo
    stream_velocity_vector = v_B - wind_vector_B   # Velocidad del aire relativo al cohete

    # Variables independientes para las que se pretenden calcular los coeficientes
    mach = [abs(stream_velocity_vector) / speed_of_sound]
    alpha = np.arctan2(stream_velocity_vector[1], stream_velocity_vector[2]) * 180/np.pi   # En deg
    beta = np.arctan2(stream_velocity_vector[0], stream_velocity_vector[2]) * 180/np.pi   # En deg

    # Motor on/off
    t = u[0]
    if t < data['burn_time']:
        motor_on = True
    else:
        motor_on = False

    # Ejecución de DATCOM
    texto_for005 = datcom.for005_mono_etapa(data,mach,[alpha],[beta],p_y_T=p_y_T,motor_on=motor_on,aerofreno=True)
    datcom.escribir_for005(ubicacion_datcom,texto_for005)
    datcom.ejecutar_datcom(ubicacion_datcom)
    for004_info = datcom.leer_for004(ubicacion_datcom + '\\for004.dat')
    coeficientes = datcom.definir_diccionario_coeficientes_rocketpy(for004_info,alpha,beta)

    return coeficientes


def definicion_atmosfera(data,mag_0,mag_10,head):
    """Define las condiciones atmosféricas. Clase Environment de RocketPy.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       mag_0: float
       Magnitud del viento en la superficie en m/s.
       mag_10: float
       Magnitud del viento a 10km de altitud en m/s.
       head: float
       Dirección hacia la que sopla el viento en º.
       --------------------------------------------------------------------------------
       env: Environment
       Objeto de la clase Environment con la configuración pedida.
    """

    # Perfil lineal del viento
    m = (mag_10 - mag_0) / 10000

    perfil_viento_u = list()
    perfil_viento_v = list()
    for h in list(range(0,10000 + 1,30)):
        magnitud_viento = m*h + mag_0  # Ecuación de la recta
        viento_u = magnitud_viento * np.sin( head * np.pi/180 ) # Componente dirección Este
        viento_v = magnitud_viento * np.cos( head * np.pi/180 ) # COmponente dirección Norte
        perfil_viento_u.append([h,viento_u])
        perfil_viento_v.append([h,viento_v])

    # Se define la clase Environment
    env = Environment(
        latitude = data['latitud_base'],
        longitude = data['longitud_base'],
        elevation = data['elevacion_base']
    )

    # Fecha del lanzamiento
    tomorrow = datetime.date.today() + datetime.timedelta(days=1)
    env.set_date(
        (tomorrow.year, tomorrow.month, tomorrow.day, 12)
    )

    # Se carga el perfil de viento
    env.set_atmospheric_model(
        type="custom_atmosphere",
        wind_u = perfil_viento_u,
        wind_v = perfil_viento_v,
    )
    env.max_expected_height = 4000

    return env


def definicion_motor(data):
    """Definición del motor a partir de los datos del cohete.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       -------------------------------------------------------------------------------
       motor: SolidMotor
       Objeto de la clase SolidMotor con la configuración pedida.
    """

    # Definición del motor
    M1174 = SolidMotor(
        thrust_source = data['thrust_source'],
        dry_mass = data['motor_dry_mass'],
        dry_inertia = (
            data['motor_I11'],
            data['motor_I11'],
            data['motor_I33'],
        ),
        nozzle_radius = data['motor_nozzle_radius'],
        grain_number = data['grain_number'],
        grain_density = data['grain_density'],
        grain_outer_radius = data['grain_outer_radius'],
        grain_initial_inner_radius = data['grain_initial_inner_radius'],
        grain_initial_height = data['grain_height'],
        grain_separation = data['grain_separation'],
        grains_center_of_mass_position = data['cdg_grain'],
        center_of_dry_mass_position = data['motor_dry_cdg'],
        nozzle_position = 0,
        throat_radius = data['throat_radius'],
        burn_time = data['burn_time'],
        coordinate_system_orientation = "nozzle_to_combustion_chamber",
    )

    return M1174


def definicion_nose_cone(data):
    """Definición del nose cone a partir de los datos del cohete.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       -------------------------------------------------------------------------------
       nose_cone: NoseCone
       Se obtiene un objeto de la clase NoseCone con las características especificadas.
    """

    # Nose cone
    nose_cone = NoseCone(
        length = data['longitud_nose_cone'],
        kind = data['tipo_nose_cone_rocketpy'],
        base_radius = data['radius'],
        bluffness = None,
        rocket_radius = data['radius'],
        name = "Nose Cone",
    )

    return nose_cone

def definicion_aletas(data):
    """Definición de las aletas a partir de los datos del cohete.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       -------------------------------------------------------------------------------
       aletas: TrapezoidalFins
       Se obtiene un objeto de la clase TrapezoidalFins con las características especi-
       ficadas.
    """

    # Aletas
    aletas = TrapezoidalFins(
        n = data['fins_number'],
        root_chord = data['fins_root_chord'],
        tip_chord = data['fins_tip_chord'],
        span = data['fins_span'],
        rocket_radius = data['radius'],
        sweep_length = data['fins_sweep_lenght'],
        name = "Aletas",     
    )

    return aletas


def definicion_coeficientes(data,coeficientes):
    """Introducción en RocketPy de los coeficientes aerodinámicos.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       coeficientes: diccionario
       Coeficientes aerodinámicos del cohete. Debe tener la misma estructura que el
       output de calcula_coeficientes_datcom.
       -------------------------------------------------------------------------------
       generic_surface: GenericSurface
       Se obtiene un objeto de la clase GenericSurface que almacena los coeficientes
       del cohete.
    """

    # Valores de referencia
    Sref = np.pi * data['radius']**2
    Lref = 2 * data['radius']

    # Tratamiento de los coeficientes
    coeff = {
        'cL':Function(
            lambda alpha, beta, mach, reynolds, pitch_rate, yaw_rate, roll_rate: coeficientes['cL'],
            [
                'alpha',
                'beta',
                'mach',
                'reynolds',
                'pitch_rate',
                'yaw_rate',
                'roll_rate',
            ],
        ),
        'cQ':Function(
            lambda alpha, beta, mach, reynolds, pitch_rate, yaw_rate, roll_rate: coeficientes['cQ'],
            [
                'alpha',
                'beta',
                'mach',
                'reynolds',
                'pitch_rate',
                'yaw_rate',
                'roll_rate',
            ],
        ),
        'cD':Function(
            lambda alpha, beta, mach, reynolds, pitch_rate, yaw_rate, roll_rate: coeficientes['cD'],
            [
                'alpha',
                'beta',
                'mach',
                'reynolds',
                'pitch_rate',
                'yaw_rate',
                'roll_rate',
            ],
        ),
        'cm':Function(
            lambda alpha, beta, mach, reynolds, pitch_rate, yaw_rate, roll_rate: coeficientes['cm'],
            [
                'alpha',
                'beta',
                'mach',
                'reynolds',
                'pitch_rate',
                'yaw_rate',
                'roll_rate',
            ],
        ),
        'cn':Function(
            lambda alpha, beta, mach, reynolds, pitch_rate, yaw_rate, roll_rate: coeficientes['cn'],
            [
                'alpha',
                'beta',
                'mach',
                'reynolds',
                'pitch_rate',
                'yaw_rate',
                'roll_rate',
            ],
        ),
        'cl':Function(
            lambda alpha, beta, mach, reynolds, pitch_rate, yaw_rate, roll_rate: coeficientes['cl'],
            [
                'alpha',
                'beta',
                'mach',
                'reynolds',
                'pitch_rate',
                'yaw_rate',
                'roll_rate',
            ],
        ),
    }

    # Generic Surface
    generic_surface = GenericSurface(
        reference_area = Sref,
        reference_length = Lref,
        coefficients = coeff,
        name = 'Coeficientes DATCOM',
    )

    return generic_surface


def definicion_cohete_superficies(data,motor,nose_cone,aletas,drag):
    """Definición del cohete a partir de las superficies aerodinámicas.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       motor: SolidMotor
       Output de definicion_motor.
       nose_cone: NoseCone
       Output de definicion_nose_cone.
       aletas: TrapezoidalFins
       Output de definicion_aletas.
       drag: float
       Output de calcula_drag_datcom.
       -------------------------------------------------------------------------------
       rocket: Rocket
       Se obtiene un objeto de la clase Rocket con las características especificadas.
    """

    rocket = Rocket(
        radius = data['radius'],
        mass = data['no_motor_mass'],
        inertia = (
            data['inercia_I11'],
            data['inercia_I11'],
            data['inercia_I33'],
        ),
        power_off_drag = drag,
        power_on_drag = drag,
        center_of_mass_without_motor = data['cdg_no_motor'],
        coordinate_system_orientation = "nose_to_tail",
    )
    rocket.add_motor(motor,data['longitud'])
    rocket.add_surfaces(nose_cone,0)
    rocket.add_surfaces(aletas,data['fins_posicion'])

    return rocket


def definicion_cohete_coeficientes(data,motor,generic_surface):
    """Definición del cohete a partir de los coeficientes aerodinámicos.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       motor: SolidMotor
       Output de definicion_motor.
       generic_surface: GenericSurface
       Output de definicion_coeficientes
       -------------------------------------------------------------------------------
       rocket: Rocket
       Se obtiene un objeto de la clase Rocket con las características especificadas.
    """

    rocket = Rocket(
        radius = data['radius'],
        mass = data['no_motor_mass'],
        inertia = (
            data['inercia_I11'],
            data['inercia_I11'],
            data['inercia_I33'],
        ),
        power_off_drag = 0,
        power_on_drag = 0,
        center_of_mass_without_motor = data['cdg_no_motor'],
        coordinate_system_orientation = "nose_to_tail",
    )
    rocket.add_motor(motor,data['longitud'])
    rocket.add_surfaces(generic_surface,data['center_of_dry_mass'])

    return rocket


def simulacion_vuelo(data,u_1,frecuencia,rocket,environment):
    """Simulación de vuelo.
       -------------------------------------------------------------------------------
       data: diccionario
       Datos del cohete para el que se quieren obtener los coeficientes. Este input se
       obtiene a partir de la función cargar_skybreaker.
       u_1: lista
       Solución inicial que corresponde con los resultados de la simulación de vuelo 
       del paso temporal anterior.
       frecuencia: float
       Frecuencia en Hz con la que se adquiere la telemetría de vuelo.
       rocket: Rocket
       Cohete que se pretende simular. Es el output de definicion_cohete.
       environment: Environment
       Condiciones atmosféricas. Es el output de definicion_atmosfera.
       --------------------------------------------------------------------------------
       u: lista
       Vector de estado con los resultados de la simulación.
    """

    # Simular hasta
    stop_time = u_1[0] + 1/frecuencia

    # Simulación de vuelo
    flight = Flight(
        rocket = rocket,
        environment = environment,
        rail_length = data['rail_longitud'],
        inclination = data['rail_inclinacion'],
        heading = data['rail_heading'],
        initial_solution = u_1,
        max_time = stop_time,
        name = "Skybreaker's Flight",
    )

    return [
        flight.t,
        flight.x(flight.t), flight.y(flight.t), flight.z(flight.t),
        flight.vx(flight.t), flight.vy(flight.t), flight.vz(flight.t),
        flight.e0(flight.t), flight.e1(flight.t), flight.e2(flight.t), flight.e3(flight.t),
        flight.w1(flight.t), flight.w2(flight.t), flight.w3(flight.t),
    ]