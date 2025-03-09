""" Archivo generado con datos del cohete Skybreaker """

'''TODAS LAS UNIDADES ESTÁN EN UNIDADES DEL SISTEMA INTERNACIONAL'''


'''AERODINÁMICA'''
Skybreaker_fins_number = 4 # Número de aletas
Skybreaker_fins_span = 0.115 # Longitud fuselaje - punta de ala
Skybreaker_fins_root_chord = 0.205 # Cuerda en la raíz
Skybreaker_fins_tip_chord = 0.123 # Cuerda en la punta
Skybreaker_fins_sweep_length = 0.0819 # Distancia en el eje longitudinal entre LE de cuerda en la raíz y el LE de la cuerda en la punta
Skybreaker_fins_position = 1.63 # Longitud desde la punta de la ojiva al inicio de las aletas
Skybreaker_fins_roll_position = [45,135,225,315] # Posición radial de las aletas

'''Para DATCOM (específicamente)'''
Skybreaker_fins_zupper = [0.01707317073170732, 0.024390243902439025] # Relación entre el espesor superior y la cuerda para cada span station
Skybreaker_fins_lmaxu = [0.17073170731707318, 0.24390243902439024] # Fracción de cuerda hasta alcanzar el espesor máximo para cada span station
Skybreaker_fins_lflatu = [0.705, 0.5] # Fracción de cuerda con espesor constante para cada span station
Skybreaker_nosecone_type_DATCOM = 'HAACK' # Tipo de ojiva para que lo pueda leer DATCOM (puede tener nombre distinto a rocketpy)

'''ESTRUCTURAS'''
Skybreaker_radius = 0.057 # Radio exterior
Skybreaker_no_motor_mass = 9.236 # Masa sin motor
Skybreaker_I11 = 5.8787 # Inercia (SIN MOTOR) en los ejes perpendiculares al eje de simetría respecto del cdg sin motor
Skybreaker_I33 = 0.03124 # Inercia (SIN MOTOR) en el eje de simetría respecto del cdg sin motor
Skybreaker_length = 1.859 # Longitud total
Skybreaker_no_motor_cdg = 0.945 # Centro de masas sin motor desde la punta de la ojiva
Skybreaker_nosecone_length = 0.327 # Longitud nose cone
Skybreaker_RHR = 400.0 # Índice de rugosidad del cohete
Skybreaker_center_of_dry_mass_position = 1.0637436520109065   # Centro de gravedad medido desde la punta de la ojiva cuando se ha consumido el propelente

'''AEROFRENO'''
Skybreaker_aerofreno_posicion = 1.309   # Distancia longitudinal desde la punta de la ojiva hasta el aerofreno [m]
Skybreaker_aerofreno_n_superficies = 4   # Número de aerofrenos [-]
Skybreaker_aerofreno_espesor = 0.005   # Espesor de la superficie del aerofreno [m]
Skybreaker_aerofreno_altura = 18.9/1000   # Longitud desde el fuselaje hasta la punta del aerofreno (100% extendido) [m]
Skybreaker_aerofreno_ancho = 50/1000   # Ancho del aerofreno (calcular para mantener superficie expuesta)(100% extendido) [m]

'''Ojiva'''
Skybreaker_ojiva_mass = 1.7741 # Masa
Skybreaker_ojiva_cdg = 0.2993746 # Centro de masas
Skybreaker_ojiva_length = 0.52 # Longitud
Skybreaker_ojiva_I11 = 1.65904706 # Inercia en los ejes perpendiculares al eje de simetría respecto del cdg
Skybreaker_ojiva_I33 = 0.76182018 # Inercia en el eje de simetría respecto del cdg
Skybreaker_nosecone_type = 'lvhaack' # Tipo de ojiva para rocketpy

'''PROPULSIÓN'''
Skybreaker_thrust_source = 'M1174.eng' # Archivo que contiene la curva de empuje del motor.
Skybreaker_start_burning_time = 0 # Encendido del motor
Skybreaker_motor_burnout_time = 4.86 # Tiempo de quemado
Skybreaker_motor_grain_number = 1 # Número de granos
Skybreaker_motor_grain_density = 1534.08 # Densidad de grano
Skybreaker_motor_I11 = 0.412925 # Inercia en los ejes perpendiculares al eje de simetría respecto del cdg
Skybreaker_motor_I33 = 0.0024438887 # Inercia en el eje de simetría respecto del cdg
Skybreaker_motor_outer_grain_radius = 0.043 # Radio exterior del grano
Skybreaker_motor_initial_inner_grain_radius = 0.0105 # Radio inicial interior del grano
Skybreaker_motor_height_grain = 0.4138 # Altura inicial de grano
Skybreaker_motor_cdg_grain = 0.3565698 # Centro de masa de los granos respecto a la salida de la tobera
Skybreaker_motor_dry_cdg = 0.3565698 # Centro de masa del motor sin propelente respecto a la salida de la tobera
Skybreaker_motor_nozzle_radius = 0.0444 # Radio de la tobera
Skybreaker_motor_throat_radius = 0.0205 # Radio de la garganta
Skybreaker_motor_grain_separation = 0.0 # Separación de grano (distancia entre 2 granos)
Skybreaker_motor_dry_mass = 2.5 # Masa sin propelente
Skybreaker_motor_total_impulse = 5745.07 # Impulso total 

'''RECUPERACIÓN'''
Skybreaker_main_cds = 0.08608776 # (CD_paracaídas · Area_ref)
Skybreaker_main_trigger = 1000 # Instante en el que se quiere que se abra
Skybreaker_main_lag = 1.0 # Tiempo que tarda el paracaídas en estar totalmente despelgado desde eyección
Skybreaker_drogue_cds = 0.05 # (CD_paracaídas · Area_ref)
Skybreaker_drogue_trigger = 'apogee'
Skybreaker_drogue_lag = 1.0 # Tiempo que tarda el paracaídas en estar totalmente despelgado desde eyección

'''LANZAMIENTO'''
latitud_base = 39.38895  # Latitud de la base de lanzamiento
longitud_base = -8.28837  # Longitud de la base de lanzamiento
elevacion_base = 100  # Elevación de la base de lanzamiento
rail_longitud = 7.5  # Longitud del rail de lanzamiento
rail_heading = 300  # Dirección hacia la que apunta el rail de lanzamiento.
rail_inclinacion = 85  # Ángulo de inclinación del rail con respecto el suelo.

'''ENVIRONMENT'''
magnitud_viento_0 = 3  # Magnitud del viento en la superficie
magnitud_viento_10 = 30  # Magnitud del viento a 10km de altitud
heading_viento = 50  # Dirección hacia la que sopla el viento

'''SIMULATION'''
frequency = 10  # Frecuencia con la que se quieren recalcular los coeficientes aerodinámicos


def load_rocket_data(delta):
    """Carga los datos del cohete SkyBreaker en forma de un diccionario.
       -----------------------------------------------------------------------------
       delta: float
       Altura normalizada de los aerofrenos. Esta es la distancia normalizada entre
       el fuselaje y la punta del aerofreno. Sus valores se encuentran entre 0 y 1.
       -----------------------------------------------------------------------------
       data: diccionario
       Proporciona un diccionario con los datos del cohete.
    """

    return {
        'fins_number':Skybreaker_fins_number,
        'fins_span':Skybreaker_fins_span,
        'fins_root_chord':Skybreaker_fins_root_chord,
        'fins_tip_chord':Skybreaker_fins_tip_chord,
        'fins_sweep_lenght':Skybreaker_fins_sweep_length,
        'fins_posicion':Skybreaker_fins_position,
        'fins_roll_position':Skybreaker_fins_roll_position,
        'fins_zupper':Skybreaker_fins_zupper,
        'fins_lmaxu':Skybreaker_fins_lmaxu,
        'fins_lflatu':Skybreaker_fins_lflatu,
        'tipo_nose_cone':Skybreaker_nosecone_type_DATCOM,
        'radius':Skybreaker_radius,
        'no_motor_mass':Skybreaker_no_motor_mass,
        'inercia_I11':Skybreaker_I11,
        'inercia_I33':Skybreaker_I33,
        'longitud':Skybreaker_length,
        'cdg_no_motor':Skybreaker_no_motor_cdg,
        'longitud_nose_cone':Skybreaker_nosecone_length,
        'RHR':Skybreaker_RHR,
        'center_of_dry_mass':Skybreaker_center_of_dry_mass_position,
        'aerofreno_posicion':Skybreaker_aerofreno_posicion,
        'aerofreno_n_superficies':Skybreaker_aerofreno_n_superficies,
        'aerofreno_espesor':Skybreaker_aerofreno_espesor,
        'aerofreno_altura':Skybreaker_aerofreno_altura *delta,
        'aerofreno_ancho':Skybreaker_aerofreno_ancho,
        'masa_ojiva':Skybreaker_ojiva_mass,
        'cdg_ojiva':Skybreaker_ojiva_cdg,
        'longitud_ojiva':Skybreaker_ojiva_length,
        'ojiva_I11':Skybreaker_ojiva_I11,
        'ojiva_I33':Skybreaker_ojiva_I33,
        'tipo_nose_cone_rocketpy':Skybreaker_nosecone_type,
        'thrust_source':Skybreaker_thrust_source,
        'start_burning_time':Skybreaker_start_burning_time,
        'burnout_time':Skybreaker_motor_burnout_time,
        'grain_number':Skybreaker_motor_grain_number,
        'grain_density':Skybreaker_motor_grain_density,
        'motor_I11':Skybreaker_motor_I11,
        'motor_I33':Skybreaker_motor_I33,
        'grain_outer_radius':Skybreaker_motor_outer_grain_radius,
        'grain_initial_inner_radius':Skybreaker_motor_initial_inner_grain_radius,
        'grain_height':Skybreaker_motor_height_grain,
        'cdg_grain':Skybreaker_motor_cdg_grain,
        'motor_dry_cdg':Skybreaker_motor_dry_cdg,
        'motor_nozzle_radius':Skybreaker_motor_nozzle_radius,
        'throat_radius':Skybreaker_motor_throat_radius,
        'grain_separation':Skybreaker_motor_grain_separation,
        'motor_dry_mass':Skybreaker_motor_dry_mass,
        'total_impulse':Skybreaker_motor_total_impulse,
        'main_cds':Skybreaker_main_cds,
        'main_trigger':Skybreaker_main_trigger,
        'main_lag':Skybreaker_main_lag,
        'drogue_cds':Skybreaker_drogue_cds,
        'drogue_trigger':Skybreaker_drogue_trigger,
        'drogue_lag':Skybreaker_drogue_lag,
        'latitud_base':latitud_base,
        'longitud_base':longitud_base,
        'elevacion_base':elevacion_base,
        'rail_longitud':rail_longitud,
        'rail_heading':rail_heading,
        'rail_inclinacion':rail_inclinacion,
        'viento_mag_0':magnitud_viento_0,
        'viento_mag_10':magnitud_viento_10,
        'viento_heading':heading_viento,
        'frequency':frequency,
    }
