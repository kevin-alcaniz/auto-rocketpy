###### FARADAY ROCKETRY UPV ######

# Módulo para obtener automáticamente los coeficientes
# aerodinámicos utilizando DATCOM.

###   Versión 2.0  (5/3/2025)   ###

import numpy as np
import subprocess
import csv


class LongitudExcedidaError(Exception):
    """ Error que salta cuando una línea de algún archivo supera la longitud permitida. """
    pass


def for005_doble_etapa(datos_cohete,machs,alphas,betas,altitudes=False,p_y_T=False,motor_on=False):
    """ Esta función sirve para obtener el texto del for005.dat para cohetes doble etapa.
        Es precisa únicamente cuando el Mach de vuelo no alcanza velocidades supersónicas.
        Unidades del Sistema Internacional a no ser que se diga lo contrario.
        ---------------------------------------------------------------------------------
        datos_cohete: diccionario
        Aquí se deben introducir todos los datos del cohete necesarios para obtener los
        coeficientes.
        Datos que se deben introducir: {
            'booster_radius':datos_cohete.booster_radius,
            'cohete_center_of_dry_mass':datos_cohete.origin_center_of_dry_mass,
            'RHR':datos_cohete.RHR,
            'contorno_cohete_x':datos_cohete.contorno_cohete_x,
            'contorno_cohete_r':datos_cohete.contorno_cohete_r,
            'nc_x':datos_cohete.nc_x,
            'as_span':datos_cohete.as_span,
            'as_root_chord':datos_cohete.as_root_chord,
            'as_tip_chord':datos_cohete.as_tip_chord,
            'as_posicion':datos_cohete.as_posicion,
            'as_sweep_lenght':datos_cohete.as_sweep_lenght,
            'as_n':datos_cohete.as_n,
            'as_roll':datos_cohete.as_roll,
            'as_zupper':datos_cohete.as_zupper,
            'as_lmaxu':datos_cohete.as_lmaxu,
            'as_lflatu':datos_cohete.as_lflatu,
            'ab_span':datos_cohete.ab_span,
            'ab_root_chord':datos_cohete.ab_root_chord,
            'ab_tip_chord':datos_cohete.ab_tip_chord,
            'ab_xle':datos_cohete.ab_xle,
            'ab_n':datos_cohete.ab_n,
            'ab_roll':datos_cohete.ab_roll,
            'ab_zupper':datos_cohete.ab_zupper,
            'ab_lmaxu':datos_cohete.ab_lmaxu,
            'ab_lflatu':datos_cohete.ab_lflatu,
            'mb_nozzle_radius':datos_cohete.mb_nozzle_radius    
        }
        machs: lista
        Lista de números de Mach para los que se quieren obtener coeficientes.
        alphas: lista
        Lista de ángulos de ataque para los que se quieren obtener coeficientes. En deg.
        betas: lista
        Lista de ángulos de derrape para los que se quieren obtener coeficientes. En deg.
        altitud: lista (opcional)
        Lista de las altitudes de vuelo correspondientes a cada número de Mach. Se utiliza
        la US Standard Atmosphere de 1962. Ideal cuando no se tiene un pronóstico de la at-
        mósfera.
        p_y_T: lista de listas (opcional)
        Lista de la presión y temperatura de vuelo correspondientes a cada número de
        Mach. Ideal cuando se tiene un pronóstico de la atmósfera. 
        Ej.: [[p1,T1],[p2,T2],[p3,T3]]
        motor_on: buleano (opcional)
        False si se desean los coeficientes para el motor apagado.
        True para obtener los coeficientes con el motor encendido.
        Se deberá adjuntar el radio de la tobera en datos_cohete en caso verdadero.
        ---------------------------------------------------------------------------------
        return texto
        Devuelve una string con el texto que se debe introducir posteriormente en la fun-
        ción escribir_for005.
    """
    
    caso_base = [
        '$FLTCON NALPHA=',
        str(float(len(alphas))),
        ',$\n',
        '$FLTCON ALPHA=',
        ','.join(str(float(alpha)) for alpha in alphas),  # Convertir ángulos de ataque en string,
        ',$\n',
        '$FLTCON NMACH=',
        str(float(len(machs))),
        ',$\n',
        '$FLTCON MACH=',
        ','.join(str(float(mach)) for mach in machs),  # Convertir machs en string
        ',$\n',
        '$FLTCON BETA=',
        str(float(betas[0])),
        ',$\n',
        '$REFQ SREF=',
        str(float(round(np.pi * datos_cohete['booster_radius']**2,4))),
        ',LREF=',
        str(float(2*datos_cohete['booster_radius'])),
        ',LATREF=',
        str(float(2*datos_cohete['booster_radius'])),
        ',XCG=',
        str(float(datos_cohete['cohete_center_of_dry_mass'])),
        ',$\n$REFQ BLAYER=TURB,',
        'RHR=',
        str(float(datos_cohete['RHR'])),
        ',$\n',
        '$AXIBOD NX=',
        str(float(len(datos_cohete['contorno_cohete_x']))),
        ',$\n',
        '$AXIBOD X(1)=',
        ','.join(str(float(round(x,3))) for x in datos_cohete['contorno_cohete_x'][0:10]),
        ',$\n',
        '$AXIBOD X(11)=',
        ','.join(str(float(round(x,3))) for x in datos_cohete['contorno_cohete_x'][10:20]),
        ',$\n',
        '$AXIBOD X(21)=',
        ','.join(str(float(round(x,3))) for x in datos_cohete['contorno_cohete_x'][20:30]),
        ',$\n',
        '$AXIBOD X(31)=',
        ','.join(str(float(round(x,3))) for x in datos_cohete['contorno_cohete_x'][30:40]),
        ',$\n',
        '$AXIBOD X(41)=',
        ','.join(str(float(round(x,3))) for x in datos_cohete['contorno_cohete_x'][40:50]),
        ',$\n',
        '$AXIBOD R(1)=',
        ','.join(str(float(round(r,3))) for r in datos_cohete['contorno_cohete_r'][0:10]),
        ',$\n',
        '$AXIBOD R(11)=',
        ','.join(str(float(round(r,3))) for r in datos_cohete['contorno_cohete_r'][10:20]),
        ',$\n',
        '$AXIBOD R(21)=',
        ','.join(str(float(round(r,3))) for r in datos_cohete['contorno_cohete_r'][20:30]),
        ',$\n',
        '$AXIBOD R(31)=',
        ','.join(str(float(round(r,3))) for r in datos_cohete['contorno_cohete_r'][30:40]),
        ',$\n',
        '$AXIBOD R(41)=',
        ','.join(str(float(round(r,3))) for r in datos_cohete['contorno_cohete_r'][40:50]),
        ',$\n',
        '$AXIBOD DISCON=',
        ','.join(str(float(i + 1)) for i in list(range(len(datos_cohete['nc_x']),len(datos_cohete['contorno_cohete_x']),1))),
        ',$\n',
        '$AXIBOD BNOSE=0.,TRUNC=.FALSE.,$\n',
        '$AXIBOD DEXIT=0.,$\n', # Motor apagado. Diámetro de la tobera. 
        '$FINSET1 SECTYP=HEX,$\n',
        '$FINSET1 SSPAN=',
        ','.join(str(float(i)) for i in ([0.0] + [datos_cohete['as_span']])),
        ',CHORD=',
        ','.join(str(float(i)) for i in ([datos_cohete['as_root_chord']] + [datos_cohete['as_tip_chord']])),
        ',XLE=',
        ','.join(str(float(round(i,4))) for i in ([datos_cohete['as_posicion']] + [datos_cohete['as_posicion'] + datos_cohete['as_sweep_lenght']])),
        ',$\n',
        '$FINSET1 NPANEL=',
        str(float(datos_cohete['as_n'])),
        ',PHIF=',
        ','.join(str(float(i)) for i in datos_cohete['as_roll']),
        ',$\n',
        '$FINSET1 ZUPPER=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['as_zupper']),
        ',LMAXU=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['as_lmaxu']),
        ',LFLATU=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['as_lflatu']),
        ',$\n',
        '$FINSET2 SECTYP=HEX,$\n',
        '$FINSET2 SSPAN=',
        ','.join(str(float(i)) for i in ([0.0] + [datos_cohete['ab_span']])),
        ',CHORD=',
        ','.join(str(float(i)) for i in ([datos_cohete['ab_root_chord']] + [datos_cohete['ab_tip_chord']])),
        ',XLE=',
        ','.join(str(float(round(i,4))) for i in datos_cohete['ab_xle']),
        ',$\n',
        '$FINSET2 NPANEL=',
        str(float(datos_cohete['ab_n'])),
        ',PHIF=',
        ','.join(str(float(i)) for i in datos_cohete['ab_roll']),
        ',$\n',
        '$FINSET2 ZUPPER=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['ab_zupper']),
        ',LMAXU=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['ab_lmaxu']),
        ',LFLATU=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['ab_lflatu']),
        ',$\n',
    ]

    # Método de cálculo del número de Reynolds
    if isinstance(altitudes,list) is True and p_y_T is False:
        metodo_reynolds = [
            '$FLTCON ALT=',
            ','.join(str(float(altitud)) for altitud in altitudes),
            ',$\n',
        ]
    elif isinstance(p_y_T,list) is True and altitudes is False:
        metodo_reynolds = [
            '$FLTCON PINF=',
            ','.join(str(float(cond[0])) for cond in p_y_T),
            ',$\n',
            '$FLTCON TINF=',
            ','.join(str(float(cond[1])) for cond in p_y_T),
            ',$\n',
        ]
    elif isinstance(p_y_T,list) is True and isinstance(altitudes,list) is True:
        metodo_reynolds = [
            '$FLTCON PINF=',
            ','.join(str(float(cond[0])) for cond in p_y_T),
            ',$\n',
            '$FLTCON TINF=',
            ','.join(str(float(cond[1])) for cond in p_y_T),
            ',$\n',
        ]
    else:
        metodo_reynolds = ['$FLTCON ALT=0.,$\n']
    caso_base += metodo_reynolds

    # Motor on/off
    if motor_on is True:
        metodo_motor_on = [
            '$AXIBOD DEXIT=',
            str(float(2*datos_cohete['mb_nozzle_radius'])),
            ',$\n',
        ]
        caso_base += metodo_motor_on

    # Tarjetas de control
    control_cards = [
        'CASEID BETA=',
        str(betas[0]),
        '\nDIM M\n',
        'DAMP\n',
        'DERIV RAD\n',
        'WRITE SB12,1,120\n',
        'WRITE DB12,1,360\n',
        'SAVE\n',
        'NEXT CASE\n',
    ]
    caso_base += control_cards

    # Variación de beta
    for beta in betas[1:]:
        caso_beta = [
            'CASEID BETA=',
            str(beta),
            '\n',
            '$FLTCON BETA=',
            str(float(beta)),
            ',$\n',
            'DAMP\n',
            'SAVE\n'
            'NEXT CASE\n',
        ]
        caso_base += caso_beta

    print('¡ATENCIÓN!: Resultados precisos únicamente para M < 1')

    return caso_base


def for005_mono_etapa(datos_cohete,machs,alphas,betas,altitudes=False,p_y_T=False,motor_on=False,truncado=False,aerofreno=False):
    """ Esta función sirve para obtener el texto del for005.dat para cohetes monoetapa.
        Unidades del Sistema Internacional a no ser que se diga lo contrario.
        ---------------------------------------------------------------------------------
        datos_cohete: diccionario
        Aquí se deben introducir todos los datos del cohete necesarios para obtener los
        coeficientes.
        Datos que se deben introducir: {
            'radius':datos_cohete.sustainer_radius_inferior,
            'center_of_dry_mass':datos_cohete.sustainer_center_of_dry_mass,
            'RHR':datos_cohete.RHR,
            'tipo_nose_cone':datos_cohete.nc_tipo,
            'longitud_nose_cone':datos_cohete.nc_longitud,
            'longitud':datos_cohete.sustainer_longitud,
            'fins_span':datos_cohete.as_span,
            'fins_root_chord':datos_cohete.as_root_chord,
            'fins_tip_chord':datos_cohete.as_tip_chord,
            'fins_posicion':datos_cohete.as_posicion,
            'fins_sweep_lenght':datos_cohete.as_sweep_lenght,
            'fins_number':datos_cohete.as_n,
            'fins_roll_position':datos_cohete.as_roll,
            'fins_zupper':datos_cohete.as_zupper,
            'fins_lmaxu':datos_cohete.as_lmaxu,
            'fins_lflatu':datos_cohete.as_lflatu,
            'radio_en_truncamiento':datos_cohete.interstage_radius_superior,
            'motor_nozzle_radius':datos_cohete.ms_nozzle_radius,   
        }
        machs: lista
        Lista de números de Mach para los que se quieren obtener coeficientes.
        alphas: lista
        Lista de ángulos de ataque para los que se quieren obtener coeficientes. En deg.
        betas: lista
        Lista de ángulos de derrape para los que se quieren obtener coeficientes. En deg.
        altitud: lista (opcional)
        Lista de las altitudes de vuelo correspondientes a cada número de Mach. Se utiliza
        la US Standard Atmosphere de 1962. Ideal cuando no se tiene un pronóstico de la at-
        mósfera.
        p_y_T: lista de listas (opcional)
        Lista de la presión y temperatura de vuelo correspondientes a cada número de
        Mach. Ideal cuando se tiene un pronóstico de la atmósfera. 
        Ej.: [[p1,T1],[p2,T2],[p3,T3)]]
        motor_on: buleano (opcional)
        False si se desean los coeficientes para el motor apagado.
        True para obtener los coeficientes con el motor encendido.
        Se deberá adjuntar el radio de la tobera en datos_cohete en caso verdadero.
        truncado: buleano (opcional)
        False si el nose cone no está truncado.
        True si el nose cone está truncado.
        Se deberá adjuntar el radio en el truncamiento en datos_cohete en caso verdadero.
        aerofreno: buleano (opcional)
        False si no hay aerofrenos desplegados.
        True si hay aerofrenos desplegados.
        Se deberá adjuntar información acerca de la geometría del aerofreno como, por e-
        jemplo, posición longitudinal, número de superficies, espesor, anchura y altura.
        ---------------------------------------------------------------------------------
        return texto
        Devuelve una string con el texto que se debe introducir posteriormente en la fun-
        ción escribir_for005.
    """

    caso_base = [
        '$FLTCON NALPHA=',
        str(float(len(alphas))),
        ',$\n',
        '$FLTCON ALPHA=',
        ','.join(str(float(alpha)) for alpha in alphas),  # Convertir ángulos de ataque en string,
        ',$\n',
        '$FLTCON NMACH=',
        str(float(len(machs))),
        ',$\n',
        '$FLTCON MACH=',
        ','.join(str(float(mach)) for mach in machs),  # Convertir machs en string
        ',$\n',
        '$FLTCON BETA=',
        str(float(betas[0])),
        ',$\n',
        '$REFQ SREF=',
        str(float(round(np.pi * datos_cohete['radius']**2,5))),
        ',LREF=',
        str(float(2*datos_cohete['radius'])),
        ',LATREF=',
        str(float(2*datos_cohete['radius'])),
        ',XCG=',
        str(float(datos_cohete['center_of_dry_mass'])),
        ',$\n$REFQ BLAYER=TURB,',
        'RHR=',
        str(float(datos_cohete['RHR'])),
        ',$\n',
        '$AXIBOD TNOSE=',
        datos_cohete['tipo_nose_cone'],
        ',LNOSE=',
        str(float(datos_cohete['longitud_nose_cone'])),
        ',DNOSE=',
        str(float(2*datos_cohete['radius'])),
        ',$\n',
        '$AXIBOD LCENTR=',
        str(float(datos_cohete['longitud']-datos_cohete['longitud_nose_cone'])),
        ',DCENTR=',
        str(float(2*datos_cohete['radius'])),
        ',$\n',
        '$AXIBOD DEXIT=0.,$\n', # Motor apagado. Diámetro de la tobera. 
        '$FINSET1 SECTYP=HEX,$\n',
        '$FINSET1 SSPAN=',
        ','.join(str(float(i)) for i in ([0.0] + [datos_cohete['fins_span']])),
        ',CHORD=',
        ','.join(str(float(i)) for i in ([datos_cohete['fins_root_chord']] + [datos_cohete['fins_tip_chord']])),
        ',XLE=',
        ','.join(str(float(round(i,4))) for i in ([datos_cohete['fins_posicion']] + [datos_cohete['fins_posicion'] + datos_cohete['fins_sweep_lenght']])),
        ',$\n',
        '$FINSET1 NPANEL=',
        str(float(datos_cohete['fins_number'])),
        ',PHIF=',
        ','.join(str(float(i)) for i in datos_cohete['fins_roll_position']),
        ',$\n',
        '$FINSET1 ZUPPER=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['fins_zupper']),
        ',LMAXU=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['fins_lmaxu']),
        ',LFLATU=',
        ','.join(str(float(round(i,3))) for i in datos_cohete['fins_lflatu']),
        ',$\n',
    ]

    # Método de cálculo del número de Reynolds
    if isinstance(altitudes,list) is True and p_y_T is False:
        metodo_reynolds = [
            '$FLTCON ALT=',
            ','.join(f"{float(altitud):.1f}" for altitud in altitudes),
            ',$\n',
        ]
    elif isinstance(p_y_T,list) is True and altitudes is False:
        metodo_reynolds = [
            '$FLTCON PINF=',
            ','.join(str(float(cond[0])) for cond in p_y_T),
            ',$\n',
            '$FLTCON TINF=',
            ','.join(str(float(cond[1])) for cond in p_y_T),
            ',$\n',
        ]
    elif isinstance(p_y_T,list) is True and isinstance(altitudes,list) is True:
        metodo_reynolds = [
            '$FLTCON PINF=',
            ','.join(str(float(cond[0])) for cond in p_y_T),
            ',$\n',
            '$FLTCON TINF=',
            ','.join(str(float(cond[1])) for cond in p_y_T),
            ',$\n',
        ]
    else:
        metodo_reynolds = ['$FLTCON ALT=0.,$\n']
    caso_base += metodo_reynolds

    # Nose cone truncado
    if truncado is True:
        metodo_truncado = [
            '$AXIBOD BNOSE=',
            str(float(datos_cohete['radio_en_truncamiento'])),
            ',TRUNC=.TRUE.,$\n',
        ]
        caso_base += metodo_truncado
    
    # Motor on/off
    if motor_on is True:
        metodo_motor_on = [
            '$AXIBOD DEXIT=',
            str(float(2*datos_cohete['motor_nozzle_radius'])),
            ',$\n',
        ]
        caso_base += metodo_motor_on

    # Aerofreno
    if aerofreno is True:
        metodo_aerofreno = [
            '$PROTUB NPROT=1.0,PTYPE=BLOCK,$\n', # Un conjunto de protuberancias + Tipo BLOCK
            '$PROTUB XPROT=',
            str(float(datos_cohete['aerofreno_posicion'])),
            ',$\n',
            '$PROTUB NLOC=',
            str(float(datos_cohete['aerofreno_n_superficies'])),
            ',$\n',
            '$PROTUB LPROT=',
            str(float(datos_cohete['aerofreno_espesor'])),
            ',WPROT=',
            str(float(datos_cohete['aerofreno_ancho'])),
            ',HPROT=',
            str(float(datos_cohete['aerofreno_altura'])),
            ',$\n',
        ]
        caso_base += metodo_aerofreno

    # Tarjetas de control
    control_cards = [
        'CASEID BETA=',
        str(betas[0]),
        '\nDIM M\n',
        'SOSE\n',
        'DAMP\n',
        'DERIV RAD\n',
        'WRITE SB1,1,120\n',
        'WRITE DB1,1,360\n',
        'SAVE\n',
        'NEXT CASE\n',
    ]
    caso_base += control_cards

    # Variación de beta
    for beta in betas[1:]:
        caso_beta = [
            'CASEID BETA=',
            str(beta),
            '\n',
            '$FLTCON BETA=',
            str(float(beta)),
            ',$\n',
            'DAMP\n',
            'SAVE\n'
            'NEXT CASE\n',
        ]
        caso_base += caso_beta
    
    return caso_base


def comprobar_longitud_filas(ruta_archivo):
    """ Esta función se ha creado para asegurarse de que las filas del
    archivo for005.dat no superan en ningún caso las 79 columnas.
    ------------------------------------------------------------------
    ruta_archivo: string
    Ruta del archivo que se quiere checkear.
    ------------------------------------------------------------------
    return False
    Si alguna fila supera las 79 columnas.
    return True
    Si ninguna fila supera las 79 columnas.
    """

    with open(ruta_archivo, 'r') as archivo:
        for linea in archivo:
            if len(linea.rstrip('\n')) > 79: # Elimina \n antes de contar caracteres
                return False
    
    return True


def escribir_for005(ruta_DATCOM,texto):
    """ Esta función sirve para escribir el archivo for005.dat que
        usará DATCOM para calcular los coeficientes.
        ----------------------------------------------------------
        ruta_DATCOM: string
        Ruta de la carpeta donde se encuentra DATCOM (datcom.exe).
        Ej.: 'D:\\DATCOM'
        texto: string
        Texto que debe escribirse en el archivo for005.dat
    """

    with open(ruta_DATCOM + '\\for005.dat','w') as for005:
        for frase in texto:
            for005.write(frase)

    # Se comprueba si alguna fila supera las 79 columnas
    ok = comprobar_longitud_filas(ruta_DATCOM + '\\for005.dat')
    if ok is False:
        raise LongitudExcedidaError(
            'Algunas líneas del archivo \'for005.dat\' superan las 79 columnas. Corrígelas antes de continuar'
        )
    

def ejecutar_datcom(ruta_DATCOM):
    """ Esta función sirve para ejecutar la aplicación de DATCOM una
        vez que se ha actualizado el archivo for005.dat.
        ------------------------------------------------------------
        ruta_DATCOM: string
        Ruta de la carpeta donde se encuentra DATCOM (datcom.exe).
        Ej.: 'D:\\DATCOM'  
    """

    subprocess.run(
        ruta_DATCOM + '\\datcom.exe',
        cwd = ruta_DATCOM,
        stdout=subprocess.DEVNULL,  # Oculta la salida estándar
        stderr=subprocess.DEVNULL,  # Oculta la salida de errores
        creationflags=subprocess.CREATE_NO_WINDOW  # Evita abrir la ventana de comandos
    )


def leer_for004(ruta_for004):
    """ Esta función sirve para leer el archivo for004.dat con el que
        DATCOM proporciona los coeficientes.
        -------------------------------------------------------------
        ruta_for004: string
        Ruta con la ubicación del archivo for004.dat.
        Ej.: 'D:\\DATCOM\\for004.dat'
        -------------------------------------------------------------
        return for004
        Lista que contiene todos los coeficientes almacenados en el ar-
        chivo for004.dat.
    """

    # Inicialización
    for004 = list()

    with open(ruta_for004) as dat:
        for004_info = dat.readlines()
        for linea in for004_info:
            try:
                for i in list(range(0,80,10)):
                    for004.append(float(linea[i:i+10]))
            except:
                pass
    
    return for004


def definir_coeficientes_datcom(for004,machs,alphas,betas):
    """ Esta función sirve para clasificar los diferentes datos leídos
        en leer_for004 según el tipo de coeficiente al que pertenecen y
        en función del número de Mach, ángulo de ataque y ángulo de de-
        rrape.
        ----------------------------------------------------------------
        for004: lista
        Lista que contiene todos los datos proporcionados por el archivo
        for004.dat, el cual se ha leído en leer_for004.
        machs: lista
        Valores del número de Mach para los cuales se han obtenido coefi-
        cientes.
        alphas: lista
        Valores del ángulo de ataque para los cuales se han obtenido coe-
        ficientes. En deg.
        betas: lista
        Valores del ángulo de derrape para los cuales se han obtenido co-
        eficientes. En deg.
        -----------------------------------------------------------------
        return coeficientes
        Diccionario que contiene todos los coeficientes con sus correspon-
        dientes valores en función del Mach, ángulo de ataque y derrape que
        proporciona DATCOM.
    """

    # Lista de coeficientes disponibles
    lista_coeffs = ['CN0','CM0','CA0','CY0','CLN0','CLL0']
    lista_coeffs += ['CNq','CMq','CAq','CYq','CLNq','CLLq']
    lista_coeffs += ['CNr','CMr','CAr','CYr','CLNr','CLLr']
    lista_coeffs += ['CNp','CMp','CAp','CYp','CLNp','CLLp']

    # Lista de coeficientes adimensionales
    lista_coeffs_0 = ['CN0','CM0','CA0','CY0','CLN0','CLL0']

    # Inicialización de los coeficientes aerodinámicos
    for coeff in lista_coeffs:
        if coeff in lista_coeffs_0:
            locals()[coeff] = {
                ('mach [-]','alpha [deg]','beta [deg]'):coeff + ' [-]',
            }
        else:
            locals()[coeff] = {
                ('mach [-]','alpha [deg]','beta [deg]'):coeff + ' [1/rad]',
            }
    
    # Se extraen los coeficientes del for004
    ciclo_mach = 20 * len(lista_coeffs)
    ciclo_beta = ciclo_mach * len(machs)
    for k in list(range(0,ciclo_beta*len(betas),ciclo_beta)):
        beta = betas[int(k/ciclo_beta)]
        for j in list(range(0,ciclo_mach*len(machs),ciclo_mach)):
            mach = machs[int(j/ciclo_mach)]
            for i in list(range(0,len(alphas),1)):
                alpha = alphas[i]
                for coeff in lista_coeffs:
                    locals()[coeff][(mach,alpha,beta)] = for004[i+j+k]
                    i += 20

    # Se guardan los coeficientes en un diccionario
    coeficientes = dict()
    for coeff in lista_coeffs:
        coeficientes[coeff] = locals()[coeff]
    
    return coeficientes


def generar_csvs(coeficientes,ruta_csvs):
    """ Esta función genera un archivo .csv para cada coeficiente individual
        con sus respectivas variables a partir del diccionario de coeficien-
        tes aerodinámicos.
        --------------------------------------------------------------------
        coeficientes: diccionario
        Diccionario que contiene los coeficientes aerodinámicos.
        ruta_csvs: string
        Ruta de la carpeta donde se quieren almacenar los archivos csv.
    """
    
    # Se lee el diccionario
    for coeficiente, data in coeficientes.items():
        with open(ruta_csvs + '/' + coeficiente + '.csv', mode='w', newline='') as archivo_csv:
            escritor = csv.writer(archivo_csv)
            for (mach, alpha, beta), valor in data.items():
                escritor.writerow([alpha, beta, mach, valor])

    print('Archivos CSV generados con éxito en: ' + ruta_csvs)


def transformar_coeficientes_a_rocketpy(coeficientes,machs,alphas,betas):
    """ Esta función adapta los coeficientes aerodinámicos porporcionados
        por DATCOM para que se puedan introducir en RocketPy.
        -----------------------------------------------------------------
        coeficientes: diccionario
        Diccionario de coeficientes obtenido en la función definir_coefi-
        cientes_datcom.
        machs: lista
        Lista de números de Mach para los que se han obtenido los coefi-
        cientes.
        alphas: lista
        Lista de ángulos de ataque para los que se han obtenido los coe-
        ficientes.
        betas: lista
        Lista de ángulos de derrape para los que se han obtenido los co-
        eficientes. 
    """
    
    # Deg to rad
    r = np.pi/180

    # Lista de coeficientes de DATCOM (en ejes cuerpo)
    lista_coeffs_datcom = ['CN0','CA0','CY0','CM0','CLN0','CLL0']
    lista_coeffs_datcom += ['CNq','CAq','CYq','CMq','CLNq','CLLq']
    lista_coeffs_datcom += ['CNr','CAr','CYr','CMr','CLNr','CLLr']
    lista_coeffs_datcom += ['CNp','CAp','CYp','CMp','CLNp','CLLp']

    # Lista de coeficientes de RocketPy (en ejes viento)
    lista_coeffs_rocketpy = ['CL0','CD0','CQ0','Cm0','Cn0','Cl_0']
    lista_coeffs_rocketpy += ['CLq','CDq','CQq','Cmq','Cnq','Cl_q']
    lista_coeffs_rocketpy += ['CLr','CDr','CQr','Cmr','Cnr','Cl_r']
    lista_coeffs_rocketpy += ['CLp','CDp','CQp','Cmp','Cnp','Cl_p']

    # Se cargan los coeficientes de DATCOM
    for coeff in lista_coeffs_datcom:
        locals()[coeff] = coeficientes[coeff]

    # Se inicializan los coeficientes de RocketPy
    for coeff in lista_coeffs_rocketpy:
        if coeff in lista_coeffs_rocketpy[0:6]:
            locals()[coeff] = {
                ('mach','alpha','beta'):coeff + ' [-]',
            }
        else:
            locals()[coeff] = {
                ('mach','alpha','beta'):coeff + ' [1/rad]',
            }

    # Se transforman los coeficientes
    for i in list(range(0,len(lista_coeffs_datcom),6)):
        for mach in machs:
            for alpha in alphas:
                for beta in betas:
                    locals()[lista_coeffs_rocketpy[i+1]][(mach,alpha*r,beta*r)] = - (
                        - locals()[lista_coeffs_datcom[i+1]][(mach,alpha,beta)] * np.cos(alpha*r) * np.cos(beta*r)
                        - locals()[lista_coeffs_datcom[i]][(mach,alpha,beta)] * np.cos(beta*r) * np.sin(alpha*r)
                        + locals()[lista_coeffs_datcom[i+2]][(mach,alpha,beta)] * np.sin(beta*r)
                    )
                    locals()[lista_coeffs_rocketpy[i+2]][(mach,alpha*r,beta*r)] = (
                        locals()[lista_coeffs_datcom[i+2]][(mach,alpha,beta)] * np.cos(beta*r)
                        + locals()[lista_coeffs_datcom[i+1]][(mach,alpha,beta)] * np.cos(alpha*r) * np.sin(beta*r)
                        + locals()[lista_coeffs_datcom[i]][(mach,alpha,beta)] * np.sin(alpha*r) * np.sin(beta*r)
                    )
                    locals()[lista_coeffs_rocketpy[i]][(mach,alpha*r,beta*r)] = - (
                        - locals()[lista_coeffs_datcom[i]][(mach,alpha,beta)] * np.cos(alpha*r)
                        + locals()[lista_coeffs_datcom[i+1]][(mach,alpha,beta)] * np.sin(alpha*r)
                    )
                    locals()[lista_coeffs_rocketpy[i+3]][(mach,alpha*r,beta*r)] = locals()[lista_coeffs_datcom[i+3]][(mach,alpha,beta)]
                    locals()[lista_coeffs_rocketpy[i+4]][(mach,alpha*r,beta*r)] = locals()[lista_coeffs_datcom[i+4]][(mach,alpha,beta)]
                    locals()[lista_coeffs_rocketpy[i+5]][(mach,alpha*r,beta*r)] = locals()[lista_coeffs_datcom[i+5]][(mach,alpha,beta)]
    
    # Se guardan los coeficientes de Rocketpy
    coeficientes_rocketpy = dict()
    for coeff in lista_coeffs_rocketpy:
        coeficientes_rocketpy[coeff] = locals()[coeff]

    return coeficientes_rocketpy


def definir_diccionario_coeficientes_rocketpy(for004,alpha,beta):
    """ Esta función extrae los coeficientes adimensionales del archivo
        for004.dat para unas condiciones de vuelo dadas (solo un mach,
        ángulo de ataque y de derrape). Esta función está pensada para
        obtener los coeficientes a tiempo real durante la simulación de
        vuelo. Proporciona un único valor para cada uno de los 6 coefi-
        cientes.
        -----------------------------------------------------------------
        for004: lista
        Lista que contiene todos los datos proporcionados por el archivo
        for004.dat, el cual se ha leído en leer_for004.
        alpha: float
        Valor del ángulo de ataque en este instante del vuelo en deg.
        beta: float
        Valor del ángulo de derrape en este instante del vuelo en deg.
        -----------------------------------------------------------------
        coeficientes: diccionario
        Devuelve un diccionario con los 6 coeficientes del cohete en el 
        formato de RocketPy. Tiene el siguiente formato:
        coeficientes = {
            "cL": 0,
            "cQ": 0,
            "cD": 0,
            "cm": 0,
            "cn": 0,
            "cl": 0,
        }
    """
    
    # Deg to rad
    r = np.pi/180

    # Creación del diccionario con los coeficientes
    coeficientes_datcom = dict.fromkeys(['CN','CM','CA','CY','CLN','CLL'], 0)

    # Se extraen los coeficientes del for004
    for004 = for004[0:20 * len(coeficientes_datcom)]

    # Se extraen los coeficientes del for004
    i = 0
    for coeff in coeficientes_datcom:
        coeficientes_datcom[coeff] = for004[i]
        i += 20

    # Se crea el diccionario de coeficientes de RocketPy
    coeficientes = dict.fromkeys(["cL","cQ","cD"], 0)
    coeficientes["cm"] = coeficientes_datcom['CM']
    coeficientes["cn"] = coeficientes_datcom['CLN']
    coeficientes["cl"] = coeficientes_datcom['CLL']

    # Se transforman los coeficientes de fuerza a ejes viento
    coeficientes["cL"] = - (
        - coeficientes_datcom['CN'] * np.cos(alpha*r)
        + coeficientes_datcom['CA'] * np.sin(alpha*r)
    )
    coeficientes["cQ"] = (
        coeficientes_datcom['CY'] * np.cos(beta*r)
        + coeficientes_datcom['CA'] * np.cos(alpha*r) * np.sin(beta*r)
        + coeficientes_datcom['CN'] * np.sin(alpha*r) * np.sin(beta*r)
    )
    coeficientes["cD"] = - (
        - coeficientes_datcom['CA'] * np.cos(alpha*r) * np.cos(beta*r)
        - coeficientes_datcom['CN'] * np.cos(beta*r) * np.sin(alpha*r)
        + coeficientes_datcom['CY'] * np.sin(beta*r)
    )

    return coeficientes


def definir_drag_rocketpy(for004):
    """ Esta función extrae el coeficiente de fuerza axial del for004.dat
        para introducirlo como coeficiente de drag (on/off) en RocketPy.
        Esta función esta pensada para obtener los coeficientes a tiempo
        real durante la simulación de vuelo. Por lo tanto, proporciona
        un único valor del CA para las condiciones de vuelo dadas.
        -----------------------------------------------------------------
        for004: lista
        Lista que contiene todos los datos proporcionados por el archivo
        for004.dat, el cual se ha leído en leer_for004.
        -----------------------------------------------------------------
        power on/off drag: float
        Coeficiente de drag power on/off de RocketPy.
    """
    
    # Deg to rad
    r = np.pi/180

    # Creación del diccionario con los coeficientes
    coeficientes_datcom = dict.fromkeys(['CN','CM','CA','CY','CLN','CLL'], 0)

    # Se extraen los coeficientes del for004
    for004 = for004[0:20 * len(coeficientes_datcom)]

    # Se extraen los coeficientes del for004
    i = 0
    for coeff in coeficientes_datcom:
        coeficientes_datcom[coeff] = for004[i]
        i += 20

    return coeficientes_datcom['CA']


def ubicacion_csvs_coeficientes(carpeta_csv):
    """ Esta función sirve para generar el diccionario necesario para indicarle
        a RocketPy la ubicación de los coeficientes.
        -----------------------------------------------------------------------
        carpeta_csv: string
        Ubicación de la carpeta donde se encuentran los .csv
        -----------------------------------------------------------------------
        coeficientes: diccionario
        Diccionario con la ubicación de cada uno de los coeficientes.
    """

    # Lista de coeficientes
    lista_coeffs_csv = ['CL0','CD0','CQ0','Cm0','Cn0','Cl_0']
    lista_coeffs_csv += ['CLq','CDq','CQq','Cmq','Cnq','Cl_q']
    lista_coeffs_csv += ['CLr','CDr','CQr','Cmr','Cnr','Cl_r']
    lista_coeffs_csv += ['CLp','CDp','CQp','Cmp','Cnp','Cl_p']
    
    # Lista de coeficientes como los quiere Rocketpy
    lista_coeffs_rocketpy = ['cL_0','cD_0','cQ_0','cm_0','cn_0','cl_0']
    lista_coeffs_rocketpy += ['cL_q','cD_q','cQ_q','cm_q','cn_q','cl_q']
    lista_coeffs_rocketpy += ['cL_r','cD_r','cQ_r','cm_r','cn_r','cl_r']
    lista_coeffs_rocketpy += ['cL_p','cD_p','cQ_p','cm_p','cn_p','cl_p']

    # Inicialización diccionario
    coeficientes = dict()

    for i in list(range(0,len(lista_coeffs_csv),1)):
        coeficientes[lista_coeffs_rocketpy[i]] = carpeta_csv + '/' + lista_coeffs_csv[i] + '.csv'
    
    return coeficientes


def convertir_a_mach_vs_coeficiente(archivo_entrada, archivo_salida):
    """ Esta función sirve para convertir los coeficientes definidos como función
        de tres variables, mach, alpha y beta, a solo función del mach. Esto está
        especialmente pensado para generar los CSV con el power on/off drag de Ro-
        cketPy, por lo que se utiliza este formato. Para que funcione correctamen-
        te se deben calcular los coeficientes únicamente para alpha y beta iguales
        a cero. Si no, aparecerán varios valores del coeficiente para un mismo nú-
        mero de Mach.
        --------------------------------------------------------------------------
        archivo_entrada: string
        Ubicación del archivo CSV con el coeficiente en función de tres variables.
        archivo_salida: string
        Ubicación donde se pretende guardar el CSV resultante con el coeficiente ú-
        nicamente función del Mach.
    """
    with open(archivo_entrada, mode='r', newline='') as infile:
        reader = csv.reader(infile)
        
        # Salta el encabezado
        next(reader, None)
        
        # Extrae las columnas del mach y del coeficiente
        filtered_rows = [(row[2], row[3]) for row in reader]
    
    with open(archivo_salida, mode='w', newline='') as outfile:
        writer = csv.writer(outfile)
        writer.writerows(filtered_rows)

    print('El archivo ' + archivo_salida + ' ha sido generado con éxito')
