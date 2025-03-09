###### FARADAY ROCKETRY UPV ######

# Módulo para obtener automáticamente los coeficientes
# aerodinámicos utilizando DATCOM.

###   Versión 2.0  (9/3/2025)   ###

import numpy as np
import subprocess
import csv


class DATCOM:
    """Esta clase sirve para crear un objeto con la ubicación de la aplicación de DATCOM, de forma que
       sus funciones sean válidas para todo el mundo independientemente de dónde tengan 'datcom.exe'.
    """

    def __init__(self,datcom_path):
        """Para inicializar la clase de DATCOM la unica propiedad que hay que definir es la ubicación
           de la carpeta donde se encuentra 'datcom.exe'. Por ejemplo: 'D:\\DATCOM', cuando la apli-
           cación se encuentra en 'D:\\DATCOM\\datcom.exe'.
           -------------------------------------------------------------------------------------------
           datcom_path: str
           Ubicación de la carpeta que contiene a 'datcom.exe'.
        """

        self.datcom_path = datcom_path


    def for005_doble_etapa(self,data,machs,alphas,betas,altitudes=False,p_y_T=False,motor_on=False):
        """ Esta función sirve para obtener el texto del 'for005.dat' para cohetes doble etapa.
            Es precisa únicamente cuando el Mach de vuelo no alcanza velocidades supersónicas.
            Unidades del Sistema Internacional a no ser que se diga lo contrario.
            ---------------------------------------------------------------------------------
            data: diccionario
            Aquí se deben introducir todos los datos del cohete necesarios para obtener los
            coeficientes.
            Datos que se deben introducir: {
                'booster_radius':data.booster_radius,
                'cohete_center_of_dry_mass':data.origin_center_of_dry_mass,
                'RHR':data.RHR,
                'contorno_cohete_x':data.contorno_cohete_x,
                'contorno_cohete_r':data.contorno_cohete_r,
                'nc_x':data.nc_x,
                'as_span':data.as_span,
                'as_root_chord':data.as_root_chord,
                'as_tip_chord':data.as_tip_chord,
                'as_posicion':data.as_posicion,
                'as_sweep_lenght':data.as_sweep_lenght,
                'as_n':data.as_n,
                'as_roll':data.as_roll,
                'as_zupper':data.as_zupper,
                'as_lmaxu':data.as_lmaxu,
                'as_lflatu':data.as_lflatu,
                'ab_span':data.ab_span,
                'ab_root_chord':data.ab_root_chord,
                'ab_tip_chord':data.ab_tip_chord,
                'ab_xle':data.ab_xle,
                'ab_n':data.ab_n,
                'ab_roll':data.ab_roll,
                'ab_zupper':data.ab_zupper,
                'ab_lmaxu':data.ab_lmaxu,
                'ab_lflatu':data.ab_lflatu,
                'mb_nozzle_radius':data.mb_nozzle_radius    
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
            Se deberá adjuntar el radio de la tobera en data en caso verdadero.
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
            str(float(round(np.pi * data['booster_radius']**2,4))),
            ',LREF=',
            str(float(2*data['booster_radius'])),
            ',LATREF=',
            str(float(2*data['booster_radius'])),
            ',XCG=',
            str(float(data['cohete_center_of_dry_mass'])),
            ',$\n$REFQ BLAYER=TURB,',
            'RHR=',
            str(float(data['RHR'])),
            ',$\n',
            '$AXIBOD NX=',
            str(float(len(data['contorno_cohete_x']))),
            ',$\n',
            '$AXIBOD X(1)=',
            ','.join(str(float(round(x,3))) for x in data['contorno_cohete_x'][0:10]),
            ',$\n',
            '$AXIBOD X(11)=',
            ','.join(str(float(round(x,3))) for x in data['contorno_cohete_x'][10:20]),
            ',$\n',
            '$AXIBOD X(21)=',
            ','.join(str(float(round(x,3))) for x in data['contorno_cohete_x'][20:30]),
            ',$\n',
            '$AXIBOD X(31)=',
            ','.join(str(float(round(x,3))) for x in data['contorno_cohete_x'][30:40]),
            ',$\n',
            '$AXIBOD X(41)=',
            ','.join(str(float(round(x,3))) for x in data['contorno_cohete_x'][40:50]),
            ',$\n',
            '$AXIBOD R(1)=',
            ','.join(str(float(round(r,3))) for r in data['contorno_cohete_r'][0:10]),
            ',$\n',
            '$AXIBOD R(11)=',
            ','.join(str(float(round(r,3))) for r in data['contorno_cohete_r'][10:20]),
            ',$\n',
            '$AXIBOD R(21)=',
            ','.join(str(float(round(r,3))) for r in data['contorno_cohete_r'][20:30]),
            ',$\n',
            '$AXIBOD R(31)=',
            ','.join(str(float(round(r,3))) for r in data['contorno_cohete_r'][30:40]),
            ',$\n',
            '$AXIBOD R(41)=',
            ','.join(str(float(round(r,3))) for r in data['contorno_cohete_r'][40:50]),
            ',$\n',
            '$AXIBOD DISCON=',
            ','.join(str(float(i + 1)) for i in list(range(len(data['nc_x']),len(data['contorno_cohete_x']),1))),
            ',$\n',
            '$AXIBOD BNOSE=0.,TRUNC=.FALSE.,$\n',
            '$AXIBOD DEXIT=0.,$\n', # Motor apagado. Diámetro de la tobera. 
            '$FINSET1 SECTYP=HEX,$\n',
            '$FINSET1 SSPAN=',
            ','.join(str(float(i)) for i in ([0.0] + [data['as_span']])),
            ',CHORD=',
            ','.join(str(float(i)) for i in ([data['as_root_chord']] + [data['as_tip_chord']])),
            ',XLE=',
            ','.join(str(float(round(i,4))) for i in ([data['as_posicion']] + [data['as_posicion'] + data['as_sweep_lenght']])),
            ',$\n',
            '$FINSET1 NPANEL=',
            str(float(data['as_n'])),
            ',PHIF=',
            ','.join(str(float(i)) for i in data['as_roll']),
            ',$\n',
            '$FINSET1 ZUPPER=',
            ','.join(str(float(round(i,3))) for i in data['as_zupper']),
            ',LMAXU=',
            ','.join(str(float(round(i,3))) for i in data['as_lmaxu']),
            ',LFLATU=',
            ','.join(str(float(round(i,3))) for i in data['as_lflatu']),
            ',$\n',
            '$FINSET2 SECTYP=HEX,$\n',
            '$FINSET2 SSPAN=',
            ','.join(str(float(i)) for i in ([0.0] + [data['ab_span']])),
            ',CHORD=',
            ','.join(str(float(i)) for i in ([data['ab_root_chord']] + [data['ab_tip_chord']])),
            ',XLE=',
            ','.join(str(float(round(i,4))) for i in data['ab_xle']),
            ',$\n',
            '$FINSET2 NPANEL=',
            str(float(data['ab_n'])),
            ',PHIF=',
            ','.join(str(float(i)) for i in data['ab_roll']),
            ',$\n',
            '$FINSET2 ZUPPER=',
            ','.join(str(float(round(i,3))) for i in data['ab_zupper']),
            ',LMAXU=',
            ','.join(str(float(round(i,3))) for i in data['ab_lmaxu']),
            ',LFLATU=',
            ','.join(str(float(round(i,3))) for i in data['ab_lflatu']),
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
                str(float(2*data['mb_nozzle_radius'])),
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


    def for005_mono_etapa(self,data,machs,alphas,betas,altitudes=False,p_y_T=False,motor_on=False,truncado=False,aerofreno=False):
        """ Esta función sirve para obtener el texto del for005.dat para cohetes monoetapa.
            Unidades del Sistema Internacional a no ser que se diga lo contrario.
            ---------------------------------------------------------------------------------
            data: diccionario
            Aquí se deben introducir todos los datos del cohete necesarios para obtener los
            coeficientes.
            Datos que se deben introducir: {
                'radius':data.sustainer_radius_inferior,
                'center_of_dry_mass':data.sustainer_center_of_dry_mass,
                'RHR':data.RHR,
                'tipo_nose_cone':data.nc_tipo,
                'longitud_nose_cone':data.nc_longitud,
                'longitud':data.sustainer_longitud,
                'fins_span':data.as_span,
                'fins_root_chord':data.as_root_chord,
                'fins_tip_chord':data.as_tip_chord,
                'fins_posicion':data.as_posicion,
                'fins_sweep_lenght':data.as_sweep_lenght,
                'fins_number':data.as_n,
                'fins_roll_position':data.as_roll,
                'fins_zupper':data.as_zupper,
                'fins_lmaxu':data.as_lmaxu,
                'fins_lflatu':data.as_lflatu,
                'radio_en_truncamiento':data.interstage_radius_superior,
                'motor_nozzle_radius':data.ms_nozzle_radius,   
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
            Se deberá adjuntar el radio de la tobera en data en caso verdadero.
            truncado: buleano (opcional)
            False si el nose cone no está truncado.
            True si el nose cone está truncado.
            Se deberá adjuntar el radio en el truncamiento en data en caso verdadero.
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
            str(float(round(np.pi * data['radius']**2,5))),
            ',LREF=',
            str(float(2*data['radius'])),
            ',LATREF=',
            str(float(2*data['radius'])),
            ',XCG=',
            str(float(data['center_of_dry_mass'])),
            ',$\n$REFQ BLAYER=TURB,',
            'RHR=',
            str(float(data['RHR'])),
            ',$\n',
            '$AXIBOD TNOSE=',
            data['tipo_nose_cone'],
            ',LNOSE=',
            str(float(data['longitud_nose_cone'])),
            ',DNOSE=',
            str(float(2*data['radius'])),
            ',$\n',
            '$AXIBOD LCENTR=',
            str(float(data['longitud']-data['longitud_nose_cone'])),
            ',DCENTR=',
            str(float(2*data['radius'])),
            ',$\n',
            '$AXIBOD DEXIT=0.,$\n', # Motor apagado. Diámetro de la tobera. 
            '$FINSET1 SECTYP=HEX,$\n',
            '$FINSET1 SSPAN=',
            ','.join(str(float(i)) for i in ([0.0] + [data['fins_span']])),
            ',CHORD=',
            ','.join(str(float(i)) for i in ([data['fins_root_chord']] + [data['fins_tip_chord']])),
            ',XLE=',
            ','.join(str(float(round(i,4))) for i in ([data['fins_posicion']] + [data['fins_posicion'] + data['fins_sweep_lenght']])),
            ',$\n',
            '$FINSET1 NPANEL=',
            str(float(data['fins_number'])),
            ',PHIF=',
            ','.join(str(float(i)) for i in data['fins_roll_position']),
            ',$\n',
            '$FINSET1 ZUPPER=',
            ','.join(str(float(round(i,3))) for i in data['fins_zupper']),
            ',LMAXU=',
            ','.join(str(float(round(i,3))) for i in data['fins_lmaxu']),
            ',LFLATU=',
            ','.join(str(float(round(i,3))) for i in data['fins_lflatu']),
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
                str(float(data['radio_en_truncamiento'])),
                ',TRUNC=.TRUE.,$\n',
            ]
            caso_base += metodo_truncado
        
        # Motor on/off
        if motor_on is True:
            metodo_motor_on = [
                '$AXIBOD DEXIT=',
                str(float(2*data['motor_nozzle_radius'])),
                ',$\n',
            ]
            caso_base += metodo_motor_on

        # Aerofreno
        if aerofreno is True:
            metodo_aerofreno = [
                '$PROTUB NPROT=1.0,PTYPE=BLOCK,$\n', # Un conjunto de protuberancias + Tipo BLOCK
                '$PROTUB XPROT=',
                str(float(data['aerofreno_posicion'])),
                ',$\n',
                '$PROTUB NLOC=',
                str(float(data['aerofreno_n_superficies'])),
                ',$\n',
                '$PROTUB LPROT=',
                str(float(data['aerofreno_espesor'])),
                ',WPROT=',
                str(float(data['aerofreno_ancho'])),
                ',HPROT=',
                str(float(data['aerofreno_altura'])),
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
    
    
    def comprobar_longitud_filas(self):
        """ Esta función se ha creado para asegurarse de que las filas del
        archivo for005.dat no superan en ningún caso las 79 columnas.
        ------------------------------------------------------------------
        ------------------------------------------------------------------
        return False
        Si alguna fila supera las 79 columnas.
        return True
        Si ninguna fila supera las 79 columnas.
        """

        with open(self.datcom_path + '\\for005.dat', 'r') as archivo:
            for linea in archivo:
                if len(linea.rstrip('\n')) > 79: # Elimina \n antes de contar caracteres
                    return False
        
        return True
    
    
    def escribir_for005(self,texto):
        """ Esta función sirve para escribir el archivo for005.dat que
            usará DATCOM para calcular los coeficientes.
            ----------------------------------------------------------
            texto: string
            Texto que debe escribirse en el archivo for005.dat
        """

        with open(self.datcom_path + '\\for005.dat','w') as for005:
            for frase in texto:
                for005.write(frase)

        # Se comprueba si alguna fila supera las 79 columnas
        ok = self.comprobar_longitud_filas()
        if ok is False:
            raise LongitudExcedidaError(
                'Algunas líneas del archivo \'for005.dat\' superan las 79 columnas. Corrígelas antes de continuar'
            )
        

    def ejecutar_datcom(self):
        """ Esta función sirve para ejecutar la aplicación de DATCOM una
            vez que se ha actualizado el archivo for005.dat.
        """

        subprocess.run(
            self.datcom_path + '\\datcom.exe',
            cwd = self.datcom_path,
            stdout=subprocess.DEVNULL,  # Oculta la salida estándar
            stderr=subprocess.DEVNULL,  # Oculta la salida de errores
            creationflags=subprocess.CREATE_NO_WINDOW  # Evita abrir la ventana de comandos
        )
    
    
    def leer_for004(self):
        """ Esta función sirve para leer el archivo for004.dat con el que
            DATCOM proporciona los coeficientes.
            -------------------------------------------------------------
            -------------------------------------------------------------
            return for004
            Lista que contiene todos los coeficientes almacenados en el ar-
            chivo for004.dat.
        """

        # Inicialización
        for004 = list()

        with open(self.datcom_path + '\\for004.dat') as dat:
            for004_info = dat.readlines()
            for linea in for004_info:
                try:
                    for i in list(range(0,80,10)):
                        for004.append(float(linea[i:i+10]))
                except:
                    pass
        
        return for004
    

    def definir_coeficientes_datcom(self,for004,machs,alphas,betas):
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


    def generar_csvs(self,coeficientes,ruta_csvs):
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


    def transformar_coeficientes_a_rocketpy(self,coeficientes,machs,alphas,betas):
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
    

    def definir_diccionario_coeficientes_rocketpy(self,for004,alpha,beta):
        """ Esta función extrae los coeficientes del archivo for004.dat para unas 
            condiciones de vuelo dadas (solo un mach, ángulo de ataque y de derra-
            pe). Esta función está pensada para obtener los coeficientes a tiempo
            real durante la simulación de vuelo. Proporciona un único valor cons-
            tante para cada uno de los 24 coeficientes.
            -----------------------------------------------------------------
            for004: lista
            Lista que contiene todos los datos proporcionados por el archivo
            for004.dat, el cual se ha leído en leer_for004.
            alpha: float
            Valor del ángulo de ataque en este instante del vuelo. En deg.
            beta: float
            Valor del ángulo de derrape en este instante del vuelo. En deg.
            -----------------------------------------------------------------
            coeficientes: diccionario
            Devuelve un diccionario con los 24 coeficientes del cohete en el 
            formato de RocketPy. Tiene el siguiente formato:
            coeficientes = {
                "cL_0": 0,
                "cQ_0": 0,
                "cD_0": 0,
                "cm_0": 0,
                "cn_0": 0,
                "cl_0": 0,
                "cL_q": 0,
                "cQ_q": 0,
                "cD_q": 0,
                "cm_q": 0,
                "cn_q": 0,
                "cl_q": 0,
                "cL_r": 0,
                "cQ_r": 0,
                "cD_r": 0,
                "cm_r": 0,
                "cn_r": 0,
                "cl_r": 0,
                "cL_p": 0,
                "cQ_p": 0,
                "cD_p": 0,
                "cm_p": 0,
                "cn_p": 0,
                "cl_p": 0,
            }
        """
        
        # Deg to rad
        r = np.pi/180

        # Lista de coeficientes de DATCOM (en ejes cuerpo)
        lista_coeffs_datcom = ['CN0','CM0','CA0','CY0','CLN0','CLL0']
        lista_coeffs_datcom += ['CNq','CMq','CAq','CYq','CLNq','CLLq']
        lista_coeffs_datcom += ['CNr','CMr','CAr','CYr','CLNr','CLLr']
        lista_coeffs_datcom += ['CNp','CMp','CAp','CYp','CLNp','CLLp']
        coeficientes_datcom = dict.fromkeys(lista_coeffs_datcom, 0)

        # Se extraen los coeficientes del for004
        i = 0
        for coeff in coeficientes_datcom:
            coeficientes_datcom[coeff] = for004[i]
            i += 20

        # Lista de coeficientes de RocketPy (en ejes viento)
        coeficientes = {}

        # Se transforman los coeficientes a ejes viento
        coeficientes["cm_0"] = coeficientes_datcom['CM0']
        coeficientes["cm_q"] = coeficientes_datcom['CMq']
        coeficientes["cm_r"] = coeficientes_datcom['CMr']
        coeficientes["cm_p"] = coeficientes_datcom['CMp']

        coeficientes["cn_0"] = coeficientes_datcom['CLN0']
        coeficientes["cn_q"] = coeficientes_datcom['CLNq']
        coeficientes["cn_r"] = coeficientes_datcom['CLNr']
        coeficientes["cn_p"] = coeficientes_datcom['CLNp']

        coeficientes["cl_0"] = coeficientes_datcom['CLL0']
        coeficientes["cl_q"] = coeficientes_datcom['CLLq']
        coeficientes["cl_r"] = coeficientes_datcom['CLLr']
        coeficientes["cl_p"] = coeficientes_datcom['CLLp']

        coeficientes["cL_0"] = - (
            - coeficientes_datcom['CN0'] * np.cos(alpha*r)
            + coeficientes_datcom['CA0'] * np.sin(alpha*r)
        )
        coeficientes["cQ_0"] = (
            coeficientes_datcom['CY0'] * np.cos(beta*r)
            + coeficientes_datcom['CA0'] * np.cos(alpha*r) * np.sin(beta*r)
            + coeficientes_datcom['CN0'] * np.sin(alpha*r) * np.sin(beta*r)
        )
        coeficientes["cD_0"] = - (
            - coeficientes_datcom['CA0'] * np.cos(alpha*r) * np.cos(beta*r)
            - coeficientes_datcom['CN0'] * np.cos(beta*r) * np.sin(alpha*r)
            + coeficientes_datcom['CY0'] * np.sin(beta*r)
        )

        coeficientes["cL_q"] = - (
            - coeficientes_datcom['CNq'] * np.cos(alpha*r)
            + coeficientes_datcom['CAq'] * np.sin(alpha*r)
        )
        coeficientes["cQ_q"] = (
            coeficientes_datcom['CYq'] * np.cos(beta*r)
            + coeficientes_datcom['CAq'] * np.cos(alpha*r) * np.sin(beta*r)
            + coeficientes_datcom['CNq'] * np.sin(alpha*r) * np.sin(beta*r)
        )
        coeficientes["cD_q"] = - (
            - coeficientes_datcom['CAq'] * np.cos(alpha*r) * np.cos(beta*r)
            - coeficientes_datcom['CNq'] * np.cos(beta*r) * np.sin(alpha*r)
            + coeficientes_datcom['CYq'] * np.sin(beta*r)
        )

        coeficientes["cL_r"] = - (
            - coeficientes_datcom['CNr'] * np.cos(alpha*r)
            + coeficientes_datcom['CAr'] * np.sin(alpha*r)
        )
        coeficientes["cQ_r"] = (
            coeficientes_datcom['CYr'] * np.cos(beta*r)
            + coeficientes_datcom['CAr'] * np.cos(alpha*r) * np.sin(beta*r)
            + coeficientes_datcom['CNr'] * np.sin(alpha*r) * np.sin(beta*r)
        )
        coeficientes["cD_r"] = - (
            - coeficientes_datcom['CAr'] * np.cos(alpha*r) * np.cos(beta*r)
            - coeficientes_datcom['CNr'] * np.cos(beta*r) * np.sin(alpha*r)
            + coeficientes_datcom['CYr'] * np.sin(beta*r)
        )

        coeficientes["cL_p"] = - (
            - coeficientes_datcom['CNp'] * np.cos(alpha*r)
            + coeficientes_datcom['CAp'] * np.sin(alpha*r)
        )
        coeficientes["cQ_p"] = (
            coeficientes_datcom['CYp'] * np.cos(beta*r)
            + coeficientes_datcom['CAp'] * np.cos(alpha*r) * np.sin(beta*r)
            + coeficientes_datcom['CNp'] * np.sin(alpha*r) * np.sin(beta*r)
        )
        coeficientes["cD_p"] = - (
            - coeficientes_datcom['CAp'] * np.cos(alpha*r) * np.cos(beta*r)
            - coeficientes_datcom['CNp'] * np.cos(beta*r) * np.sin(alpha*r)
            + coeficientes_datcom['CYp'] * np.sin(beta*r)
        )

        return coeficientes


    def definir_drag_rocketpy(self,for004):
        """ Esta función extrae el coeficiente de fuerza axial del for004.dat
            para introducirlo como coeficiente de drag (on/off) en RocketPy.
            Esta función esta pensada para obtener los coeficientes a tiempo
            real durante la simulación de vuelo. Por lo tanto, proporciona
            un único valor constante del CA para las condiciones de vuelo dadas.
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


    def ubicacion_csvs_coeficientes(self,carpeta_csv):
        """ Esta función sirve para generar el diccionario necesario para indicarle
            a RocketPy la ubicación de los CSV con los coeficientes.
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


    def convertir_a_mach_vs_coeficiente(self,archivo_entrada,archivo_salida):
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


    def motor_status(self,data,flight):
        """Esta función sirve para determinar durante la simulación de vuelo si el motor
           está encendido o apagado y, en consecuencia, proporcionar el valor buleano de
           la variable 'motor_on' de las funciones que escriben el archivo 'for005.dat'.
           -----------------------------------------------------------------------------
           data: diccionario
           Diccionario que contiene los datos del cohete.
           flight: Flight
           Objeto de la clase Flight que se quiere examinar.
           -----------------------------------------------------------------------------
           motor_on: buleano
           True si está encendido el motor del cohete.
           False si está apagado.
        """

        # Motor on/off
        t = flight.t
        if data['start_burning_time'] < t < data['burnout_time']:
            return True
        else:
            return False
        

    def check_number(self,number):
        """Esta función comprueba que el formato de los números es adecuado para introdu-
           cirlo en el archivo for005.dat. Este archivo no admite notación científica y
           en Python, por defecto, se utiliza para números menores que 0.0001.
           ------------------------------------------------------------------------------
           number: int, float
           Número que se pretende examinar.
           ------------------------------------------------------------------------------
           number: int, float
           Número adaptado para el archivo 'for005.dat'.
        """

        if abs(number) < 0.0001:
            return 0
        else:
            return number


    def run_datcom_for_drag(self,data,flight,environment):
        """Calcula el coeficiente de fuerza axial que se corresponde con el drag power
        on/off de RocketPy. El aerofreno se encuentra activado por defecto. Si no se
        desea calcular su resistencia aerodinámica en el diccionario data se deben in-
        troducir sus características geométricas como nulas.
        ------------------------------------------------------------------------------
        data: diccionario
        Datos del cohete para el que se quiere obtener el coeficiente.
        flight: Flight
        Objeto de la clase Flight para cuyas condiciones de vuelo se quiere obtener el
        coeficiente.
        environment: Environment
        Objeto de la clase Environment para cuyas condiciones atmosféricas se que quie-
        re obtener el coeficiente.
        -------------------------------------------------------------------------------
        CD: float
        Devuelve el coeficiente de fuerza axial del cohete.
        """

        # Se cargan las variables del vuelo
        mach = self.check_number(flight.mach_number(flight.t))
        alpha = self.check_number(flight.partial_angle_of_attack(flight.t))
        beta = self.check_number(flight.angle_of_sideslip(flight.t))
        pressure = flight.pressure(flight.t)
        temperature = environment.temperature(flight.z(flight.t))

        # Motor on/off
        motor_on = self.motor_status(data,flight)

        # Ejecución de DATCOM
        texto_for005 = self.for005_mono_etapa(data,[mach],[alpha],[beta],p_y_T=[[pressure,temperature]],motor_on=motor_on,aerofreno=True)
        self.escribir_for005(texto_for005)
        self.ejecutar_datcom()
        for004_info = self.leer_for004()
        drag = self.definir_drag_rocketpy(for004_info)

        return drag


    def run_datcom_for_coefficients(self,data,flight,environment):
        """Calcula los coeficientes de DATCOM del cohete en el formato de RocketPy a par-
        tir de las condiciones de vuelo actuales. Proporciona un único valor constante pa-
        ra cada uno de los 24 coeficientes.
        ------------------------------------------------------------------------------
        data: diccionario
        Datos del cohete para el que se quieren obtener los coeficientes.
        flight: Flight
        Objeto de la clase Flight para cuyas condiciones de vuelo se quiere obtener el
        coeficiente.
        environment: Environment
        Objeto de la clase Environment para cuyas condiciones atmosféricas se que quie-
        re obtener el coeficiente.
        -------------------------------------------------------------------------------
        coeficientes: diccionario
        Devuelve un diccionario con los 24 coeficientes del cohete en el formato de Ro-
        cketPy.
        """

        # Se cargan las variables del vuelo
        mach = self.check_number(flight.mach_number(flight.t))
        alpha = self.check_number(flight.partial_angle_of_attack(flight.t))
        beta = self.check_number(flight.angle_of_sideslip(flight.t))
        pressure = flight.pressure(flight.t)
        temperature = environment.temperature(flight.z(flight.t))

        # Motor on/off
        motor_on = self.motor_status(data,flight)

        # Ejecución de DATCOM
        texto_for005 = self.for005_mono_etapa(data,[mach],[alpha],[beta],p_y_T=[[pressure,temperature]],motor_on=motor_on,aerofreno=True)
        self.escribir_for005(texto_for005)
        self.ejecutar_datcom()
        for004_info = self.leer_for004()
        coeficientes = self.definir_diccionario_coeficientes_rocketpy(for004_info,alpha,beta)

        return coeficientes


    def unir_csvs_flight_data(self,diccionario_csvs,guardado_csv):
        """ Esta función sirve para unir los csv exportados de RocketPy
        con los resultados de la simulación cuando se utiliza el mé-
        todo de coeficientes o en el vuelo de un cohete doble etapa.
        -----------------------------------------------------------
        diccionario_csvs: diccionario
        En las claves del diccionario debe constar el nombre del vue-
        lo y en los valores la ubicación del archivo csv con los dat-
        os. Los vuelos deben estar ordenados en orden cronológico.
        Ej.: {'ORIGIN':'DATA/ORIGIN.csv','Sustainer':'DATA/Sustainer.csv'}
        guardado_csv: string
        Ubicación y nombre donde guardar del csv resultante.
        """

        # Se leen los archivos csv originales
        with open(guardado_csv, mode='w', newline='') as csv_completo:
            escritor = csv.writer(csv_completo)
            for vuelo,ubicacion_csvs in diccionario_csvs.items():
                posicion_clave = list(diccionario_csvs.keys()).index(vuelo)
                with open(ubicacion_csvs,'r') as archivo_csv:
                    lector = csv.reader(archivo_csv)
                    encabezado = next(lector)
                    if posicion_clave == 0:
                        escritor.writerow(encabezado)
                    for linea in lector:
                        escritor.writerow(linea + [vuelo])


class LongitudExcedidaError(Exception):
    """ Error que salta cuando una línea de algún archivo supera la longitud permitida."""
    pass
