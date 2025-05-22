# MARCIM-WG

Elaborado por: Diego Edison Cabuya Padilla

# 1. Resumen

Este software implementa en NetLogo una adaptación del software SERDUX-MARCIM para su uso en un juego de guerra de ciberdefensa marítima llamado MARCIM-WG. El software simula la propagación de ciberataques en infraestructuras marítimas. Permite configurar escenarios estratégicos para juegos de guerra, parametrizando la red, capacidades del objetivo y atacante, características del ataque y los parámetros del modelo, así como visualizar los resultados mediante simulación computacional.

# 2. Descripción general

El software denominado MARCIM-WG (MARCIM Wargame) es un programa computacional desarrollado en el entorno de simulación NetLogo (Wilensky, 2016), que implementa una adaptación operativa y funcional del modelo matemático-computacional SERDUX-MARCIM (Cabuya-Padilla et al., 2025).

El modelo base, SERDUX-MARCIM (Cabuya-Padilla et al., 2025), fue desarrollado por Diego Edison Cabuya Padilla como parte del proyecto de investigación “MARCIM: Marco de referencia para el modelamiento y simulación de la ciberdefensa marítima a nivel estratégico” (D. E. Cabuya-Padilla & Castaneda-Marroquin, 2024), y está registrado ante la Dirección Nacional de Derechos de Autor (2025). Este modelo fue formulado y validado académicamente en el artículo titulado “SERDUX-MARCIM: Maritime Cyberattack Simulation Using Dynamic Modeling, Compartmental Models in Epidemiology and Agent-Based Modeling”, publicado en el International Journal of Information Security (Cabuya-Padilla et al., 2025).

MARCIM-WG tiene como propósito servir como módulo adjudicador para la ejecución del juego de guerra de ciberdefensa marítima, permitiendo al usuario simular la propagación de un ciberataque en redes de infraestructura marítima crítica bajo distintas condiciones operativas. El software reproduce dinámicas de ataque y defensa mediante un sistema de ecuaciones diferenciales dependientes del tiempo, evaluando el comportamiento de los nodos de la red bajo seis posibles estados definidos en el modelo SERDUX: Susceptible (S), Expuesto (E), Resistente (R), Degradado (D), No disponible (U) y Destruido (X).

El programa fue desarrollado con fines educativos, analíticos y estratégicos, y permite la parametrización detallada de escenarios mediante el ajuste de variables relacionadas con:

* la topología de la red objetivo (número de nodos y conexiones),
* las capacidades de ciberseguridad, ciberdefensa, y soporte y sostenibilidad del objetivo.
* los controles de seguridad del objetivo (correctivos, preventivos, disuasorios, compensatorios y detectivos), 
* las capacidades del atacante (factores del atacante y factores de vulnerabilidad), y
* las tasas y parámetros técnicos del sistema de ecuaciones diferenciales.

La ejecución del modelo se realiza desde NetLogo (Wilensky, 2016), integrando módulos escritos en lenguaje Python (Python Software Foundation, 2023), lo cual permite resolver el sistema de ecuaciones y visualizar dinámicamente la evolución del ataque, el comportamiento de los nodos, los niveles de servicio, y los indicadores derivados del enfoque Cyber Risk Assessment (Evaluación del Riesgo Cibernético) propuesto en el artículo original.

El software forma parte integral de la metodología del juego de guerra MARCIM-WG, en el cual un jugador (actor estratégico) toma decisiones frente a un escenario de crisis cibernética simulada y observa los resultados computacionales derivados de sus decisiones estratégicas. Este entorno facilita el entrenamiento en ciberdefensa marítima y el análisis de respuesta ante incidentes complejos bajo condiciones de incertidumbre.

# 3. Descripción de la interfaz de usuario

La interfaz de usuario del software MARCIM-WG ha sido diseñada para facilitar la interacción operativa entre el adjudicador, el modelo computacional y el participante del juego de guerra. Se encuentra estructurada en cuatro secciones funcionales principales:

## 3.1. Panel de control del adjudicador

En esta sección, el adjudicador configura todos los parámetros necesarios para ejecutar el modelo SERDUX-MARCIM conforme a las condiciones establecidas por el diseño del juego. Este panel está dividido en las siguientes subsecciones:

* SISTEMA DE ECUACIONES DIFERENCIALES: configuración y control de los valores de las tasas y parámetros iniciales para el sistema de ecuaciones diferenciales.
* CONFIGURACIÓN INICIAL RED: configuración del número inicial de nodos en los estados Susceptible (S), Expuesto (E), Degradado (D) y No Disponible (U), junto con el número promedio de conexiones entre nodos.
* ATACANTE: Configuración de los factores de atacante y vulnerabilidad.
* CIBERATAQUE: Configuración del grado y duración inicial del ciberataque.
* CAPACIDADES - OBJETIVO: configuración de las capacidades del objetivo en términos de ciberdefensa, ciberinteligencia y, soporte y sostenibilidad.
* CONTROLES DE SEGUIRDAD - OBJETIVO: configuración de los controles de seguridad del objetivo: compensatorios, disuasorios, detectivos, preventivos, y correctivos
* SISTEMA DE ECUACIONES DIFERENCIALES: permite configurar las tasas de transición y parámetros técnicos requeridos para la solución del sistema diferencial que gobierna el comportamiento dinámico del modelo.
* CONFIGURACIÓN INICIAL RED: define el número inicial de nodos en los estados Susceptible (S), Expuesto (E), Degradado (D) y No Disponible (U), así como el número promedio de conexiones entre nodos dentro de la red objetivo.
* ATACANTE: establece los factores asociados al atacante y las condiciones de vulnerabilidad del objetivo.
* CIBERATAQUE: permite definir el grado (Ψ) y duración (δ) del ciberataque, parámetros fundamentales en la evolución del modelo.
* CAPACIDADES - OBJETIVO: configura los niveles de capacidad en tres dominios principales del objetivo: Ciberdefensa (Target Cyberdefense Capability - TCD), Ciberinteligencia (Target Cyberintelligence Capability - TCI), y Soporte y sostenibilidad (Target Support and Sustainability - TSS).
* CONTROLES DE SEGURIDAD - OBJETIVO: permite configurar la presencia y nivel de cinco tipos de controles: correctivos, preventivos, detectivos, disuasorios y compensatorios, todos los cuales influyen directamente en la capacidad del objetivo para resistir, responder o mitigar un ciberataque.

## 3.2. Controles de la simulación

Esta sección agrupa los controles que permiten la ejecución operativa del modelo. Incluye los siguientes botones y campos:

* CONDICIONES INICIALES: carga una parametrización por defecto adecuada para iniciar una nueva simulación.
* CONFIGURACIÓN: registra los valores establecidos en el PANEL DE CONTROL DEL ADJUDICADOR y configura los módulos de ejecución del software. En la primera ejecución, inicializa el entorno de Python en segundo plano.
* EJECUTAR (POR PASOS): lanza la simulación por un número determinado de pasos, definidos por el campo “pasos-sim”. La ejecución se detiene cuando se cumpla al menos una de las condiciones de parada predefinidas:1) Todos los nodos están en estado S, E o R; 2) Todos los nodos están en estado D, U o X.
* pasos-sim: define el número de pasos por ejecutar en la simulación.
* Ronda: registra el número de ronda ejecutada en el marco del juego de guerra.

## 3.3. Escenario predefinido para el juego de guerra

Esta sección permite cargar configuraciones específicas del modelo que representan situaciones prediseñadas por el Director del Juego. Está compuesta por dos bloques funcionales:

### VALORES PRE-DEF (Valores predefinidos)

* Incluye cinco botones que, al ser activados, cargan en el PANEL DE CONTROL DEL ADJUDICADOR valores determinados por la narrativa y estructura del escenario diseñado para el juego de guerra. Cada botón (Ronda #) se utiliza al inicio de una ronda particular y debe ser seguido de la ejecución del botón EJECUTAR (POR PASOS) para que la simulación procese dicha configuración y genere los resultados correspondientes.

### ACCIONES ESPECIALES

Este bloque contiene dos botones que representan eventos especiales dentro de la narrativa del juego de guerra:

* Ofensiva – Ciberataque: al activarse, modifica automáticamente ciertos parámetros del modelo, incrementando la agresividad del atacante o introduciendo condiciones críticas adicionales para el objetivo.
* Pago del Ransomware: simula la decisión estratégica del objetivo de pagar un rescate ante un ciberataque tipo ransomware, lo cual altera determinados valores defensivos o de sostenibilidad de la red.

Ambas acciones tienen un impacto directo en la dinámica del modelo y deben ser seguidas de la ejecución de la simulación mediante el botón EJECUTAR (POR PASOS).

#### Indicadores de activación

* Ciberataque: variable que indica si la acción de ofensiva cibernética ha sido activada (valores: 0 = no activado, 1 o 2 = activado una o más veces).
* Pago Ransom: variable que refleja si se ha activado la acción de pago del ransomware (valores: 0 = no activado, 1 = activado).

## 3.4. Panel de visualización del jugador

Este panel proporciona al participante del juego una representación gráfica y numérica del estado de la simulación, permitiendo monitorear la evolución del ciberataque y evaluar los efectos de sus decisiones. Está compuesto por las siguientes subsecciones:

* CIBER-RIESGO: muestra los porcentajes y resultados de la evaluación de riesgo cibernético conforme al enfoque propuesto en SERDUX-MARCIM, incluyendo las variables de probabilidad, impacto y gravedad del riesgo.
* ESTADO DE LA RED: presenta el número total de nodos en la red y su distribución actual entre los seis estados del modelo SERDUX (S, E, R, D, U, X), con visualización gráfica en el tiempo.
* NIVEL DE SERVICIOS: grafica la proporción de nodos activos (estados S, E, R) frente a los inactivos (estados D, U, X), y su evolución a lo largo de la simulación.
* ATACANTE: permite visualizar los valores actuales de los factores del atacante y de vulnerabilidad.
* CIBERATAQUE: muestra los valores actuales de grado y duración del ciberataque en curso.
* CONTROL Y VISUALIZACIÓN DE LA RED DEL OBJETIVO (SERDUX): ofrece una representación gráfica dinámica de la red de nodos y sus interconexiones, permitiendo observar el cambio de estado individual de cada nodo a lo largo del tiempo, con códigos de color diferenciados por estado.

# 4. Valores predefinidos para el juego de guerra

El software MARCIM-WG incluye un escenario base diseñado para su ejecución dentro de la dinámica del juego de guerra de ciberdefensa marítima. Este escenario base está dividido en cinco rondas secuenciales. Cada una permite observar la evolución de un ciberataque simulado en función de las decisiones tomadas por los jugadores.

Para ejecutar el escenario completo, el adjudicador debe seguir los siguientes pasos metodológicos:

## Ronda 1
1. Presionar el botón CONDICIONES INICIALES.
2. Presionar el botón CONFIGURACIÓN para registrar los valores iniciales del PANEL DE CONTROL DEL ADJUDICADOR.
3. Modificar los valores correspondientes en la sección PANEL DE CONTROL DEL ADJUDICADOR, conforme a la narrativa y fricción establecida por el Director del Juego.
4. Seleccionar Ronda 1.
5. Presionar EJECUTAR (POR PASOS) para correr la simulación correspondiente.
6. Analizar los resultados en el PANEL DE VISUALIZACIÓN DEL JUGADOR.

## Rondas 2 a 5
1. Modificar los valores correspondientes en el PANEL DE CONTROL DEL ADJUDICADOR, reflejando la evolución del escenario de acuerdo con la narrativa diseñada.
2. Seleccionar el número de ronda correspondiente (Ronda 2, Ronda 3, etc.).
3. Presionar EJECUTAR (POR PASOS) para procesar los nuevos parámetros e iniciar la simulación de la ronda.
4. Visualizar y analizar los resultados en el PANEL DE VISUALIZACIÓN DEL JUGADOR.

Este procedimiento se repite sucesivamente hasta completar la ejecución de la Ronda 5, que marca el cierre del escenario predefinido. Cada ronda simula una fase distinta del ciberataque, con eventos y configuraciones variables, permitiendo a los participantes adaptar sus decisiones y estrategias en función del comportamiento emergente del sistema.

# 5. Uso sin escenario pre-definido

El software MARCIM-WG también permite su uso en modalidad libre, es decir, sin seguir la estructura del escenario predefinido de juego de guerra. Esta modalidad resulta útil para fines de experimentación, validación del modelo, análisis de sensibilidad o personalización de nuevos escenarios por parte del adjudicador o de investigadores.

El procedimiento para ejecutar la simulación en modalidad libre es el siguiente:

1. Establecer valores manuales: el usuario configura directamente las variables y parámetros requeridos en la sección PANEL DE CONTROL DEL ADJUDICADOR. Esto incluye la topología de la red, los estados iniciales de los nodos, las capacidades del objetivo, las características del atacante, y los parámetros del ciberataque y del sistema de ecuaciones.
2. Presionar el botón CONFIGURACIÓN: esta acción guarda los valores ingresados y prepara el entorno computacional para la ejecución de la simulación, incluyendo la inicialización del módulo Python si es la primera ejecución.
3. Ejecutar la simulación: el usuario presiona el botón EJECUTAR (POR PASOS) para iniciar la simulación, cuyo número de pasos está determinado por el valor ingresado en el campo “pasos-sim”. La simulación se detiene automáticamente si se cumple alguna de las condiciones de parada definidas por el modelo.
4. Análisis de resultados: los resultados de la simulación se visualizan automáticamente en el PANEL DE VISUALIZACIÓN DEL JUGADOR, incluyendo el estado de la red, el nivel de servicios, la evaluación de riesgo cibernético y la evolución de los nodos en tiempo real.

Esta modalidad ofrece total flexibilidad al usuario, permitiéndole construir sus propios escenarios, explorar distintos comportamientos del modelo SERDUX-MARCIM y adaptar el entorno de simulación a diferentes propósitos analíticos o académicos.

# 6. Funcionamiento del software MARCIM-WG

El software MARCIM-WG opera como un sistema de simulación híbrido que adapta el modelo matemático computacional SERDUX-MARCIM, implementado originalmente en Matlab y NetLogo, para su integración en ejercicios de juegos de guerra. La lógica interna del software responde a un flujo estructurado en siete etapas funcionales que permiten transformar decisiones estratégicas en resultados simulados, mediante la resolución computacional de ecuaciones diferenciales y el modelado basado en agentes.

A continuación, se describe el flujo de ejecución del programa siguiendo la estructura del código de programación:

## INITIAL VALUES AND SETUP
Captura y almacena las variables definidas por el usuario en la sección SIMULATION CONFIGURATION, incluyendo parámetros del sistema de ecuaciones diferenciales, configuración de red, capacidades del objetivo y características del ciberataque.
## NETWORK CREATION
Genera la red objetivo con sus nodos nin_ini, asignando a cada uno un estado inicial del modelo SERDUX (S, E, R, D, U, X) y estableciendo las conexiones entre nodos conforme a los parámetros definidos. Esta red representa la infraestructura digital del actor marítimo simulado.
## CYBER RISK ASSESSMENT
Calcula los indicadores de riesgo cibernético con base en la metodología propuesta en el modelo SERDUX-MARCIM, incluyendo:

* Gravedad del ciber-riesgo,
* Probabilidad de ocurrencia, y
* Impacto estimado sobre el objetivo.

## PYTHON CONFIGURATION
Inicializa el entorno Python en segundo plano y carga los módulos requeridos para la resolución del sistema matemático. Este paso se ejecuta únicamente en la primera simulación o si se reinicia el entorno.
## SYSTEM OF DIFFERENTIAL EQUATIONS SOLUTION
Ejecuta las funciones programadas en Python para resolver el sistema de ecuaciones diferenciales que modela la evolución temporal de los estados de los nodos frente al ciberataque. Esta solución constituye la base cuantitativa para la adjudicación de resultados.
## NODE DISTRIBUTION BETWEEN SERDUX STATES
Determina la redistribución de los nodos entre los diferentes estados del modelo (S, E, R, D, U, X) conforme a los resultados computacionales obtenidos. Esta redistribución refleja el progreso del ciberataque y los efectos de las decisiones tomadas por el jugador.
## NODES ACTUALIZATION AND SIMULATION RESULTS
Actualiza el estado de los nodos en tiempo real y muestra los resultados en la interfaz gráfica del software. El usuario puede observar:

* El comportamiento dinámico de la red,
* Los niveles de servicio,
* Los estados individuales de los nodos,
* Las gráficas de evolución temporal, y
* El resumen de variables del riesgo cibernético.

Este flujo de funcionamiento garantiza que las decisiones estratégicas tomadas en el contexto del juego de guerra tengan efectos cuantificables, reproducibles y visualizables, permitiendo así una experiencia rigurosa de aprendizaje y análisis.

# 7. Requisitos previos

Para ejecutar correctamente el software MARCIM-WG, se deben cumplir ciertos requisitos técnicos previos relacionados con el entorno de simulación, los lenguajes de programación utilizados y las dependencias computacionales. A continuación, se detallan los elementos necesarios:

## 7.1. Entorno de simulación

### NetLogo:
* Versión recomendada: 6.3.0 o superior
* Fuente oficial: https://ccl.northwestern.edu/netlogo/
* NetLogo es el entorno principal desde el cual se ejecuta la interfaz gráfica y el control general del modelo.

### Python:
  * Versión recomendada: 3.10 o superior
  * Fuente oficial: https://www.python.org/
  * Python se utiliza como entorno de cálculo numérico para la resolución del sistema de ecuaciones diferenciales del modelo SERDUX-MARCIM.

## 7.2. Librerías de Python requeridas

Se deben instalar las siguientes librerías mediante el gestor de paquetes pip o un entorno virtual apropiado:

* math  
* sys  
* os  
* sympy  
* numpy  
* scipy  
* matplotlib  
* re  
* collections  
* pandas

Estas librerías permiten realizar operaciones matemáticas simbólicas y numéricas, resolver ecuaciones diferenciales, graficar resultados y gestionar estructuras de datos del modelo.

## 7.3. Configuración de la ruta de Python en NetLogo

Para habilitar la interoperabilidad entre NetLogo y Python, es necesario configurar correctamente la ruta del ejecutable de Python en la interfaz de NetLogo. Esto se realiza desde:

* NetLogo > menú Tools > Preferences > sección Extensions  
* Allí se debe ingresar la ruta absoluta del ejecutable de Python (por ejemplo: C:\Users\usuario\AppData\Local\Programs\Python\Python310\python.exe)

Este paso es obligatorio para que el software pueda utilizar la extensión python de NetLogo y ejecutar los módulos necesarios para el modelo matemático.

# 8. Modelos relacionados

El software MARCIM-WG ha sido desarrollado sobre la base del modelo computacional SERDUX-MARCIM, el cual, a su vez, se fundamenta en enfoques híbridos de simulación que integran modelos compartimentales de epidemiología, modelado dinámico y modelado basado en agentes. A continuación, se relacionan los modelos conceptuales y computacionales directamente relacionados con esta implementación:

* SERDUX-MARCIM  
* SIR  
* SEIR  
* SEIRS  
* MalSEIRS  
* Virus  
* Disease  
* Preferential Attachment  
* Diffusion on a Directed Network  
* NetLogo Virus on a Network model

# 9. Referencias

Cabuya-Padilla, D., Díaz-López, D., Martínez-Páez, J., Hernández, L., & Castaneda-Marroquín, C. (2025). *SERDUX-MARCIM: Maritime cyberattack simulation using dynamic modeling, compartmental models in epidemiology and agent-based modeling*. International Journal of Information Security. https://doi.org/10.1007/s10207-025-00985-6

Cabuya-Padilla, D. E., & Castaneda-Marroquin, C. A. (2024). *Marco de referencia para el modelamiento y simulación de la ciberdefensa marítima - MARCIM: Estado del arte y metodología*. DYNA, 91(231), 169–179. https://doi.org/10.15446/dyna.v91n231.109774

Python Software Foundation. (2023). *Python*. https://www.python.org/

Wilensky, U. (2016). *NetLogo*. Center for Connected Learning and Computer-Based Modeling, Northwestern University. https://ccl.northwestern.edu/netlogo/
