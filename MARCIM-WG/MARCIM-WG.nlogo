;;EXTENSIONES Y VARIABLES GLOBALES
extensions [ py ] ; Llama a las extensiones usadas en el programa

globals ; Crea las variables globales a utilizar en los procesos
[
  ;SERDUX Estados y Red (Real en nodos - Jugador inicial - Calculado inicial - En Python)
  S-r S-i S0 S-py ;Susceptibles
  E-r E-i E0 E-py ;Expuestos
  R-r R-i R0 R-py ;Resistentes
  D-r D-i D0 D-py ;Degradados
  U-r U-i U0 U-py ;No-Disponibles
  X-r X-i X0 X-py ;Destruidos

  N; Número total de nodos

  ;Tasas (Generales - Iniciales - En Python)
  PRO PRO0 PRO-py; Propagación (beta)
  CAD CAD-py; Ciberataque - Degradación (sigma)
  CAU CAU-py; Ciberataque - No Disponible (nabla)
  CAX CAX-py; Ciberataque - Destrucción (chi)
  REC REC-py; Recuperación (gamma)
  SAN SAN-py; Sanitización (phi)
  LOR LOR-py; Pérdida de Resistencia (omega)
  UOC UOC-py; No-Disponibilidad provocada por otras causas (mu)

  ;Enfoque de riesgo cibernético
  R Rasse; Gravedad del ciberataque
  L Lasse; Probabilidad del ciberataque
  I Iasse; Impacto del ciberataque
  ALI; Probabilidad del atacante
  TLI; Probabilidad del objetivo
  TNT; Tráfico de red del objetivo (theta)
  nl; Número de conexiones de nodos

  ; Atacante
  ATF; Factores del atacante
  VUF; Factores de vulnerabilidad

  ; Capacidades del Objetivo
  TCD; Capacidad de Ciberdefensa del Objetivo
  TCI; Capacidad de Ciberinteligencia del Objetivo
  TSS; Capacidad de Soporte y Sostenibilidad del Objetivo

  ;Controles de Seguridad del Objetivo
  CCM; Controles compensatorios (Omega)
  CDE; Controles disuasorios (alpha)
  CPR; Controles preventivos (upsilon)
  CCR; Controles correctivos (eta)
  CDV; Controles detectivos (zeta)
  LRC; Contrloles de reducción de la probabilidad(iota)
  IRC; Controles de reducción del impacto (Gamma)

  ;Ciberataque
  ADE; Grado del ciberataque (Psi)
  TIM;Impacto técnico
  BIM;Impacto al negocio
  ADU;Duración del ciberataque

  ;Sistema de Ecuaciones Diferenciales
  xi; Parámetro para la tasa de propagación de ciberataques (xi)
  fi; Fase inicial de la función en la ejecución de un ciberataque: Degradado y destruido
  ff; Fase final de la función en la ejecución de un ciberataque: Degradado y destruido
  tf; Tiempo final para controlar las funciones en la ejecución de un ciberataque: Degradado y destruido
  wi; Ancho del intervalo activo en la ejecución de un ciberataque: Degradado y destruido
  a; Intensidad de la amortiguación del coseno en la tasa de pérdida de resistencia
  m; Longitud del periodo del coseno en la tasa de pérdida de resistencia

  ;Tiempo
  t

  ;Distribución de Estados
  dis

  ;Control
  change-parameter?
  solcontrol ; Para controlar la solución de la distribución
  sc ec rc dc uc xc solc; Variables para corregir la solución de la distribución en los nodos
  sd ed rd dd ud xd
  setup-control
  control_sim
  control_cyber_attack
  control_ransomware_payment
  CAF
]
turtles-own [ serdux-state links-number ]

;OBSERVACIÓN:El código a continuación mantiene la estructura origina y comentarios del software SERDUX-MARCIM y su modificación fue una reducción y eliminación de los módulos no requeridos para la ejecución del juego de guerra MARCIM-WG
;CONFIGURACIÓN---------------------------------------------------
to setup
  clear-all

  ; Variables storage and initialization
  rate-and-parameter-setup

  set t 0

  ;Creates the nodes in the graph (figure-color-size-location)
  set S-i susceptibles-inicial
  set E-i expuestos-inicial
  set D-i degradados-inicial
  set U-i no-disponibles-inicial
  set N (S-i + E-i + D-i + U-i)

  set-default-shape turtles "computer server"
  create-turtles S-i
  [
    setxy (random-xcor * 0.98) (random-ycor * 0.98)
    set serdux-state "S"
    set color blue
  ]
  create-turtles E-i
  [
    setxy (random-xcor * 0.98) (random-ycor * 0.98)
    set serdux-state "E"
    set color orange
  ]
  create-turtles D-i
  [
    setxy (random-xcor * 0.98) (random-ycor * 0.98)
    set serdux-state "D"
    set color violet
  ]
  create-turtles U-i
  [
    setxy (random-xcor * 0.98) (random-ycor * 0.98)
    set serdux-state "U"
    set color red
  ]


  ask turtles [ set size 2]
  ask turtles [ set links-number 0]

  ; Links creation depending of chooser: random or fixed
  setup-spatially-clustered-network-fixed
  ask links [ set color gray - 1]

  serdux-and-rates-initial-states

  ;; Count the actual turtles by state in graphic and safe the value in X-r
  serdux-actual-state

  ;Cyber Risk Approach initial calculation and assessment
  cyber-risk-approach-calculation

  ;Python initalization variables
  python-state-update
  python-rate-parameter-update

  set change-parameter? false
  set control_sim 1
  set control_cyber_attack 0
  set control_ransomware_payment 0
  reset-ticks

end

;EXECUTION---------------------------------------------------------------------------------------------------------------
to go

  ; Routine when the experimenter change the parameters during the simulation
  ifelse change-parameter? = true
  [
    rate-and-parameter-setup
    set t 0
    serdux-and-rates-initial-states
    serdux-actual-state
    python-state-update
    python-rate-parameter-update
  ]
  ; Routine when the experimenter DON'T change the parameters during the simulation
  ; Normal Routine
  [
    set t t + 1
  ]

  cyber-risk-approach-calculation
  ; Differential equation solver
  python-equation-solver

  serdux-actual-state

  ;SEARUD Changes of states (solution & graph update)
  node-graph-distribution-solution
  if solcontrol = 1 [node-graph-distribution-update]
  if solcontrol = 0 [node-graph-distribution-update-no-solution]

  serdux-actual-state


  ; Simulation stop conditions
  if all? turtles [serdux-state != "D" and serdux-state != "U" and serdux-state != "X"][ stop ]
  if all? turtles [serdux-state != "S" and serdux-state != "E" and serdux-state != "R"][ stop ]

  ; Reestablish the condition
  set change-parameter? false

  tick

end

;; FUNCTIONS AND MODULES-----------------------------------------------------------------------------------------------------

;; VARIABLES IN VISUAL CONTROLS***********************
; Takes the current values of the visual controls and saves them in variables
to rate-and-parameter-setup

  set nl conexiones-entre-nodos

  ;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)
  ; Rates
  set PRO0 tasa-propagacion-inicial-PRO0
  set UOC tasa-no-disponibilidad-otras-causas-UOC

  ;Parameters
  set tf parametro-tiempo-ciberataque-D-tf

  set a stregth-damping-loss-resistance-LOR-a
  set m time-damping-loss-resistance-LOR-m

  ;TARGET Capabilities
  set TCD capacidades-ciberdefensa-obj-TCD
  set TCI capacidades-ciberinteligencia-obj-TCI
  set TSS capacidades-soporte-sostenibilidad-obj-TSS

  ;TARGET Security Controls
  set CCM controles-compensatorios-obj-CCM
  set CDE controles-disuasorios-obj-CDE
  set CPR controles-preventivos-obj-CPR
  set CCR controles-correctivos-obj-CCR
  set CDV controles-detectivos-obj-CDV

  ;ATTACKER LIKELIHOOD
  set ATF factores-atacante-ATF
  set VUF vulnerability-factors-VUF

  ;CYBER ATTACK
  set ADE grado-ciberataque-ADE
  set ADU duracion-ciberataque-ADU
end


;; lINKS CREATION***********************

to setup-spatially-clustered-network-fixed ; Link creation - Fixed links
  ask turtles
  [
    let no-opt 0 ; initialize the local varible to control while overflow due to can't find a node wich has the conditions
    while [ links-number < nl and no-opt = 0 ]
    [
      ; select a node without link with the main node AND with a number of links less than the fixed number
      let choice (one-of other turtles with [(not link-neighbor? myself) and (links-number < nl)])
      ifelse choice != nobody
      [
        create-link-with choice
        ; increments the counter of links in the main link and in the choice link
        set links-number (links-number + 1)
        ask choice [ set links-number (links-number + 1)]
      ]
      [
        set no-opt 1 ; overflow control
      ]
    ]
  ]
end

;; SERDUX-MARCIM INITIAL STATES DEFINITION********************
to serdux-and-rates-initial-states; Store the firs states values to check the integrity of the Equation Solution
  ;States
  set S0 (count turtles with [serdux-state = "S"])
  set E0 (count turtles with [serdux-state = "E"])
  set R0 (count turtles with [serdux-state = "R"])
  set D0 (count turtles with [serdux-state = "D"])
  set U0 (count turtles with [serdux-state = "U"])
  set X0 (count turtles with [serdux-state = "X"])
end

to serdux-actual-state ; Saves in S-r the current values of the nodes in the graph
  set S-r (count turtles with [serdux-state = "S"])
  set E-r (count turtles with [serdux-state = "E"])
  set R-r (count turtles with [serdux-state = "R"])
  set D-r (count turtles with [serdux-state = "D"])
  set U-r (count turtles with [serdux-state = "U"])
  set X-r (count turtles with [serdux-state = "X"])
end


;;CYBER RISK APPROACH***************************************************
; Calculation of the variables related with CYBER RISK ATTACK SEVERITY
to cyber-risk-approach-calculation
  set N (S-r + E-r + R-r + D-r + U-r + X-r)

  set LRC 3 / ( ( 1 / CCM ) + ( 1 / CDE ) + ( 1 / CDV ) )
  set IRC 3 / ( ( 1 / CCR ) + ( 1 / CPR ) + ( 1 / CDV ) )

  set TNT nl / (N - 1)
  if TNT > 0.1 [set TNT 0.1]

  set TLI (0.25 * LRC) + (0.25 * TCI) + (0.25 * TCD) + (0.25 * TSS) - TNT
  set ALI (ATF + VUF) / 2

  set L (ALI - (TLI * 0.1))
  set I (( ADE + ADU ) / 2 ) - ( 0.1 * IRC )

  set R L * I
  if L < 0.3 [set Lasse "Bajo"]
  if L >= 0.3 and L < 0.7 [set Lasse "Medio"]
  if L >= 0.7 [set Lasse "Alto"]

  if I < 0.3 [set Iasse "Bajo"]
  if I >= 0.3 and I < 0.7 [set Iasse "Medio"]
  if I >= 0.7 [set Iasse "Alto"]

  if L < 0.3 and I < 0.3  [set Rasse "Insignificante"]
  if L < 0.3 and I >= 0.3 and I < 0.7 [set Rasse "Bajo"]
  if L < 0.3 and I >= 0.7 [set Rasse "Medio"]

  if L >= 0.3 and L < 0.7 and I < 0.3 [set Rasse "Bajo"]
  if L >= 0.3 and L < 0.7 and I >= 0.3 and I < 0.7 [set Rasse "Medio"]
  if L >= 0.3 and L < 0.7 and I >= 0.7 [set Rasse "Alto"]

  if L >= 0.7 and I < 0.3 [set Rasse "Medio"]
  if L >= 0.7 and I >= 0.3 and I < 0.7 [set Rasse "Alto"]
  if L >= 0.7 and I >= 0.7 [set Rasse "Crítico"]

  if ADE < 0.5 and ADU <= 0.5 [set CAF "Disrupción"]
  if ADE < 0.5 and ADU > 0.5 [set CAF "Degradación"]
  if ADE >= 0.5 and ADU <= 0.5 [set CAF "Denegación"]
  if ADE >= 0.5 and ADU > 0.5 [set CAF "Destrucción"]
end

;; SERDUX TRANSFORMATION PROCESSES ----------------------------------------------------------------------------------
to node-graph-distribution-solution ; Solution to maintain the SERDUX model consistency
;Functions*****************
  (py:run
    "def distribution_validation(distribution):"
    " for value in distribution:"
    "  if value < 0:"
    "   return False"
    "  return True"
  )
  (py:run
    "def allowed_movements():"
    " return [('S', 'E'), ('S', 'U'), ('S', 'R'), ('E', 'R'), ('E', 'D'), ('E', 'U'), ('D', 'R'), ('D', 'X'), ('D', 'U'), ('U', 'X'), ('U', 'R'), ('R', 'S'),  ('R', 'U') ]"
  )
  (py:run
    "def find_solution(initial_distribution, final_distribution):"

    " def id_generation(distribution):"
    "  return tuple(distribution)"

    " def obtain_moved_elements(path):"
    "  moved_elements = set()"
    "  for movement in path:"
    "   moved_elements.add((movement[1], movement[2]))"
    "  return moved_elements"

    " if sum(initial_distribution) != sum(final_distribution):"

    "  return 0"
    ;
    " if not distribution_validation(initial_distribution) or not distribution_validation(final_distribution):"

    "  return 0"
    ;
    " SERDUX_containers = ['S', 'E', 'R', 'D', 'U', 'X']"
    " visited = set()  # Para evitar ciclos infinitos"
    " cola = deque([(initial_distribution, [])])" ;Queue of states (distributions) to explore
    ;
    " while cola:"
    "  actual_distribution, path = cola.popleft()"
    "  actual_id = id_generation(actual_distribution)"
    ;
    "  if actual_id in visited:"
    "   continue"
    ;
    "  visited.add(actual_id)"
    ;
    "  if actual_distribution == final_distribution:"
    "   return path" ;Solution Finded
    ;
    "  moved_elements = obtain_moved_elements(path)"
    ;
    "  for origin, destination in allowed_movements():"
    "   origin_idx = SERDUX_containers.index(origin)"
    "   destination_idx = SERDUX_containers.index(destination)"
    "   movement = min(actual_distribution[origin_idx], final_distribution[destination_idx] - actual_distribution[destination_idx])"
    ;
    "   if (origin, destination) not in moved_elements and movement > 0:"
    "    new_distribution = copy.deepcopy(actual_distribution)"
    "    new_distribution[origin_idx] -= movement"
    "    new_distribution[destination_idx] += movement"
    "    new_path = path + [(movement, origin, destination, new_distribution)]"
    "    cola.append((new_distribution, new_path))"

    " return 0";No solution
  )
  (py:run
    "def show_solution(solution):"
    " if solution:"
    "  print('--------------------------------------------------------------------------------')"
    "  print('initial distribution')"
    "  print(initial_distribution)"
    "  print('final distribution')"
    "  print(final_distribution)"
    "  df = pd.DataFrame(solution, columns=['Node Quantity', 'Origin', 'Destination', 'SERDUX State'])"
    "  df.index.name = 'Movement Number'"
    "  print(df)"
  )
;Variables setup****************************
  py:set "si" S-r
  py:set "ei" E-r
  py:set "ri" R-r
  py:set "di" D-r
  py:set "ui" U-r
  py:set "xi" X-r
  py:set "sd" S-py
  py:set "ed" E-py
  py:set "rd" R-py
  py:set "dd" D-py
  py:set "ud" U-py
  py:set "xd" X-py

 ;Main process*******************
  (py:run
    "solcontrol = 0"
    "initial_distribution = [si, ei, ri, di, ui, xi]"
    "final_distribution = [sd, ed, rd, dd, ud, xd]"
    "solution = find_solution(initial_distribution, final_distribution)"
;    "show_solution(solution)"
  )

;Store the solution in a vector
  (py:run
    "if solution != 0:"
    " vm = [0,0,0,0,0,0,0,0,0,0,0,0,0]"
    " for i in range(len(solution)):"
    "   o=solution[i][1]"
    "   d=solution[i][2]"
    "   m=solution[i][0]"
    "   if   o=='S' and d=='E': vm[0]=m"
    "   elif o=='S' and d=='U': vm[1]=m"
    "   elif o=='S' and d=='R': vm[2]=m"
    "   elif o=='E' and d=='R': vm[3]=m"
    "   elif o=='E' and d=='D': vm[4]=m"
    "   elif o=='E' and d=='U': vm[5]=m"
    "   elif o=='D' and d=='R': vm[6]=m"
    "   elif o=='D' and d=='X': vm[7]=m"
    "   elif o=='D' and d=='U': vm[8]=m"
    "   elif o=='U' and d=='X': vm[9]=m"
    "   elif o=='U' and d=='R': vm[10]=m"
    "   elif o=='R' and d=='S': vm[11]=m"
    "   elif o=='R' and d=='U': vm[12]=m"
    "   solcontrol = 1"

    "if solution == 0:"
    " solcontrol = 0"
   )
  set dis (py:runresult "vm")
  set solcontrol (py:runresult "solcontrol")
  set sd (py:runresult "sd")
  set ed (py:runresult "ed")
  set rd (py:runresult "rd")
  set dd (py:runresult "dd")
  set ud (py:runresult "ud")
  set xd (py:runresult "xd")
  set solc (py:runresult "sum(initial_distribution) - sum(final_distribution)")
end

to node-graph-distribution-update ; Node update in the graphic
  ask n-of item 0 dis (turtles with  [ serdux-state = "S" ])
    [ set serdux-state "E" set color orange ]
  ask n-of item 1 dis (turtles with  [ serdux-state = "S" ])
    [ set serdux-state "U" set color red ]
  ask n-of item 2 dis (turtles with  [ serdux-state = "S" ])
    [ set serdux-state "R" set color green ]
  ask n-of item 3 dis (turtles with  [ serdux-state = "E" ])
    [ set serdux-state "R" set color green ]
  ask n-of item 4 dis (turtles with  [ serdux-state = "E" ])
    [ set serdux-state "D" set color violet ]
  ask n-of item 5 dis (turtles with  [ serdux-state = "E" ])
    [ set serdux-state "U" set color red ]
  ask n-of item 6 dis (turtles with  [ serdux-state = "D" ])
    [ set serdux-state "R" set color green ]
  ask n-of item 7 dis (turtles with  [ serdux-state = "D" ])
    [ set serdux-state "X" set color gray ]
  ask n-of item 8 dis (turtles with  [ serdux-state = "D" ])
    [ set serdux-state "U" set color red ]
  ask n-of item 9 dis (turtles with  [ serdux-state = "U" ])
    [ set serdux-state "X" set color gray  ]
  ask n-of item 10 dis (turtles with  [ serdux-state = "U" ])
    [ set serdux-state "R" set color green ]
  ask n-of item 11 dis (turtles with  [ serdux-state = "R" ])
    [ set serdux-state "S" set color blue ]
  ask n-of item 12 dis (turtles with  [ serdux-state = "R" ])
    [ set serdux-state "U" set color red ]
end

to node-graph-distribution-update-no-solution ; Node update in the graphic when there is an error due to floating number
  serdux-actual-state
  set sc (S-r - sd)
  if sc > 0[
    ask n-of sc (turtles with  [ serdux-state = "S"])
    [ set serdux-state "U" set color red ]
  ]
  if sc < 0[
    ask n-of (- sc) (turtles with  [serdux-state = "E" or serdux-state = "R" or serdux-state = "D" or serdux-state = "U" or serdux-state = "X"])
    [ set serdux-state "S" set color blue ]
  ]
  serdux-actual-state

  set ec (E-r - ed)
  if ec > 0[
    ask n-of ec (turtles with  [ serdux-state = "E"])
    [ set serdux-state "U" set color red ]
  ]
  if ec < 0[
    ask n-of (- ec) (turtles with  [ serdux-state = "R" or serdux-state = "D" or serdux-state = "U" or serdux-state = "X"])
    [ set serdux-state "E" set color orange ]
  ]
  serdux-actual-state

  set rc (R-r - rd)
  if rc > 0[
    ask n-of rc (turtles with  [ serdux-state = "R"])
    [ set serdux-state "U" set color red ]
  ]
  if rc < 0[
    ask n-of (- rc) (turtles with  [serdux-state = "D" or serdux-state = "U" or serdux-state = "X"])
    [ set serdux-state "R" set color green ]
  ]
  serdux-actual-state

  set dc (D-r - dd)
  if dc > 0[
    ask n-of dc (turtles with  [ serdux-state = "D"])
    [ set serdux-state "U" set color red ]
  ]
  if dc < 0[
    ask n-of (- dc) (turtles with  [serdux-state = "U" or serdux-state = "X"])
    [ set serdux-state "D" set color violet ]
  ]
  serdux-actual-state

  set xc (X-r - xd)
  if xc > 0[
  ask n-of xc (turtles with  [ serdux-state = "U"])
  [ set serdux-state "U" set color red ]
  ]
  if xc < 0[
    ask n-of (- xc) (turtles with  [serdux-state = "U"])
    [ set serdux-state "X" set color gray ]
  ]
  serdux-actual-state

end


;;PYTHON SETUP AND EXECUTION ---------------------------------------------------------------------------------------
to python-setup ; Initializes Python and imports libraries
  py:setup
  py:python
  (py:run
    "import math"
    "import sys"
    "import os"
    "import sympy"
    "from sympy import *"
    "import numpy as np"
    "from scipy.integrate import solve_ivp"
    "import matplotlib.pyplot as plt"
    "from re import M"
    "from collections import deque"
    "import copy"
    "import pandas as pd"
  )
end

to python-state-update ;Setup initial variable conditions in Python
  ;;Initial SEARUD values
  py:set "S0" S-r
  py:set "E0" E-r
  py:set "R0" R-r
  py:set "D0" D-r
  py:set "U0" U-r
  py:set "X0" X-r

  set S-py S-r
  set E-py E-r
  set R-py R-r
  set D-py D-r
  set U-py U-r
  set X-py X-r
end

to python-rate-parameter-update
  ;Rates and Parameters
  py:set "PRO0" PRO0
  py:set "L" L
  py:set "ADE" ADE
  py:set "ADU" ADU
  py:set "CCM" CCM
  py:set "CDE" CDE
  py:set "CCR" CCR
  py:set "CPR" CPR
  py:set "CDV" CDV
  py:set "a" a
  py:set "m" m
  py:set "tf" tf
  py:set "UOC" UOC
  py:set "NNO" 0
end

to python-equation-solver ;Solve the system of differential equations in Python
  py:set "simt" t + 20
  (py:run
    "def system(t, variables, PRO0, L, ADE, ADU, CCM, CDE, CCR, CPR, CDV, a, m, tf, UOC, NNO):"
    " S, E, R, D, U, X = variables"

    " N =  S + E + R + D + U + X"
    " LRC = 3 / ((1 / CCM) + (1 / CDE) + (1 / CDV))"
    " IRC = 3 / ((1 / CPR) + (1 / CCR) + (1 / CDV))"
    " xi = 2 / ((1 / L) + (1 / ADE))"
    " PRO = PRO0 / (1 + ((1 - xi)*(D + U + X)))"
    " Theta = ADU * (1 - IRC)"
    " Iphase = ((1 - IRC) * ADE) / (1 + np.exp(-LRC * (t - (tf * Theta) + (tf / 4))))"
    " Fphase = -((1 - IRC) * ADE) / (1 + np.exp(-IRC * (t - (tf * Theta) + (tf / 4) - (tf * Theta))))"
    " CAD = Iphase + Fphase"
    " ConditionU = (ADE >= 0.5) & (ADU <= 0.5)"
    " CAU = np.where(ConditionU, 2 / ((1 / ADE) + (1 / ADU)), 0)"
    " ConditionX = (ADE >= 0.5) & (ADU > 0.5)"
    " CAX = np.where(ConditionX, 1 / ((1 / ADE) + (1 / ADU)), 0)"
    " REC = (CCR / 2) * (np.tanh(((D + U + X) / N) * 3))"
    " SAN = (CPR / 2) * (1 - np.tanh(((S + E) / N) * 3))"
    " LOR = np.abs(np.exp(-a * t) * np.cos((2 * np.pi * t )/ m))"

    ;STATES
    " dSdt = NNO + (LOR * R) - (PRO * S) - (UOC * S) - (SAN * S)"
    " dEdt = (PRO * S) - (CAD * E) - (UOC * E) - (SAN * E)"
    " dRdt = (REC * D) + (REC * U) + (SAN * S) + (SAN * E) - (LOR * R) - (UOC * R)"
    " dDdt = (CAD * E) - (REC * D) - (UOC * D) - (CAU * D) - (CAX * D)"
    " dUdt = (UOC * (S + E + D + R)) + (CAU * D) - (CAX * U) - ( REC * U )"
    " dXdt = (CAX * D) + (CAX * U)"
    ;RETURN
    " return [dSdt, dEdt, dRdt, dDdt, dUdt, dXdt]"
  )
  ; Node SERDUX state initial conditions
  py:run "initial_conditions = [S0, E0, R0, D0, U0, X0]"

  ;; Time span
  py:run "t_span = (0, simt)"

  ;; Solve the system using numerical integration
  ;Numeric Method (RK45): Runge-Kutta, Order 5(4), Dormand-Prince.
  py:run "sol = solve_ivp(system, t_span, initial_conditions, args=(PRO0, L, ADE, ADU, CCM, CDE, CCR, CPR, CDV, a, m, tf, UOC, NNO), dense_output=True)"

  ;; Extract the results
  py:run "t_values = np.linspace(t_span[0], t_span[1], t_span[1])"
  py:run "S, E, R, D, U, X = sol.sol(t_values)"

  ;; SEARUD results at specific time
  py:set "t_eval" t

  ;Retreive rate values
  py:run "PRO, CAD, CAUy, CAXy, REC, SAN, LOR, Iphase, Fphase = [], [], [], [], [], [], [], [], []"
  (py:run
    "for t in t_values:"
    "  s, e, r, d, u, x = sol.sol(t)"
    "  N = s + e + r + d + u + x"
    "  LRC = 3 / ((1 / CCM) + (1 / CDE) + (1 / CDV))"
    "  IRC = 3 / ((1 / CPR) + (1 / CCR) + (1 / CDV))"
    "  xi = 2 / ((1 / L) + (1 / ADE))"
    "  PRO.append(PRO0 / (1 + ((1 - xi)*(d + u + x))))"
    "  Theta = ADU * (1 - IRC)"
    "  Iphase_val = ((1 - IRC) * ADE) / (1 + np.exp(-LRC * (t - (tf * Theta) + (tf / 4))))"
    "  Fphase_val = -((1 - IRC) * ADE) / (1 + np.exp(-IRC * (t - (tf * Theta) + (tf / 4) - (tf * Theta))))"
    "  Iphase.append(Iphase_val)"
    "  Fphase.append(Fphase_val)"
    "  CAD.append(Iphase_val + Fphase_val)"
    "  ConditionU = (ADE >= 0.5) & (ADU <= 0.5)"
    "  CAU = np.where(ConditionU, 2 / ((1 / ADE) + (1 / ADU)), 0)"
    "  if CAU.size > 0:"
    "    CAUy.append(CAU.tolist())"
    "  else:"
    "    CAUy.append([0])"
    "  ConditionX = (ADE >= 0.5) & (ADU > 0.5)"
    "  CAX = np.where(ConditionX, 1 / ((1 / ADE) + (1 / ADU)), 0)"
    "  if CAX.size > 0:"
    "    CAXy.append(CAX.tolist())"
    "  else:"
    "    CAXy.append([0])"
    "  REC.append((CCR / 2) * np.tanh(((d + u + x) / N) * 3))"
    "  SAN.append((CPR / 2) * (1 - np.tanh(((s + e) / N) * 3)))"
    "  LOR.append(np.abs(np.exp(-a * t) * np.cos(2 * np.pi * t / m)))"
  )

  ;Find the closes index to t_eval
  py:run "index_t_eval = np.abs(t_values - t_eval).argmin()"

  ;Keep values in variables and rounded to the nearest integer
  set S-py (py:runresult "np.round(S[index_t_eval])")
  if S-py < 0 [set S-py 0]
  set E-py (py:runresult "np.round(E[index_t_eval])")
  if E-py < 0 [set E-py 0]
  set R-py (py:runresult "np.round(R[index_t_eval])")
  if R-py < 0 [set R-py 0]
  set D-py (py:runresult "np.round(D[index_t_eval])")
  if D-py < 0 [set D-py 0]
  set U-py (py:runresult "np.ceil(U[index_t_eval])")
  if U-py < 0 [set U-py 0]
  set X-py (py:runresult "np.floor(X[index_t_eval])")
  if X-py < 0 [set X-py 0]
  set PRO-py (py:runresult "PRO[index_t_eval]")
  set CAD-py (py:runresult "CAD[index_t_eval]")
  set CAU-py (py:runresult "CAUy[index_t_eval]")
  set CAX-py (py:runresult "CAXy[index_t_eval]")
  set REC-py (py:runresult "REC[index_t_eval]")
  set SAN-py (py:runresult "SAN[index_t_eval]")
  set LOR-py (py:runresult "LOR[index_t_eval]")
  set UOC-py UOC

  ; Correction
  let dif ( S0 + E0 + R0 + D0 + U0 + X0 - S-py - E-py - R-py - D-py - U-py - X-py )
  if dif != 0 [
    set U-py (U-py + dif)
    if U-py < 0[
      set U-py 0
      set R-py (R-py + dif)
      if R-py < 0[
        set R-py 0
      ]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
1803
77
2287
562
-1
-1
9.333333333333334
1
10
1
1
1
0
0
0
1
-25
25
-25
25
0
0
1
ticks
30.0

BUTTON
814
75
977
108
2. CONFIGURACIÓN
if setup-control != 1[python-setup]\nsetup\nset setup-control 1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1379
78
1801
319
Estado de la Red del Objetivo (SERDUX)
Tiempo (t)
Nodos
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Susceptible" 1.0 0 -13345367 true "" "plot S-py"
"Expuesto" 1.0 0 -955883 true "" "plot E-py"
"Resistente" 1.0 0 -10899396 true "" "plot R-py"
"Degradado" 1.0 0 -8630108 true "" "plot D-py"
"No Disponible" 1.0 0 -2674135 true "" "plot U-py"
"Destruido" 1.0 0 -16777216 true "" "plot X-py"

SLIDER
43
495
225
528
conexiones-entre-nodos
conexiones-entre-nodos
2
(susceptibles-inicial + expuestos-inicial + degradados-inicial + no-disponibles-inicial) - 1
2.0
2
1
links
HORIZONTAL

BUTTON
814
116
977
149
3. EJECUTAR (POR PASOS)
repeat (pasos-sim) [go]\nset control_sim (control_sim + 1)
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
814
155
906
200
pasos-sim
pasos-sim
1 6 12 24 48 72 96 120 144 168 336 672
7

SLIDER
264
91
504
124
tasa-propagacion-inicial-PRO0
tasa-propagacion-inicial-PRO0
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
264
131
504
164
tasa-no-disponibilidad-otras-causas-UOC
tasa-no-disponibilidad-otras-causas-UOC
0
0.0025
0.0
0.0005
1
NIL
HORIZONTAL

SLIDER
263
209
504
242
stregth-damping-loss-resistance-LOR-a
stregth-damping-loss-resistance-LOR-a
0.1
20
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
263
249
504
282
time-damping-loss-resistance-LOR-m
time-damping-loss-resistance-LOR-m
0.1
240
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
44
374
224
407
expuestos-inicial
expuestos-inicial
0
1000
0.0
1
1
nodes
HORIZONTAL

SLIDER
44
414
223
447
degradados-inicial
degradados-inicial
0
100
0.0
1
1
nodes
HORIZONTAL

TEXTBOX
33
306
248
324
CONFIGURACIÓN INICIAL RED
14
0.0
1

MONITOR
8
90
113
131
Propagación
PRO-py
3
1
10

MONITOR
8
136
113
177
Recuperación
REC-py
6
1
10

MONITOR
9
236
113
277
Pérdida Resistencia
LOR-py
6
1
10

MONITOR
1217
148
1267
193
S
S-py
0
1
11

MONITOR
1271
148
1321
193
E
E-py
0
1
11

MONITOR
1217
201
1267
246
D
D-py
0
1
11

MONITOR
1324
148
1374
193
R
R-py
0
1
11

TEXTBOX
832
212
960
230
VALORES PRE-DEF
14
0.0
1

MONITOR
1271
201
1321
246
U
U-py
0
1
11

MONITOR
1324
201
1374
246
X
X-py
17
1
11

MONITOR
121
90
255
131
C. Ataque - Degradación
CAD-py
3
1
10

MONITOR
8
186
114
227
Sanitización
SAN-py
6
1
10

MONITOR
121
136
255
177
C. Ataque - No Disponible
CAU-py
3
1
10

MONITOR
120
234
253
275
No Disponibilidad Otras C.
UOC-py
17
1
10

SLIDER
290
494
480
527
duracion-ciberataque-ADU
duracion-ciberataque-ADU
0
1
0.0
0.1
1
NIL
HORIZONTAL

TEXTBOX
349
309
447
344
ATACANTE
14
0.0
1

SLIDER
288
333
479
366
factores-atacante-ATF
factores-atacante-ATF
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
289
375
478
408
vulnerability-factors-VUF
vulnerability-factors-VUF
0
1
0.2
0.1
1
NIL
HORIZONTAL

TEXTBOX
562
65
763
83
CAPACIDADES - OBJETIVO
14
0.0
1

SLIDER
529
172
776
205
capacidades-ciberinteligencia-obj-TCI
capacidades-ciberinteligencia-obj-TCI
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
529
130
777
163
capacidades-soporte-sostenibilidad-obj-TSS
capacidades-soporte-sostenibilidad-obj-TSS
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
121
64
407
83
SISTEMA DE ECUACIONES DIFERENCIALES
14
0.0
1

SLIDER
526
458
772
491
controles-correctivos-obj-CCR
controles-correctivos-obj-CCR
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
526
417
772
450
controles-preventivos-obj-CPR
controles-preventivos-obj-CPR
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
526
294
774
327
controles-compensatorios-obj-CCM
controles-compensatorios-obj-CCM
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
526
335
772
368
controles-disuasorios-obj-CDE
controles-disuasorios-obj-CDE
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
1233
51
1365
70
ESTADO DE LA RED
14
0.0
1

TEXTBOX
1060
397
1166
416
CIBERATAQUE
14
0.0
1

TEXTBOX
528
261
779
279
CONTROLES SEGURIDAD - OBJETIVO\n
14
0.0
1

MONITOR
121
185
254
226
C. Ataque - Destrucción
CAX-py
3
1
10

MONITOR
1000
81
1145
126
Gravedad del Ciber-Riesgo
Rasse
17
1
11

MONITOR
1000
149
1146
194
Probabilidad Ciberataque
Lasse
17
1
11

MONITOR
1000
200
1146
245
Impacto Ciberataque
Iasse
17
1
11

TEXTBOX
1048
53
1156
71
CIBER-RIESGO
14
0.0
1

SLIDER
289
457
480
490
grado-ciberataque-ADE
grado-ciberataque-ADE
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
263
170
505
203
parametro-tiempo-ciberataque-D-tf
parametro-tiempo-ciberataque-D-tf
0
300
0.0
10
1
NIL
HORIZONTAL

BUTTON
814
235
977
268
Ronda 1
if control_sim = 1 [\n;;NETWORK SETUP\nset conexiones-entre-nodos 2\n\n;;SERDUX - INITIAL STATES\nset susceptibles-inicial 300\nset expuestos-inicial 40\nset degradados-inicial 10\nset no-disponibles-inicial 0\n\n;;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)\n;rates\nset tasa-propagacion-inicial-PRO0 0.25\nset tasa-no-disponibilidad-otras-causas-UOC 0.0001\n\nset parametro-tiempo-ciberataque-D-tf 240\n\nset stregth-damping-loss-resistance-LOR-a 0.01\nset time-damping-loss-resistance-LOR-m 120\n\n;;ATTACKER\nset factores-atacante-ATF 0.4\nset vulnerability-factors-VUF 0.4\n  \n;;CYBER ATTACK\nset grado-ciberataque-ADE 0.1\nset duracion-ciberataque-ADU 0.1\n  \nset pasos-sim 120\n  \nset change-parameter? true\n\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
814
272
977
305
Ronda 2
if control_sim = 2 [\n;;NETWORK SETUP\nset conexiones-entre-nodos 2\n\n;;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)\n;rates\nset tasa-propagacion-inicial-PRO0 0.5\nset tasa-no-disponibilidad-otras-causas-UOC 0.0001\n\nset parametro-tiempo-ciberataque-D-tf 240\n\nset stregth-damping-loss-resistance-LOR-a 0.01\nset time-damping-loss-resistance-LOR-m 48\n\n;;ATTACKER\nset factores-atacante-ATF 0.4\nset vulnerability-factors-VUF 0.4\n\n;;CYBER ATTACK\nset grado-ciberataque-ADE 0.4\nset duracion-ciberataque-ADU 0.4\n  \nset pasos-sim 48\n  \nset change-parameter? true\n\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
815
308
978
341
Ronda 3
if control_sim = 3 [\n;;NETWORK SETUP\nset conexiones-entre-nodos 2\n\n;;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)\n;rates\nset tasa-propagacion-inicial-PRO0 1\nset tasa-no-disponibilidad-otras-causas-UOC 0.1\n\nset parametro-tiempo-ciberataque-D-tf 240\n\nset stregth-damping-loss-resistance-LOR-a 0.01\nset time-damping-loss-resistance-LOR-m 12\n\n;;ATTACKER\nset factores-atacante-ATF precision (factores-atacante-ATF + 0.5) 1\nset vulnerability-factors-VUF precision (vulnerability-factors-VUF + 0.5) 1\n  \n;;CYBER ATTACK\nset grado-ciberataque-ADE precision (grado-ciberataque-ADE + 0.6) 1\nset duracion-ciberataque-ADU precision (duracion-ciberataque-ADU + 0.1) 1\n  \nset pasos-sim 12\n  \nset change-parameter? true\n\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
44
455
224
488
no-disponibles-inicial
no-disponibles-inicial
0
100
0.0
1
1
nodes
HORIZONTAL

MONITOR
1152
148
1202
193
% L
L * 100
2
1
11

MONITOR
1151
200
1201
245
% I
I * 100
2
1
11

MONITOR
1029
288
1187
333
% Factores del Atacante
ATF * 100
2
1
11

MONITOR
1030
337
1188
382
% Factores de Vulnerabilidad
VUF * 100
2
1
11

MONITOR
1030
470
1189
515
% Grado del Ciberataque
ADE * 100
2
1
11

MONITOR
1030
517
1190
562
% Duración del Ciberataque
ADU * 100
2
1
11

TEXTBOX
790
10
808
560
|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|
11
0.0
1

TEXTBOX
985
10
1002
555
|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|
11
0.0
1

MONITOR
1234
348
1362
393
Nodos Activos (SER)
S-r + E-r + R-r
0
1
11

MONITOR
1236
465
1363
510
Nodos Inactivos (DUX)
D-r + U-r + X-r
17
1
11

MONITOR
1235
399
1362
444
% Servicios Activos
(S-r + E-r + R-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)
2
1
11

PLOT
1379
321
1802
563
Nivel de Servicios del Objetivo
Tiempo (t)
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Activos" 1.0 0 -13840069 true "" "plot (S-r + E-r + R-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)"
"Inactivos" 1.0 0 -5298144 true "" "plot (D-r + U-r + X-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)"

MONITOR
1239
82
1355
127
Número de Nodos
susceptibles-inicial + expuestos-inicial + degradados-inicial + no-disponibles-inicial
0
1
11

SLIDER
526
376
772
409
controles-detectivos-obj-CDV
controles-detectivos-obj-CDV
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
113
10
748
42
MARCIM-WG PANEL DE CONTROL DEL ADJUDICADOR
26
105.0
1

MONITOR
1236
517
1363
562
% Servicios Inactivos
(D-r + U-r + X-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)
2
1
11

SLIDER
530
92
777
125
capacidades-ciberdefensa-obj-TCD
capacidades-ciberdefensa-obj-TCD
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
1239
320
1376
339
NIVEL SERVICIOS
14
0.0
1

TEXTBOX
860
11
949
30
CONTROLES
14
0.0
1

TEXTBOX
1533
50
2142
68
CONTROL Y VISUALIZACIÓN DEL COMPORTAMIENTO DE LA RED DEL OBJETIVO (SERDUX)\n
14
0.0
1

BUTTON
814
346
978
379
Ronda 4
if control_sim = 4 [\n;;NETWORK SETUP\nset conexiones-entre-nodos 2\n\n;;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)\n;rates\nset tasa-propagacion-inicial-PRO0 1\nset tasa-no-disponibilidad-otras-causas-UOC 0.0025\n\nset parametro-tiempo-ciberataque-D-tf 240\n\nset stregth-damping-loss-resistance-LOR-a 0.01\nset time-damping-loss-resistance-LOR-m 120\n\n;;ATTACKER\nset factores-atacante-ATF precision (factores-atacante-ATF + 0.1) 1\nset vulnerability-factors-VUF precision (vulnerability-factors-VUF + 0.1) 1\n  \n;;CYBER ATTACK\nset duracion-ciberataque-ADU precision (duracion-ciberataque-ADU + 0.3) 1\n  \nset pasos-sim 120\n  \nset change-parameter? true\n\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
814
382
978
415
Ronda 5
if control_sim = 5 [\nset pasos-sim 168\nset change-parameter? true\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
814
442
978
475
Ofensiva - Ciberataque
if control_sim = 4 or control_sim = 5[\nif control_cyber_attack < 2 [\n\n;;ATTACKER\nset factores-atacante-ATF precision (factores-atacante-ATF - 0.2) 1\nif factores-atacante-ATF < 0.1 [set factores-atacante-ATF 0.1 ]\n  \n;;CYBER ATTACK\nset grado-ciberataque-ADE precision (grado-ciberataque-ADE - 0.2) 1\nif grado-ciberataque-ADE < 0.1 [set grado-ciberataque-ADE 0.1 ]\nset duracion-ciberataque-ADU precision (duracion-ciberataque-ADU - 0.1) 1\nif duracion-ciberataque-ADU < 0.1 [set duracion-ciberataque-ADU 0.1 ]\n   \nset change-parameter? true\nset control_cyber_attack (control_cyber_attack + 1)\n]\n]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
814
480
977
513
Pago del Ransomware
if control_sim = 3 or control_sim = 4 or control_sim = 5[\nif control_ransomware_payment = 0 [\n;;ATTACKER\nset factores-atacante-ATF precision (factores-atacante-ATF - 0.3) 1\nif factores-atacante-ATF < 0.1 [set factores-atacante-ATF 0.1 ]\nset vulnerability-factors-VUF precision (vulnerability-factors-VUF - 0.3) 1\nif vulnerability-factors-VUF < 0.1 [set vulnerability-factors-VUF 0.1 ]\n  \n;;CYBER ATTACK\nset grado-ciberataque-ADE precision (grado-ciberataque-ADE - 0.3) 1\nif grado-ciberataque-ADE < 0.1 [set grado-ciberataque-ADE 0.1 ]\nset duracion-ciberataque-ADU precision (duracion-ciberataque-ADU - 0.3) 1\nif duracion-ciberataque-ADU < 0.1 [set duracion-ciberataque-ADU 0.1 ]\n  \nset change-parameter? true\nset control_ransomware_payment 1\n]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
813
37
978
70
1. CONDICIONES INICIALES
;;NETWORK SETUP\nset conexiones-entre-nodos 2\n\n;;SERDUX - INITIAL STATES\nset susceptibles-inicial 350\nset expuestos-inicial 0\nset degradados-inicial 0\nset no-disponibles-inicial 0\n\n;;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)\n;rates\nset tasa-propagacion-inicial-PRO0 0\nset tasa-no-disponibilidad-otras-causas-UOC 0\n\nset parametro-tiempo-ciberataque-D-tf 0\n\nset stregth-damping-loss-resistance-LOR-a 0\nset time-damping-loss-resistance-LOR-m 0\n\n;;TARGET\n;Target Capabilities\n  SET capacidades-ciberdefensa-obj-TCD 0.1\n  set capacidades-ciberinteligencia-obj-TCI 0.1\n  set capacidades-soporte-sostenibilidad-obj-TSS 0.1\n  \n;Target Security Controls\n  set controles-compensatorios-obj-CCM 0.1\n  set controles-disuasorios-obj-CDE 0.1\n  set controles-preventivos-obj-CPR 0.1\n  set controles-correctivos-obj-CCR 0.1\n  set controles-detectivos-obj-CDV 0.1\n\n\n;;ATTACKER\n  set factores-atacante-ATF 0.2\n  set vulnerability-factors-VUF 0.2\n  \n;;CYBER ATTACK\n  set grado-ciberataque-ADE 0\n  set duracion-ciberataque-ADU 0\n  \n  set pasos-sim 120\n  \n  set change-parameter? true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
908
155
979
200
Ronda
control_sim - 1
17
1
11

MONITOR
898
517
978
562
Pago Ransom
control_ransomware_payment
17
1
11

MONITOR
815
517
895
562
Ciberataque
control_cyber_attack
17
1
11

MONITOR
1030
422
1189
467
Efecto del Ciberataque
CAF
17
1
11

MONITOR
1151
79
1201
124
% R
R * 100
2
1
11

TEXTBOX
337
428
436
446
CIBERATAQUE
14
0.0
1

TEXTBOX
818
422
979
440
ACCIONES ESPECIALES
14
0.0
1

TEXTBOX
1071
262
1146
280
ATACANTE
14
0.0
1

TEXTBOX
1292
10
1987
39
MARCIM-WG PANEL DE VISUALIZACIÓN DEL JUGADOR
26
115.0
1

SLIDER
44
334
223
367
susceptibles-inicial
susceptibles-inicial
0
10000
350.0
1
1
nodes
HORIZONTAL

@#$#@#$#@
# DESCRIPCIÓN DEL PROGRAMA MARCIM-WG

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
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

computer server
false
0
Rectangle -7500403 true true 75 30 225 270
Line -16777216 false 210 30 210 195
Line -16777216 false 90 30 90 195
Line -16777216 false 90 195 210 195
Rectangle -10899396 true false 184 34 200 40
Rectangle -10899396 true false 184 47 200 53
Rectangle -10899396 true false 184 63 200 69
Line -16777216 false 90 210 90 255
Line -16777216 false 105 210 105 255
Line -16777216 false 120 210 120 255
Line -16777216 false 135 210 135 255
Line -16777216 false 165 210 165 255
Line -16777216 false 180 210 180 255
Line -16777216 false 195 210 195 255
Line -16777216 false 210 210 210 255
Rectangle -7500403 true true 84 232 219 236
Rectangle -16777216 false false 101 172 112 184

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
