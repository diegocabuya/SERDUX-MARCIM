;;EXTENSIONS AND GLOBAL VARIABLES-----------------------------------------------------------------------------------------------------------

extensions [ py ] ; Call the extensions used in the program

globals ; Creates the global variables to use in the processes
[
  ;SERDUX States - Network (Real in Nodes - Initial experimenter - Initial calculated - In python)
  S-r S-i S0 S-py ;Susceptible
  E-r E-i E0 E-py ;Exposed
  R-r     R0 R-py ;Resistant
  D-r D-i D0 D-py ;Degraded
  U-r U-i U0 U-py ;Unavailable
  X-r     X0 X-py ;Destroyed

  N; Total Number of Nodes

  ;Rates (General - Initial - In python)
  PRO PRO0 PRO-py; Propagation (beta)
  CAD CAD-py; Cyber Attack - Degraded (sigma)
  CAU CAU-py; Cyber Attack - Unavailable (nabla)
  CAX CAX-py; Cyber Attack - Destroyed (chi)
  REC REC-py; Recovery (gamma)
  SAN SAN-py; Sanitation (phi)
  LOR LOR-py; Loss of Resistance (omega)
  UOC UOC-py; Node Unavailability Provoked by Other Causes (mu)

  ;Cyber Risk Approach
  R Rasse; Risk Cyber Attack Severity
  L Lasse; Cyber Attack Likelihood
  I Iasse; Cyber Attack Impact
  ALI; Attacker Likelihood
  TLI; Target Likelihood
  TNT; Target's Network Traffick (theta)
  nl; Number of Nodes Conexions

  ; Attacker
  ATF; Attacker Factors
  VUF; Vulnerability Factors

  ; Target Capabilities
  TCD; Target Cyberdefense Capability
  TCI; Target Cyberintelligence Capability
  TSS; Target Support and Sustainability Capability

  ;Target Security Controls
  CCM; Compensatory Controls (Omega)
  CDE; Deterrent Controls (alpha)
  CPR; Preventive Controls (upsilon)
  CCR; Corrective Controls (eta)
  CDV; Detective Controls (zeta)
  LRC; Likelihood Reduction Controls (iota)
  IRC; Impact Reduction Controls (Gamma)

  ;Cyber Attack
  ADE; Cyber Attack Degree (Psi)
  TIM;Technical Impact
  BIM;Business Impact
  ADU;Cyber Attack Duration

  ;System of Differential Equations
  xi; Parameter for Cyber Attack Propagation Rate (xi)
  fi; Initial function phase in Cyber Attack Execution - Degraded & Destroyed
  ff; Final function phase in Cyber Attack Execution - Degraded & Destroyed
  tf; Final tima to control functions in Cyber Attack Execution - Degraded & Destroyed
  wi; width of the active interval in Cyber Attack Execution - Degraded & Destroyed
  a; Strength of the cosine damping in Loss of Resistance Rate
  m; Length of the cosine period in Loss of Resistance Rate

  ;Time
  t

  ;State Distribution
  dis

  ;Control
  change-parameter?
  solcontrol ; to control distribution solution
  sc ec rc dc uc xc solc; variables to correct no solution in distribution nodes IN
  sd ed rd dd ud xd
  setup-control
]
turtles-own [ serdux-state links-number ]

;SETUP-----------------------------------------------------------------------------------------------------------------
to setup
  clear-all

  ; Variables storage and initialization
  rate-and-parameter-setup

  set t 0

  ;Creates the nodes in the graph (figure-color-size-location)
  set S-i susceptible-initial
  set E-i exposed-initial
  set D-i degraded-initial
  set U-i unavailable-initial
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


  ask turtles [ set size 1]
  ask turtles [ set links-number 0]

  ; Links creation depending of chooser: random or fixed
  setup-spatially-clustered-network-fixed
  ask links [ set color gray - 1 ]

  serdux-and-rates-initial-states

  ;; Count the actual turtles by state in graphic and safe the value in X-r
  serdux-actual-state

  ;Cyber Risk Approach initial calculation and assessment
  cyber-risk-approach-calculation

  ;Python initalization variables
  python-state-update
  python-rate-parameter-update

  set change-parameter? false
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

  set nl number-of-nodes-connections

  ;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)
  ; Rates
  set PRO0 propagation-rate-PRO0
  set UOC unavailability-other-causes-rate-UOC

  ;Parameters
  set tf parameter-for-time-in-cyber-attack-D-tf

  set a stregth-damping-loss-resistance-LOR-a
  set m time-damping-loss-resistance-LOR-m

  ;TARGET Capabilities
  set TCD target-cyberdefense-capability-TCD
  set TCI target-cyberintelligence-capability-TCI
  set TSS target-support-sustainability-capability-TSS

  ;TARGET Security Controls
  set CCM target-compensatory-controls-CCM
  set CDE target-deterrent-controls-CDE
  set CPR target-preventive-controls-CPR
  set CCR target-corrective-controls-CCR
  set CDV target-detective-controls-CDV

  ;ATTACKER LIKELIHOOD
  set ATF attacker-factors-ATF
  set VUF vulnerability-factors-VUF

  ;CYBER ATTACK
  set ADE cyber-attack-degree-ADE
  set ADU cyber-attack-duration-ADU
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
  if L < 0.3 [set Lasse "LOW"]
  if L >= 0.3 and L < 0.7 [set Lasse "MEDIUM"]
  if L >= 0.7 [set Lasse "HIGH"]

  if I < 0.3 [set Iasse "LOW"]
  if I >= 0.3 and I < 0.7 [set Iasse "MEDIUM"]
  if I >= 0.7 [set Iasse "HIGH"]

  if L < 0.3 and I < 0.3  [set Rasse "NOTE"]
  if L < 0.3 and I >= 0.3 and I < 0.7 [set Rasse "LOW"]
  if L < 0.3 and I >= 0.7 [set Rasse "MEDIUM"]

  if L >= 0.3 and L < 0.7 and I < 0.3 [set Rasse "LOW"]
  if L >= 0.3 and L < 0.7 and I >= 0.3 and I < 0.7 [set Rasse "MEDIUM"]
  if L >= 0.3 and L < 0.7 and I >= 0.7 [set Rasse "HIGH"]

  if L >= 0.7 and I < 0.3 [set Rasse "MEDIUM"]
  if L >= 0.7 and I >= 0.3 and I < 0.7 [set Rasse "HIGH"]
  if L >= 0.7 and I >= 0.7 [set Rasse "CRITICAL"]

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
  py:set "simt" t + 30
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
1482
34
2098
651
-1
-1
11.922
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
579
45
742
79
1. SETUP
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

BUTTON
578
180
741
214
2. GO
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
1005
10
1474
233
Network Status
Time (t)
Nodes
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Susceptible" 1.0 0 -13345367 true "" "plot S-py"
"Exposed" 1.0 0 -955883 true "" "plot E-py"
"Resistant" 1.0 0 -10899396 true "" "plot R-py"
"Degraded" 1.0 0 -8630108 true "" "plot D-py"
"Unavailable" 1.0 0 -2674135 true "" "plot U-py"
"Destroyed" 1.0 0 -16777216 true "" "plot X-py"

SLIDER
20
284
270
317
number-of-nodes-connections
number-of-nodes-connections
2
(susceptible-initial + exposed-initial + degraded-initial + unavailable-initial) - 1
3.0
2
1
links
HORIZONTAL

BUTTON
579
89
742
123
2. GO ( STEP-BY-STEP)
repeat (num-of-steps) [go] 
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
603
124
716
169
num-of-steps
num-of-steps
1 5 10 15 30 45 60 90 100 120 240 360 600 1200
9

SLIDER
12
354
282
387
propagation-rate-PRO0
propagation-rate-PRO0
0
1
0.75
0.1
1
NIL
HORIZONTAL

SLIDER
11
393
283
426
unavailability-other-causes-rate-UOC
unavailability-other-causes-rate-UOC
0
0.0025
0.0025
0.0005
1
NIL
HORIZONTAL

SLIDER
11
434
283
467
stregth-damping-loss-resistance-LOR-a
stregth-damping-loss-resistance-LOR-a
0.1
20
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
12
474
284
507
time-damping-loss-resistance-LOR-m
time-damping-loss-resistance-LOR-m
0.1
240
120.0
0.1
1
NIL
HORIZONTAL

SLIDER
51
167
217
200
exposed-initial
exposed-initial
0
1000
124.0
1
1
nodes
HORIZONTAL

SLIDER
50
207
219
240
degraded-initial
degraded-initial
0
100
31.0
1
1
nodes
HORIZONTAL

TEXTBOX
80
54
220
74
NODES & STATES
14
0.0
1

MONITOR
765
437
870
478
Propagation
PRO-py
3
1
10

MONITOR
765
483
870
524
Recovery
REC-py
6
1
10

MONITOR
767
583
873
624
Loss of Resistance
LOR-py
6
1
10

PLOT
1001
410
1477
649
Rates
Time (t)
Variable
0.0
5.0
0.0
1.0
true
true
"" ""
PENS
"Propagation" 1.0 0 -13345367 true "" "plot PRO-py"
"C. Attack - Degraded" 1.0 0 -8630108 true "" "plot CAD-py"
"C. Attack - Unavailable" 1.0 0 -2674135 true "" "plot CAU-py"
"C. Attack - Destroyed" 1.0 0 -16777216 true "" "plot CAX-py"
"Recovery" 1.0 0 -10899396 true "" "plot REC-py"
"Sanitation" 1.0 0 -955883 true "" "plot SAN-py"
"Loss of Resistance" 1.0 0 -11221820 true "" "Plot LOR-py"
"Unavailability Other Causes" 1.0 0 -5825686 true "" "plot UOC-py"

MONITOR
795
35
846
80
S
S-py
0
1
11

MONITOR
855
35
906
80
E
E-py
0
1
11

MONITOR
795
88
846
133
D
D-py
0
1
11

MONITOR
915
35
966
80
R
R-py
0
1
11

TEXTBOX
57
561
253
584
USE CASE PRE-SET VALUES
14
0.0
1

MONITOR
855
88
906
133
U
U-py
0
1
11

MONITOR
855
139
913
184
N
N
0
1
11

MONITOR
915
88
966
133
X
X-py
17
1
11

MONITOR
884
437
995
478
C. Attack - Degraded
CAD-py
3
1
10

MONITOR
765
533
871
574
Sanitation
SAN-py
6
1
10

MONITOR
882
484
993
525
C. Attack - Unavailable
CAU-py
3
1
10

MONITOR
882
583
993
624
Unavailability Other C.
UOC-py
17
1
10

SLIDER
340
471
525
504
cyber-attack-duration-ADU
cyber-attack-duration-ADU
0
1
0.75
0.1
1
NIL
HORIZONTAL

TEXTBOX
397
528
495
563
ATTACKER
14
0.0
1

SLIDER
344
551
526
584
attacker-factors-ATF
attacker-factors-ATF
0
1
0.75
0.1
1
NIL
HORIZONTAL

SLIDER
344
591
528
624
vulnerability-factors-VUF
vulnerability-factors-VUF
0
1
0.8
0.1
1
NIL
HORIZONTAL

TEXTBOX
409
29
488
47
Capabilities
12
0.0
1

SLIDER
306
89
553
122
target-cyberintelligence-capability-TCI
target-cyberintelligence-capability-TCI
0
1
0.25
0.1
1
NIL
HORIZONTAL

SLIDER
306
132
553
165
target-support-sustainability-capability-TSS
target-support-sustainability-capability-TSS
0
1
0.25
0.1
1
NIL
HORIZONTAL

TEXTBOX
19
328
305
347
SYSTEM OF DIFFERENTIAL EQUATIONS
14
0.0
1

SLIDER
308
322
554
355
target-corrective-controls-CCR
target-corrective-controls-CCR
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
308
280
554
313
target-preventive-controls-CPR
target-preventive-controls-CPR
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
307
196
555
229
target-compensatory-controls-CCM
target-compensatory-controls-CCM
0
1
0.25
0.1
1
NIL
HORIZONTAL

SLIDER
308
238
554
271
target-deterrent-controls-CDE
target-deterrent-controls-CDE
0
1
0.25
0.1
1
NIL
HORIZONTAL

TEXTBOX
854
410
906
428
RATES
14
0.0
1

TEXTBOX
788
10
980
29
NETWORK STATE - SERDUX
14
0.0
1

BUTTON
41
620
249
653
Change Rates and Parameters
set change-parameter? true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
384
411
496
430
CYBER ATTACK
14
0.0
1

TEXTBOX
393
175
513
193
Security Controls
12
0.0
1

MONITOR
882
533
993
574
C. Attack - Destroy
CAX-py
3
1
10

MONITOR
581
298
744
343
Cyber Risk Attack Severity
Rasse
17
1
11

MONITOR
582
398
745
443
Cyber Attack Likelihood
Lasse
17
1
11

MONITOR
581
551
743
596
Cyber Attack Impact
Iasse
17
1
11

TEXTBOX
622
221
706
247
CYBER RISK
14
0.0
1

SLIDER
340
431
524
464
cyber-attack-degree-ADE
cyber-attack-degree-ADE
0
1
0.75
0.1
1
NIL
HORIZONTAL

SLIDER
11
516
284
549
parameter-for-time-in-cyber-attack-D-tf
parameter-for-time-in-cyber-attack-D-tf
0
300
240.0
10
1
NIL
HORIZONTAL

TEXTBOX
409
10
468
30
TARGET
14
0.0
1

BUTTON
25
582
104
615
SIM 1
;;NETWORK SETUP\nset number-of-nodes-connections 3\n\n;;SERDUX - INITIAL STATES\nset susceptible-initial 6045\nset exposed-initial 124\nset degraded-initial 31\nset unavailable-initial 0\n\n;;SYSTEM OF DIFFERENTIAL EQUATIONS (SDE)\n;rates\nset propagation-rate-PRO0 0.75\nset unavailability-other-causes-rate-UOC 0.0025\n\nset parameter-for-time-in-cyber-attack-D-tf 240\n\nset stregth-damping-loss-resistance-LOR-a 0.1\nset time-damping-loss-resistance-LOR-m 120\n\n;;TARGET\n;Target Capabilities\n  SET target-cyberdefense-capability-TCD 0.1\n  set target-cyberintelligence-capability-TCI 0.25\n  set target-support-sustainability-capability-TSS 0.25\n  \n;Target Security Controls\n  set target-compensatory-controls-CCM 0.25\n  set target-deterrent-controls-CDE 0.25\n  set target-preventive-controls-CPR 0.1\n  set target-corrective-controls-CCR 0.1\n  set target-detective-controls-CDV 0.25\n\n\n;;ATTACKER\n  set attacker-factors-ATF 0.75\n  set vulnerability-factors-VUF 0.8\n  \n;;CYBER ATTACK\n  set cyber-attack-degree-ADE 0.75\n  set cyber-attack-duration-ADU 0.75\n  \n  set num-of-steps 120
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
110
582
189
615
SIM 2
set unavailability-other-causes-rate-UOC 1\nset cyber-attack-duration-ADU 0.25\nset num-of-steps 120\nset change-parameter? true
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
194
582
273
615
SIM 3
set unavailability-other-causes-rate-UOC 0.0025\nset target-preventive-controls-CPR 0.25\nset target-corrective-controls-CCR 0.25\n\n ask turtles with  [ serdux-state = \"X\" ]\n    [ set serdux-state \"U\" set color red ]\n serdux-actual-state\n python-state-update\n\n\nset change-parameter? true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
293
10
311
680
|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n
11
0.0
1

SLIDER
50
246
218
279
unavailable-initial
unavailable-initial
0
100
0.0
1
1
nodes
HORIZONTAL

MONITOR
635
248
686
293
R
R
2
1
11

MONITOR
636
349
687
394
L
L
4
1
11

MONITOR
635
501
686
546
I
I
2
1
11

MONITOR
582
448
639
493
ALI
ALI
2
1
11

MONITOR
685
447
742
492
TLI
TLI
2
1
11

MONITOR
692
601
743
646
IRC
IRC
2
1
11

MONITOR
581
601
632
646
ADE
ADE
2
1
11

MONITOR
635
601
686
646
ADU
ADU
2
1
11

TEXTBOX
565
15
583
685
|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n
11
0.0
1

TEXTBOX
753
15
770
685
|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n
11
0.0
1

MONITOR
792
268
844
313
SER
S-r + E-r + R-r
0
1
11

MONITOR
912
268
964
313
DUX
D-r + U-r + X-r
17
1
11

MONITOR
762
321
875
366
% Active Services
(S-r + E-r + R-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)
2
1
11

PLOT
1001
237
1476
406
Level of Services
Time (t)
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Active Services" 1.0 0 -13840069 true "" "plot (S-r + E-r + R-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)"
"Inactive Services" 1.0 0 -5298144 true "" "plot (D-r + U-r + X-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)"

SLIDER
50
128
218
161
susceptible-initial
susceptible-initial
0
10000
6045.0
1
1
nodes
HORIZONTAL

MONITOR
79
78
189
123
Number of Nodes
susceptible-initial + exposed-initial + degraded-initial + unavailable-initial
0
1
11

SLIDER
308
363
554
396
target-detective-controls-CDV
target-detective-controls-CDV
0
1
0.25
0.1
1
NIL
HORIZONTAL

TEXTBOX
25
38
270
57
---------------------------------------------------
11
0.0
1

TEXTBOX
30
10
259
42
SERDUX-MARCIM
25
105.0
1

MONITOR
882
321
994
366
% Inactive Services
(D-r + U-r + X-r) * 100 / (S-py + E-py + R-py + D-py  + U-py + X-py)
2
1
11

SLIDER
306
49
553
82
target-cyberdefense-capability-TCD
target-cyberdefense-capability-TCD
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
811
241
948
260
LEVEL OF SERVICES
14
0.0
1

TEXTBOX
626
10
715
29
CONTROLS
14
0.0
1

TEXTBOX
1635
10
1989
28
SERDUX NODE LEVEL CONTROL AND VISUALIZATION
14
0.0
1

@#$#@#$#@
# SOFTWARE TITLE

SERDUX-MARCIM implementation in Netlogo ©.

# WHAT IS IT?

This software is a Netlogo implementation for  "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024),  a model for simulating, modeling, and analyzing cyber attacks' propagation in maritime infrastructure. 

The software's purpose is to be a simulation tool for the SERDUX-MARCIM model, in which the experimenter could analyze and forecast the propagation of a cyber attack over a maritime infrastructure using different scenarios and configurations through the setup of network-specific characteristics, target capabilities, attacker capabilities, cyber attack characteristics, and specific MATHEMATICAL model characteristics.

# USER INTERFACE DESCRIPTION

Three parts divide the user interface:

## 1) SIMULATION CONFIGURATION

In this part, the experimenter establishes the value of every variable and parameter considered for the SERDUX-MARCIM model (Diego Cabuya et al., 2024), assuming the specifications and assessment described in the related scientific paper. In this sense, the following are the sub-parts for this configuration:

* **NODES & STATES:** Configuration of the initial number of nodes in states S, E, D, and U, together with the average number of nodes connexions.

* **SYSTEM OF DIFFERENTIAL EQUATIONS:** Configuration of the initial rates and parameters necessary for the system of differential equations.

* **TARGET:** Configuration of the Target's Capabilities and Security Controls values.

* **CYBER ATTACK:** Configuration of the initial values for cyber attack degree and duration.

* **ATTACKER:** Configuration of the initial values for attacker and vulnerability factors.

## 2) CONTROLS

This part contains the main routines and instructions to run every simulation. It has three buttons:

* **SETUP:** This is the first button to execute when it opens the software for the first time. It is also the button to capture the values established in the SIMULATION CONFIGURATION part to run the simulation. When the button is executed for the first time, the necessary Python configuration runs in a second plane; in the following executions, the software updates the variables and parameters established in the SIMULATION CONFIGURATION part.

* **GO:** Once the experimenter establishes the variables and parameters for the simulation and executes the SETUP button, the simulation starts in an ENDLESS loop that finishes when it executes the GO button again, or the simulation ends by the "simulation stop conditions": (1) All nodes are in states S, E or R; (2) All nodes are in states D, U, or X.

* **GO (STEP-BY-STEP):** This button has the same effect as the GO button; however, for this execution, the simulation runs a fixed number of ticks (time steps), taking into account the value in the **"num-of-step"** chooser. The simulation will stop when the fixed ticks are finished.

## 3) SIMULATION RESULTS

The simulation results and outcomes are presented in this part through NetLogo monitor and plot tools, considering the following sub-parts:

* **CYBER RISK:** Show all the SERDUX-MARCIM Cyber Risk Approach values the software calculates to assess the general cyber risk.

* **NETWORK STATE—SERDUX:** Show the number of nodes in every SERDUX state at a moment t (tick), the total number of nodes at a moment t (tick), and a line graphic to visualize the number of nodes during the simulation.

* **LEVEL OF SERVICES:** Show the sum of nodes in states S, E, and R, which represents the "Active Services," together with the percentage that represents this number of nodes relative to the total number of nodes. Likewise, with nodes in states D, U, and X, which represents the "Inactive Services."   A line graphic is also used to visualize the number of nodes in these groups during the simulation.

* **RATES:** Show the values of every SERDUX-MARCIM rate at a moment t (tick). A line graphic is also used to visualize the rate behavior during the simulation.

* **SERDUX NODE LEVEL CONTROL AND VISUALIZATION:** Show the network and nodes considered in the simulation to control the network's changes during the simulation visually and to allow monitoring of nodes and their individual behavior during the simulations using the monitoring tools provided by NetLogo software.

## 4) USE CASE PRE-SET VALUES

The scientific paper "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024) contains a use case in which the proposed SERDUX-MARCIM model was successfully implemented and applied in a real-world case study, the 2017 Maersk cyber attack, demonstrating its efficacy in addressing and reflecting the targeted problem.

Considering this use case, this software contains three buttons with pre-set values to run and validate the use case:

* **SIM1:** Pre-set values for the first simulation considered in the use case

* **SIM2:** Pre-set values for the second simulation considered in the use case.

* **SIM3:** Pre-set values for the third simulation considered in the use case.

To run this simulation, the experimenter must first execute the SIM1 button, then execute SETUP and run the experimentation using the GO (STEP-BY-STEP) button. When the first simulation is over (120 ticks), the experimenter must press the SIM2 button and execute the GO (STEP-BY-STEP) button. When the second simulation is over (120 ticks), the experimenter must press the SIM3 button and execute the GO (STEP-BY-STEP) button.



Additionally, if the experimenter changes the conditions during the simulation, the variables and parameters must first be set in the SIMULATION CONFIGURATION part. After that, execute the **CHANGE RATES AND PARAMETER** button and run the simulation with the GO or GO (STEP-BY-STEP) button. This is an extra functionality to change the dynamic during simulations.

# HOW IT WORKS

The software implements the scientific paper "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024). In this sense, the code is fixed to establish the calculations and generate the results.

The general way that the software works is the following:

**(1) INITIAL VALUES AND SETUP:** The values set in the SIMULATION CONFIGURATION part are captured and stored in internal variables, and all the NetLogo parameters and variables are initialized.

**(2) NETWORK CREATION:**  Creation of the node network taking into account the number of nodes in every SERDUX state and the number of connections.

**(3) CYBER RISK APPROACH ASSESSMENT:** Calculation of all Cyber Risk Approach variables.

**(4) PYTHON CONFIGURATION:** Initialization and configuration of Python modules to run the SERDUX-MARCIM mathematical model.

**(5) SYSTEM OF DIFFERENTIAL EQUATIONS SOLUTION:** Executing the Python System of Differential Equation functions and routines to have the simulation data in the defined simulation time.

**(6) NODE DISTRIBUTION BETWEEN SERDUX STATES:** This step executes the Python Node Distribution functions and routines to solve the allowed state transitions of the nodes considering the SERDUX-MARCIM model. In this sense, it defines the node state transitions considering the System of Differential Equation solution.

**(7) NODES ACTUALIZATION AND SIMULATION RESULTS:** With the solution of steps (5) and (6), the software updates the state of every node, rates, parameter, and variables considered in SERDUX-MARCIM, and it is presented in the SIMULATION RESULTS part.

# HOW TO USE IT

The primary use of the software is as follows:

**1)** Establishes the value of every variable and parameter considered for the SERDUX-MARCIM model in the SIMULATION CONFIGURATION part, considering "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024) scientific paper.

**2)** Execute the SETUP Button.

**3)** Execute the GO or GO (STEP-BY-STEP) button to run the simulation.

**4)** Check and analyze the simulation results and outcomes in the SIMULATION RESULTS.

To use the software appropriately, it was previously necessary to **install NetLogo Software** and the **Python Programming Language** with the necessary libraries: math, sys, os, sympy, numpy, scipy, matplotlib, re, collections, and pandas. Lastly, it is necessary **set the path to the Python executable in Netlogo**; Additional information available in the "NetLogoUser Manual"  https://ccl.northwestern.edu/netlogo/docs/ .

# RELATED MODELS

SERDUX-MARCIM, SIR, SEIR, SEIRS, MalSEIRS, Virus, Disease, Preferential Attachment, Diffusion on a Directed Network, NetLogo Virus on a Network model.

# HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Cabuya-Padilla, Diego. SERDUX-MARCIM application in Netlogo, 2024.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

# COPYRIGHT AND LICENSE

Copyright 2024 Diego Edison Cabuya Padilla ©.
diego.cabuya@gmail.com


Based on:

* Cabuya-Padilla, Diego et al., (2024). SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling.
* Stonedahl, F. and Wilensky, U. (2008). NetLogo Virus on a Network model.
 http://ccl.northwestern.edu/netlogo/models/VirusonaNetwork. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
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
