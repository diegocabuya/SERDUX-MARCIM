# SERDUX-MARCIM implementation in Netlogo

## WHAT IS IT?

This software is a Netlogo implementation for  "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024),  a model for simulating, modeling, and analyzing cyber attacks' propagation in maritime infrastructure. 

The software's purpose is to be a simulation tool for the SERDUX-MARCIM model, in which the experimenter could analyze and forecast the propagation of a cyber attack over a maritime infrastructure using different scenarios and configurations through the setup of network-specific characteristics, target capabilities, attacker capabilities, cyber attack characteristics, and specific MATHEMATICAL model characteristics.

## USER INTERFACE DESCRIPTION

Three parts divide the user interface:

### 1) SIMULATION CONFIGURATION

In this part, the experimenter establishes the value of every variable and parameter considered for the SERDUX-MARCIM model (Diego Cabuya et al., 2024), assuming the specifications and assessment described in the related scientific paper. In this sense, the following are the sub-parts for this configuration:

* **NODES & STATES:** Configuration of the initial number of nodes in states S, E, D, and U, together with the average number of nodes connexions.

* **SYSTEM OF DIFFERENTIAL EQUATIONS:** Configuration of the initial rates and parameters necessary for the system of differential equations.

* **TARGET:** Configuration of the Target's Capabilities and Security Controls values.

* **CYBER ATTACK:** Configuration of the initial values for cyber attack degree and duration.

* **ATTACKER:** Configuration of the initial values for attacker and vulnerability factors.

### 2) CONTROLS

This part contains the main routines and instructions to run every simulation. It has three buttons:

* **SETUP:** This is the first button to execute when it opens the software for the first time. It is also the button to capture the values established in the SIMULATION CONFIGURATION part to run the simulation. When the button is executed for the first time, the necessary Python configuration runs in a second plane; in the following executions, the software updates the variables and parameters established in the SIMULATION CONFIGURATION part.

* **GO:** Once the experimenter establishes the variables and parameters for the simulation and executes the SETUP button, the simulation starts in an ENDLESS loop that finishes when it executes the GO button again, or the simulation ends by the "simulation stop conditions": (1) All nodes are in states S, E or R; (2) All nodes are in states D, U, or X.

* **GO (STEP-BY-STEP):** This button has the same effect as the GO button; however, for this execution, the simulation runs a fixed number of ticks (time steps), taking into account the value in the **"num-of-step"** chooser. The simulation will stop when the fixed ticks are finished.

### 3) SIMULATION RESULTS

The simulation results and outcomes are presented in this part through NetLogo monitor and plot tools, considering the following sub-parts:

* **CYBER RISK:** Show all the SERDUX-MARCIM Cyber Risk Approach values the software calculates to assess the general cyber risk.

* **NETWORK STATE—SERDUX:** Show the number of nodes in every SERDUX state at a moment t (tick), the total number of nodes at a moment t (tick), and a line graphic to visualize the number of nodes during the simulation.

* **LEVEL OF SERVICES:** Show the sum of nodes in states S, E, and R, which represents the "Active Services," together with the percentage that represents this number of nodes relative to the total number of nodes. Likewise, with nodes in states D, U, and X, which represents the "Inactive Services."   A line graphic is also used to visualize the number of nodes in these groups during the simulation.

* **RATES:** Show the values of every SERDUX-MARCIM rate at a moment t (tick). A line graphic is also used to visualize the rate behavior during the simulation.

* **SERDUX NODE LEVEL CONTROL AND VISUALIZATION:** Show the network and nodes considered in the simulation to control the network's changes during the simulation visually and to allow monitoring of nodes and their individual behavior during the simulations using the monitoring tools provided by NetLogo software.

### 4) USE CASE PRE-SET VALUES

The scientific paper "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024) contains a use case in which the proposed SERDUX-MARCIM model was successfully implemented and applied in a real-world case study, the 2017 Maersk cyber attack, demonstrating its efficacy in addressing and reflecting the targeted problem.

Considering this use case, this software contains three buttons with pre-set values to run and validate the use case:

* **SIM1:** Pre-set values for the first simulation considered in the use case

* **SIM2:** Pre-set values for the second simulation considered in the use case.

* **SIM3:** Pre-set values for the third simulation considered in the use case.

To run this simulation, the experimenter must first execute the SIM1 button, then execute SETUP and run the experimentation using the GO (STEP-BY-STEP) button. When the first simulation is over (120 ticks), the experimenter must press the SIM2 button and execute the GO (STEP-BY-STEP) button. When the second simulation is over (120 ticks), the experimenter must press the SIM3 button and execute the GO (STEP-BY-STEP) button.



Additionally, if the experimenter changes the conditions during the simulation, the variables and parameters must first be set in the SIMULATION CONFIGURATION part. After that, execute the **CHANGE RATES AND PARAMETER** button and run the simulation with the GO or GO (STEP-BY-STEP) button. This is an extra functionality to change the dynamic during simulations.

## HOW IT WORKS

The software implements the scientific paper "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024). In this sense, the code is fixed to establish the calculations and generate the results.

The general way that the software works is the following:

**(1) INITIAL VALUES AND SETUP:** The values set in the SIMULATION CONFIGURATION part are captured and stored in internal variables, and all the NetLogo parameters and variables are initialized.

**(2) NETWORK CREATION:**  Creation of the node network taking into account the number of nodes in every SERDUX state and the number of connections.

**(3) CYBER RISK APPROACH ASSESSMENT:** Calculation of all Cyber Risk Approach variables.

**(4) PYTHON CONFIGURATION:** Initialization and configuration of Python modules to run the SERDUX-MARCIM mathematical model.

**(5) SYSTEM OF DIFFERENTIAL EQUATIONS SOLUTION:** Executing the Python System of Differential Equation functions and routines to have the simulation data in the defined simulation time.

**(6) NODE DISTRIBUTION BETWEEN SERDUX STATES:** This step executes the Python Node Distribution functions and routines to solve the allowed state transitions of the nodes considering the SERDUX-MARCIM model. In this sense, it defines the node state transitions considering the System of Differential Equation solution.

**(7) NODES ACTUALIZATION AND SIMULATION RESULTS:** With the solution of steps (5) and (6), the software updates the state of every node, rates, parameter, and variables considered in SERDUX-MARCIM, and it is presented in the SIMULATION RESULTS part.

## HOW TO USE IT

The primary use of the software is as follows:

**1)** Establishes the value of every variable and parameter considered for the SERDUX-MARCIM model in the SIMULATION CONFIGURATION part, considering "SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling" (Diego Cabuya et al., 2024) scientific paper.

**2)** Execute the SETUP Button.

**3)** Execute the GO or GO (STEP-BY-STEP) button to run the simulation.

**4)** Check and analyze the simulation results and outcomes in the SIMULATION RESULTS.

To use the software appropriately, it was previously necessary to **install NetLogo Software** and the **Python Programming Language** with the necessary libraries: math, sys, os, sympy, numpy, scipy, matplotlib, re, collections, and pandas. Lastly, it is necessary **set the path to the Python executable in Netlogo**; Additional information available in the "NetLogoUser Manual"  https://ccl.northwestern.edu/netlogo/docs/ .

## RELATED MODELS

SERDUX-MARCIM, SIR, SEIR, SEIRS, MalSEIRS, Virus, Disease, Preferential Attachment, Diffusion on a Directed Network, NetLogo Virus on a Network model.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Cabuya-Padilla, Diego. SERDUX-MARCIM application in Netlogo, 2024.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2024 Diego Edison Cabuya Padilla ©.
diego.cabuya@gmail.com

Based on:

* Cabuya-Padilla, Diego et al., (2024). SERDUX-MARCIM: Maritime cyberattack simulation using compartmental models in epidemiology and agent-based modeling.
* Stonedahl, F. and Wilensky, U. (2008). NetLogo Virus on a Network model.
 http://ccl.northwestern.edu/netlogo/models/VirusonaNetwork. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
