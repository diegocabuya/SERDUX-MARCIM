# SERDUX-MARCIM: Maritime Cyberattack Simulation

This README is for the MATLAB files used in Section 7 of the SERDUX-MARCIM paper, focusing on the analysis of the SERDUX-MARCIM model.

## Main Files to run the simulations of the model 

### Used in section 7.1  Initial conditions
- `SERDUX.m`: Main script for running the cyber-physical system simulation.

### Used in section 7.4  Comparative evaluation of SERDUX
- `Malseris_Tests.m`: Script containing tests for the Malseris cyber-physical system model.
- `SEIRS_test.m`: Script for testing the SEIRS model.


## Files used to solve the ODE

- `Rates.m`: Function file  used to solve the ODE systems of differential equations for SERDUX.
- `SEIRS.m`: Function file used to solve the ODE systems of differential equations for SEIRS.
- `MalSEIRS.m`: Function file document used to solve the ODE systems of differential equations for MalSEIRS.

## Files Used in Section 7.2 Experiments

- `L_test.m`: Script for testing the cyber attack likelihood parameter.
- `a_test.m`: Script for testing the damping parameter.
- `omega_test.m`: Script for testing the omega parameter.
- `beta_test.m`: Script for testing the malware initial propagation rate parameter.
- `upsilon_test.m`: Script for testing the preventive controls parameter.
- `delta_test.m`: Script for testing the attack duration parameter.
- `zeta_test.m`: Script for testing the detective controls parameter.
- `Psi_test.m`: Script for testing the attack degree parameter.
- `eta_test.m`: Script for testing the corrective controls parameter.
- `m_test.m`: Script for testing the cosine's period parameter.
- `mu_test.m`: Script for testing the machine unavailability rate parameter.

## Usage

Each script or function file can be executed independently to perform specific tests or simulations, except for the Function files(Rates.m, SEIRS.m, MalSEIRS.m). Make sure to set the necessary parameters and initial conditions within the files before running them.



