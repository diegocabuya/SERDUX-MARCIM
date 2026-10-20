# ABM-MARCIM — verification data of the agent layer

Data, experiment definitions and analysis script behind Section V ("Verification and Results") of

> Cabuya-Padilla, D. E., & Castaneda-Marroquin, C. A. *ABM-MARCIM: Agent-Based Specification of a Maritime
> Cyberdefense Framework with a Differential-Equation Update Function* (manuscript).

The model itself (SERDUX-MARCIM, NetLogo 6.4.0 + Python) is deposited in the CoMSES Net Computational Model
Library, <https://www.comses.net/codebases/8d024071-5142-4a0d-b32f-8372b26f89e1/>, and maintained in this
repository under `SERDUX-MARCIM/`.

## Files

| File | What it is | Size | SHA-256 |
|---|---|---:|---|
| `data/esc1.csv` | BehaviorSpace result table, **scenario 1**: reference network, N = 71 agents, 30 replications × 36 steps (0–35) | 413,750 B | `624ecb415cbdadae069a279caec07e79700bd4746d079e9efb168b973a185df6` |
| `data/esc2_nodos10.csv` | BehaviorSpace result table, **scenario 2**: first event of the Maersk case, N = 6200 agents, 10 replications × 121 steps (0–120) | 15,307,742 B | `0b9249e966ceeccf0ab5a67a886d9ae45639a9430d76c488d420b55d1871909e` |
| `behaviorspace/experiments.xml` | The two BehaviorSpace experiment definitions (parameters, metrics, repetitions, time limit), reconstructed from the headers of the two tables | | |
| `analysis/figures.py` | Reads the two tables, recomputes every figure reported in the paper and draws Figures 2–5 | | |

Both tables were exported by NetLogo 6.4.0 ("Table version 2.0") on 18 August 2026 from a working copy of
the deposited model file, with BehaviorSpace set to *table output*, no spreadsheet output, and one run at a
time (each run starts its own Python interpreter). The parameter values are those of Table 4 of the paper.

## Format of the tables

Standard BehaviorSpace table layout: six header lines (NetLogo version, model file, experiment name,
timestamp, world bounds), then one header row and one data row per `[run number]` × `[step]`. Columns:

- `[run number]` — replication (1–30 in scenario 1, 1–10 in scenario 2).
- 23 parameter columns, constant within each experiment (`propagation-rate-PRO0`, `cyber-attack-degree-ADE`,
  `target-preventive-controls-CPR`, …). Their values are listed in `behaviorspace/experiments.xml`.
- `[step]` — simulation step, from 0 (initial conditions) to the time limit.
- `count turtles with [serdux-state = "S"]` … `"X"` — number of agents in each of the six states
  (Susceptible, Exposed, Resistant, Degraded, Unavailable, Destroyed).
- `solcontrol` — 1 when the state allocation algorithm (breadth-first search over the thirteen admissible
  transitions) reconciled the aggregate solution with the agent population at that step; 0 when the
  reconciliation routine had to act. Step 0 is always 0 (no allocation is performed at initialization).
- `L`, `R`, `I` — cyberattack likelihood, risk severity and impact of the maritime cyber risk model.
- `map [ ag -> [serdux-state] of ag ] sort turtles` — the state of **every agent**, in order of agent
  identifier, as a NetLogo list such as `[S S E S D …]`. Because `sort turtles` orders by `who`, position
  *i* refers to the same agent at every step and in every replication, which is what allows individual
  histories to be reconstructed and compared across replications.

## Reproducing the figures

```bash
pip install matplotlib numpy pillow
python3 analysis/figures.py          # reads ../data, writes ../figures
```

Before drawing anything, the script recomputes the 19 quantities stated in the paper (aggregate determinism,
divergence maxima and final values, state-change statistics, time in state, share of agents reaching
*Destroyed*, allocation resolution rates and the reconciliation steps 6, 24, 31 and 33) and stops if any of
them differs from the published value. The directories can be overridden with the environment variables
`ABM_DATOS` (input) and `ABM_FIG` (output).

## License

Data and documentation in this folder: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
The analysis script: MIT. The model code keeps the license stated in its CoMSES release.
