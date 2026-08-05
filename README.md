<div align="center">

<!-- >> AJUSTAR <<: cuando tengas un logo/banner, súbelo a assets/logo.png y descomenta:
<img src="assets/logo.png" alt="SERDUX-MARCIM" width="220"/>
-->

# SERDUX-MARCIM

### Maritime Cyberattack Simulation using Dynamic Modeling, Compartmental Epidemiological Models and Agent-Based Modeling

*A strategic modeling and simulation environment for maritime cyberdefense — part of the MARCIM reference framework.*

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![DOI](https://img.shields.io/badge/DOI-10.1007%2Fs10207--025--00985--6-blue.svg)](https://doi.org/10.1007/s10207-025-00985-6)
[![Made with MATLAB](https://img.shields.io/badge/Made%20with-MATLAB-orange.svg)](https://www.mathworks.com/products/matlab.html)
[![Made with NetLogo](https://img.shields.io/badge/Made%20with-NetLogo-brightgreen.svg)](https://ccl.northwestern.edu/netlogo/)
[![Last commit](https://img.shields.io/github/last-commit/diegocabuya/SERDUX-MARCIM.svg)](https://github.com/diegocabuya/SERDUX-MARCIM/commits/main)
[![Stars](https://img.shields.io/github/stars/diegocabuya/SERDUX-MARCIM.svg?style=social)](https://github.com/diegocabuya/SERDUX-MARCIM/stargazers)

<!-- >> AJUSTAR <<: cuando conectes Zenodo (ver README de instrucciones), pega aquí el badge del DOI del software -->

</div>

---

## Table of contents

- [Overview](#overview)
- [Key features](#key-features)
- [Repository structure](#repository-structure)
- [Requirements](#requirements)
- [Getting started](#getting-started)
- [Demo](#demo)
- [Related publications](#related-publications)
- [How to cite](#how-to-cite)
- [Roadmap](#roadmap)
- [Contributing](#contributing)
- [Acknowledgements](#acknowledgements)
- [Contact](#contact)

---

## Overview

**SERDUX-MARCIM** is a simulation model for representing, modeling and analyzing the propagation of cyberattacks across maritime infrastructure, taking into account network-specific characteristics as well as target and attacker capabilities. It combines **compartmental models from epidemiology** with **agent-based modeling** to reproduce how a cyber incident spreads through multiple interconnected nodes and impacts the maritime domain.

The model is part of **MARCIM** (*Marco de Referencia para el Modelamiento y Simulación de la Ciberdefensa Marítima* / Reference Framework for the Modeling and Simulation of Maritime Cyberdefense), a research line developed at the **Escuela Naval de Cadetes "Almirante Padilla"** (Cartagena, Colombia). The simulation environment is implemented in **MATLAB** and **NetLogo**, and integrates widely accepted cyber risk assessment methodologies (IMO, OWASP, MITRE, ISACA).

> <!-- >> AJUSTAR <<: 1–2 frases con el propósito en tus palabras si quieres personalizar más. -->

## Key features

- 🌊 **Maritime-specific** — models actors, capabilities and dynamics of the maritime cyberdefense ecosystem.
- 🦠 **Epidemiological propagation** — compartmental (SIR-like) states to describe how a cyberattack spreads node to node.
- 🤖 **Agent-based** — attacker, defender and support agents with distinct capabilities and behaviors.
- 📊 **Risk-aware** — incorporates OWASP, MITRE (D5 effects), ISACA controls and IMO guidance.
- 🎯 **Scenario validation** — tested against real-world maritime cyber campaigns (e.g., Maersk/NotPetya).
- 🎓 **Training-ready** — used as the engine behind a Hybrid Tabletop Exercise (TTX) methodology.

## Repository structure

| Folder | Description |
| --- | --- |
| [`SERDUX-MARCIM/`](SERDUX-MARCIM) | Core simulation model (MATLAB + NetLogo): compartmental + agent-based engine, scenarios and datasets. |
| [`MARCIM Project/`](MARCIM%20Project) | Conceptual and reference-framework materials of the MARCIM project. <!-- >> AJUSTAR descripción << --> |
| [`MARCIM-WG/`](MARCIM-WG) | Working-group / wargaming resources and supporting material. <!-- >> AJUSTAR descripción << --> |
| [`Hybrid-TTX-MARCIM/`](Hybrid-TTX-MARCIM) | Hybrid Tabletop Exercise: scenario, datasets and implementation used in the maritime-sector TTX. |

## Requirements

- **MATLAB** R20XX or later <!-- >> AJUSTAR versión mínima << --> (Toolboxes: `<listar si aplica>`)
- **NetLogo** 6.x or later <!-- >> AJUSTAR versión << -->
- Operating system: Windows / macOS / Linux <!-- >> AJUSTAR si aplica << -->

## Getting started

```bash
# 1. Clone the repository
git clone https://github.com/diegocabuya/SERDUX-MARCIM.git
cd SERDUX-MARCIM
```

<!-- >> AJUSTAR <<: pon aquí los pasos reales para correr la simulación. Por ejemplo: -->
1. Open the NetLogo model located in `SERDUX-MARCIM/` (`<nombre_del_modelo>.nlogo`).
2. Configure the scenario parameters (nodes, attacker/defender capabilities, risk profile).
3. Run the simulation and, if needed, export results to MATLAB for post-processing using `<script>.m`.

## Demo

<!-- >> AJUSTAR <<: graba un GIF de la simulación de NetLogo corriendo y súbelo a assets/demo.gif.
     Un GIF animado es lo que MÁS aporta al "efecto página web". Luego descomenta: -->
<!-- ![SERDUX-MARCIM demo](assets/demo.gif) -->

> _A short animated demo of the simulation will appear here._

## Related publications

If this repository is useful for your work, these are the peer-reviewed references behind it:

- Cabuya-Padilla, D., Díaz-López, D., Martínez-Páez, J., Hernandez, L., & Castañeda-Marroquín, C. (2025). **SERDUX-MARCIM: Maritime Cyberattack Simulation Using Dynamic Modeling, Compartmental Models in Epidemiology and Agent-Based Modeling.** *International Journal of Information Security.* https://doi.org/10.1007/s10207-025-00985-6
- Cabuya-Padilla, D. E., & Castañeda-Marroquín, C. A. (2024). **Marco de referencia para el modelamiento y simulación de la ciberdefensa marítima — MARCIM: estado del arte y metodología.** *DYNA, 91*(231), 169–179. https://doi.org/10.15446/dyna.v91n231.109774
<!-- >> AJUSTAR <<: agrega aquí el paper del Hybrid TTX y el artículo del modelo estratégico cuando estén publicados. -->

## How to cite

This repository includes a [`CITATION.cff`](CITATION.cff) file, so GitHub shows a **"Cite this repository"** button in the right-hand sidebar (APA and BibTeX generated automatically).

Preferred citation:

> Cabuya-Padilla, D., Díaz-López, D., Martínez-Páez, J., Hernandez, L., & Castañeda-Marroquín, C. (2025). *SERDUX-MARCIM: Maritime Cyberattack Simulation Using Dynamic Modeling, Compartmental Models in Epidemiology and Agent-Based Modeling.* International Journal of Information Security. https://doi.org/10.1007/s10207-025-00985-6

## Roadmap

- [ ] Extend the model toward mathematical and operational representations.
- [ ] Add expert-based and experimental validation of applied scenarios.
- [ ] Publish reproducible scenario datasets.
<!-- >> AJUSTAR <<: pon los siguientes pasos reales del proyecto. -->

## Contributing

Contributions, issues and suggestions are welcome. Please open an [issue](https://github.com/diegocabuya/SERDUX-MARCIM/issues) to discuss changes before submitting a pull request.

## Acknowledgements

This work is part of the research project *"Marco de referencia para el modelamiento y simulación de la ciberdefensa marítima – MARCIM"*, developed within the doctoral program in *Ciencias del Mar* and the **Grupo de Investigación en Comunicaciones, Control y Diseño Naval (GICCDN)** of the **Escuela Naval de Cadetes "Almirante Padilla"**.

## Contact

**Diego Edison Cabuya-Padilla** — [ORCID 0000-0001-5338-9943](https://orcid.org/0000-0001-5338-9943) · diego.cabuya@enap.edu.co

**Carlos Alfonso Castañeda-Marroquín** — [ORCID 0000-0002-4957-158X](https://orcid.org/0000-0002-4957-158X) · carlos.castaneda@enap.edu.co

Escuela Naval de Cadetes "Almirante Padilla" — Cartagena, Colombia
