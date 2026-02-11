# REMAINS-Chile

**REMAINS-Chile** is a spatially explicit stochastic landscape dynamics model adapted for central-southern Chile (regions Ñuble, Biobío, and La Araucanía). 
It builds on the original **REMAINS** model developed by the FirESmart project for Mediterranean landscapes.

The model simulates **wildfire regimes**, **fire suppression strategies**, and **vegetation dynamics** over a 20-year horizon using a 1 km² grid.

## Key Features

- Probabilistic wildfire ignition and spread based on slope, land-cover flammability, and fuel continuity
- Region-specific calibrated fire size and annual burnt area distributions (1999–2018 CONAF data)
- Two suppression mechanisms: fuel-based (threshold on spread rate) and mosaic-based (contiguous agriculture acts as barrier)
- Custom improved `fire.risk()` function (explicit orography join, finer slope classes, proper inclusion of economic damage in risk score)
- Monte Carlo replicates (default n=30) for uncertainty quantification
- Optional initial maps at t=0 (fire risk, land-cover type, etc.)
- Outputs: detailed fire statistics, burnt/suppressed area by region & land-cover, age distributions

## Current Configuration (February 2026)

- Wildfires enabled; land-cover change, prescribed burns, post-fire regeneration, afforestation, and encroachment disabled
- Three suppression scenarios:
  - **0.1**: low suppression (fuel threshold = 0.1)
  - **0.5**: medium suppression
  - **0.9**: high suppression
- Initial landscape aged to `tschg = 50` years
- Prescribed burn area varies by scenario (1000–20000 ha/year)

## Repository Structure

REMAINS_Chile/
├── data/
│   ├── landscape.prob.fire.50.rda      # ignition prob + land cover (tschg=50)
│   ├── orography.rda                   # slope, elevation, region
│   ├── mask.study.area.rds             # study area mask
│   ├── burnt.area.dist.*.rda           # calibrated annual burnt area by region (factor 4.0 for climate change scenario)
│   └── fire.size.dist.*.rda            # calibrated fire size distributions
├── outputs/
│   ├── 0.1/, 0.5/, 0.9/                # scenario folders
│   │   ├── maps/                       # initial TIFF maps (only for 0.1)
│   │   ├── result.rda                  # full simulation list
│   │   ├── fires.txt, lc.burnt.txt, …  # tracking tables
│   │   └── model_output.txt
├── scripts/
|    ├── run_model_chile.R                   # main runner script
|    ├── params_chile.R                      # Chile-specific parameters
|    ├── fire_risk_chile.R                   # custom fire.risk()
|    ├── wildfires_chile.R                   # wildfire simulation
|    ├── interface.r                         # neighborhood interface classification
|    ├── internals.r                         # helper functions
|    └── land.dyn.mdl.chile.R                # core model loop

## Requirements

- R ≥ 4.2
- tidyverse, terra
- Optional: Original REMAINS package (not required, custom functions override where needed)

## Quick Start

1. Ensure all .rda/.rds files are in data/ with paths matching the script
2. Run the main script: source("run_model_chile.R")
3. Runtime: ~10-60 minutes total (30 replicates x 3 scenarios), depending on hardware.
4. Outputs appear in outputs/0.1/, outputs/0.5/, outputs/0.9

## Outputs:

- results.rda: full list of tracking data frames(fires, burnt area by LCT, forest age, etc.)
- Text tables: fires.txt, lc.burnt.txt, forest.age.burnt.txt
- Initial maps (scenario 0.1 only): maps/firerisk_r1_t0.tif, land.cover.type_run1_time0.tif, etc

## Adaptations from Original REMAINS

- Custom fire.risk(): explicit orography join, fine slope classes, NA handling, correct inclusion of economic damage in final risk score.
- No hidden dependencies
- Cleaner loading scenario management via loop

## Calibration Report from Calibración del submodelo de incendios forestales de REMAINS en la región Centro–Sur de Chile

This repository includes adaptations based on the ANIS Project Objective 4 report by Adrián Regos and Augusto Pablo Salonio Carbó.

**Methodology Highlights**

- Fire propagation: Ignition from risk layer; spread to 8 neighbors using polynomial model (fuel load from time since last fire, topography, land cover).
- Suppression: Fuel-based (spread rate threshold) and mosaic-based (1 ha agricultural/burnt areas).
- Calibration: 30 simulations for 3 suppression scenarios (10%, 50%, 90%). Adjusted burnt area distributions (factors 2.5 for Ñuble/Biobío, 2 for Araucanía) to match 1999–2018 historical data. Projections 2019–2038 using 2018 land cover. Climate change scenario: burnt areas ×4.

**Results Summary**

- Burnt areas 2019–2038 (50% suppression): Increase vs 1999–2018 – Araucanía +25.8% (41,650 ha), Ñuble +24% (29,700 ha), Biobío +15.5% (39,850 ha).
- Suppressed by fuel: Mixed changes (Ñuble +20%, Araucanía +2.8%, Biobío -3%).
- Suppressed by mosaics: Decreases (Ñuble -28.5%, Biobío -32.6%, Araucanía -6.2%).
- Climate change (×4): Larger burnt increases (Ñuble +26.1%, Biobío +18.2%, Araucanía +1.5%). Suppression limits highlighted, especially mosaics.

**Conclusion from Report**

Model robust for simulating Chilean fire regimes. Projects notable increases under baseline and climate scenarios. Urges adaptive landscape management integrating all submodules for holistic view.

## Acknowledgments & References

This adaptation is based on the original REMAINS model developed by the FirESmart project.

- **Original upstream repository**:  
  https://github.com/FirESmart-Project/REMAINS/tree/master

- **Original publication (please cite if you use core REMAINS logic)**:  
  Pais, S., Aquilué, N., Brotons, L., Honrado, J. P., Fernandes, P., Regos, A. (2023).  
  The REMAINS R-package: Paving the Way for Fire-Landscape Modeling and Management.  
  *Environmental Modelling & Software*. DOI: [https://doi.org/10.1016/j.envsoft.2023.105801]

- **Chile calibration & adaptation (ANID Project OE4, May 2025)**:  
  Regos, A., Salonio Carbó, A. P. (2025).  
  Calibración del submodelo de incendios forestales de REMAINS en la región Centro–Sur de Chile.  
  Proyecto ANID – Objetivo Específico 4.  
  CSIC / Misión Biológica de Galicia (MBG).  
  Available at: [link to PDF or Zenodo DOI if archived]

When publishing results obtained with this adaptation, please cite **both** the original REMAINS paper/repo and the ANID OE4 report above.

## Authorship

Chile-specific wildfire simulation module
Author: **Adrián Regos**
ORCID: https://orcid.org/0009-0005-9460-0764
Author & maintainer: **Augusto Pablo Salonio Carbó**  
ORCID: https://orcid.org/0009-0005-9460-0764  

Based on the original REMAINS model developed by the FirESmart project.

This adaptation was developed within **Proyecto ANID – Objetivo Específico 4**  
Authors of the calibration report: Adrián Regos & Augusto Pablo Salonio Carbó  
Institutions: CSIC / Misión Biológica de Galicia (MBG)

## Citation

Please see the `CITATION.cff` file for the preferred citation of this repository.
When publishing results obtained with this model, please cite **both** the original REMAINS paper and the ANID OE4 calibration report.
