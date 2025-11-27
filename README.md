# NetMetaEasy core functions

This repository contains the **core R functions** used in the online network meta-analysis tool:
https://metaanalysisonline.com/netmetaeasy/
The goal of this repo is **not** to provide a full R package or Shiny application, but to expose the **backend computational logic** of the NetMetaEasy web tool in a reusable form.
---
## What this repository contains
The R scripts in this repo implement the main analytical steps used in the online application:
- **`netmeta_core.R`**  
  Core wrappers around `netmeta` / `netmetabin` for:
  - binary outcomes (RR, OR),
  - continuous outcomes (SMD, MD, ROM),
  - time-to-event outcomes (HR, RR, OR),
  and both **2-arm** and **multi-arm (3+ arm)** study designs.
- **`league_table.R`**  
  Helper functions to:
  - generate league tables using `netleague()`,
  - optionally back-transform estimates,
  - mark statistically significant comparisons (95% CI excluding the null).
- **`netsplit_helpers.R`**  
  Utility functions for:
  - extracting full net-splitting results (direct, indirect, NMA, RoR, z, p),
  - generating a compact table for reporting p-values and RoR.
- **`netgraph_helpers.R`**  
  Logic for:
  - computing per-treatment sample sizes from the input data,
  - mapping sample sizes to node sizes (`cex.points`) for `netgraph()`.
- **`network_legend_clean.R`**  
  A clean, publication-ready legend for:
  - node size ~ number of participants,
  - edge thickness ~ number of studies.
- **`netforest_plot.R`**  
  A forest-plot wrapper around `netmeta::forest()` that mirrors the behaviour of the online tool:
  - choice of reference treatment,
  - random vs fixed effects,
  - optional ranking (e.g. P-score-based ordering),
  - configurable x-axis limits and labels.
- **`funnel_egger.R`**  
  Helper functions to:
  - extract pairwise TE / SE values,
  - run Egger’s test via `metagen()` + `metabias()`.
---
## Important note
> This repository does **not** contain:
> - the Shiny user interface,
> - any data import/export logic,
> - complete application wiring.
It only exposes the **computational core** so that advanced users can:
- reproduce analyses run via the web interface,
- integrate the same methods in their own R scripts or pipelines,
- inspect and modify the analytical steps used by the online tool.
---
## How to use these functions
1. Clone or download this repository from GitHub.
2. In R, source the functions you need, for example:
```r
source("R/netmeta_core.R")
source("R/league_table.R")
source("R/netsplit_helpers.R")
source("R/netgraph_helpers.R")
source("R/netforest_plot.R")
source("R/funnel_egger.R")
source("R/network_legend_clean.R")
