# GPT Party

Replication repository for the GPT Party project, which compares GPT-4 party placements with expert survey benchmarks from GPS and CHES.

## What's in this repo

- `code/gps/`: data prep, prompting, and analysis scripts for Global Party Survey comparisons.
- `code/ches/`: harmonization and analysis scripts for Chapel Hill Expert Survey comparisons.
- `data/`: intermediate and analysis-ready datasets used by the scripts.

## Getting started

1. Open the project in RStudio (`article_spsa2024_gpt_party.Rproj`) or from your R session.
2. Install required packages used in the scripts (`dplyr`, `tidyr`, `ggplot2`, and related dependencies).
3. Run scripts in `code/gps/` and `code/ches/` in numerical/file-order when dependencies exist between files.

## Notes

- This repository is intended for transparency and replication.
- If you reuse results, please cite the corresponding paper/project output.
