# Baseball-Playoffs-Data-

An R analysis of MLB team statistics (Lahman `Teams` data through 2020). It looks at playoff qualification, the Wild Card era, and which team stats explain wins and runs.

## Repository Contents

| File | Description |
|---|---|
| `more baseball scripts.R` | R script (run in RStudio) with the full analysis: regression models and ggplot2 charts |
| `Teams2020.csv` | Team-season dataset used for the analysis (2,955 rows) |

## Commit History

### `ca7c513`: Initial commit (2021-09-18)
- Created the repository with a one-line README.

### `fb27ae4`: Upload of the R analysis and dataset (2021-09-18)
- **Added the files:** `more baseball scripts.R` and `Teams2020.csv`.
- **Wins model:** built a multiple regression for team wins with backward elimination, removing insignificant stats one at a time (triples, saves, shutouts, ERA, home runs and others). The final model, `W ~ G + L + SF + ER + CG + HA + BBA + SOA + DP`, reached a 99.8% fit.
- **Wild Card analysis:**
  - the Red Sox benefited most when the Wild Card was added
  - most Wild Card teams from 1995–2012 needed about 95 wins to qualify, and 90 wins was the next most common
  - the AL East and NL East earned the most Wild Cards
- **Division titles:** Atlanta won the most, followed by the Dodgers and Yankees. Anaheim won the fewest among division winners.
- **Run scoring for playoff teams:**
  - correlations of walks with runs (moderate) and home runs with runs
  - a multiple regression, `R = X2B + X3B + HR + BB + SO + SB + SF`, in which every variable is statistically significant (82.7% correlation)
- **Visualizations:** about 17 ggplot2 charts (bar charts, histograms, scatterplots, line charts and a boxplot).

## Tools
R, RStudio, ggplot2
