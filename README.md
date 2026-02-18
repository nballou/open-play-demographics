# Multi-platform Trace Data Reveal Demographic Differences in Video Game Play, but Individuals Vary Far More

This repository contains the reproducible manuscript and analysis code for a descriptive study examining how video game play patterns differ across demographic groups.

The rendered manuscript is available at [https://nballou.github.io/open-play-demographics/](https://nballou.github.io/open-play-demographics/).

## Overview

Most knowledge about "who plays video games" comes from surveys subject to recall bias and social desirability effects. This study uses publicly available behavioral logs from Steam, Xbox, and Nintendo—spanning 1.5 million hours from 3,768 US and UK adults (18–40 years old)—to examine how play patterns differ according to age, gender, ethnicity, and neurodiversity.

Key findings include:
- Older players' peak playtime occurs approximately 1 hour earlier in the day
- Women tend to re-engage with the same game over longer time periods
- Genre preferences vary across demographic groups (e.g., sports games, simulation, RPGs)
- Despite observable group-level trends, demographic categories explain at most ~5% of behavioral variance
- Within-group variation far exceeds between-group differences across all outcomes

## Repository Structure

```
├── index.qmd           # Main manuscript (Quarto document)
├── _quarto.yml         # Quarto project configuration
├── references.bib      # Bibliography
├── R/
│   └── helpers.R       # Helper functions for visualizations and analysis
├── data/               # Data downloaded from Zenodo during render
├── _extensions/        # Quarto extensions (ACM format, preprint-typst)
├── _freeze/            # Frozen computation outputs
└── site_libs/          # Site dependencies
```

## Data

This analysis uses data from the [Open Play dataset](https://doi.org/10.5281/zenodo.17536656), which contains multi-platform digital trace data from US and UK video game players. The data is automatically downloaded from Zenodo when rendering the manuscript.

Key data components used:
- **Digital trace data**: Hourly and session-level gameplay records from Steam, Xbox, and Nintendo Switch
- **Demographic data**: Age, gender, ethnicity, and neurodiversity status from intake surveys
- **Game metadata**: Genre classifications harmonized across platforms

## Reproducing the Analysis

### Prerequisites

1. **R** (version 4.0 or later)
2. **Quarto** (version 1.4 or later) — [Download here](https://quarto.org/docs/get-started/)

### Setup

1. Clone this repository:
   ```bash
   git clone https://github.com/nballou/open-play-demographics.git
   cd open-play-demographics
   ```

2. Restore the R environment:
   ```r
   renv::restore()
   ```

3. Render the manuscript:
   ```bash
   quarto render
   ```

The first render will download the data from Zenodo (~194MB), which may take a few minutes.

### Output Formats

The manuscript renders to multiple formats:
- **HTML** — Interactive web version with code folding
- **PDF (Typst)** — Preprint format (`index-typst.pdf`)
- **PDF (ACM)** — Conference submission format (`index-acm.pdf`)
- **Word** — For journal submissions requiring `.docx`

To render a specific format:
```bash
quarto render index.qmd --to html
quarto render index.qmd --to preprint-typst
quarto render index.qmd --to acm-pdf
```

## Key Analyses

The manuscript includes:

1. **Play volume analysis** — Weekly hours, session counts, and session duration across demographic groups
2. **Engagement patterns** — How long players stick with individual games
3. **Temporal patterns** — Time-of-day and day-of-week play distributions
4. **Genre preferences** — Radar charts showing relative genre allocations by demographic group
5. **Intersectional analysis** — Heatmap examining combinations of demographic characteristics
6. **Variance decomposition** — Eta-squared and R² quantifying how much demographics explain

## Citation

If you use this code or analysis, please cite:

```
Ballou, N. (2026). Multi-platform trace data reveal demographic differences in video
game play, but individuals vary far more. Proceedings of the ACM SIGCHI Annual
Symposium on Computer-Human Interaction in Play (CHI PLAY '26).
```

## License

Code: GPL-3.0
Manuscript content: CC-BY-4.0

## Contact

Nick Ballou — [nick@nickballou.com](mailto:nick@nickballou.com) — [@nikiballou](https://twitter.com/nikiballou)
