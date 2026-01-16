# Reproducible Manuscript Template

A Quarto template for writing reproducible academic manuscripts. This repository outputs your manuscript in three formats from a single source file:

- **HTML** - Interactive web version with code folding, hosted via GitHub Pages
- **PDF** - Publication-ready preprint using the [mvuorre/preprint](https://github.com/mvuorre/preprint) Typst extension
- **Word (.docx)** - For journal submissions that require Word format

## Getting Started

### Prerequisites

Before using this template, ensure you have the following installed:

1. **R** (version 4.0 or later recommended)
2. **Quarto** (version 1.4 or later) - [Download here](https://quarto.org/docs/get-started/)
3. **Positron** (recommended) or another IDE with Quarto support

### Initialize Your Project

#### 1. Create your repository

Click "Use this template" on GitHub to create a new repository, then clone it locally:

```bash
git clone https://github.com/YOUR-USERNAME/YOUR-REPO-NAME.git
cd YOUR-REPO-NAME
```

#### 2. Install the Typst preprint extension

The PDF output uses Matti Vuorre's preprint extension for Typst. Install it by running:
```bash
quarto add mvuorre/preprint
```

This creates an `_extensions/` directory in your project. Commit this directory to version control.

#### 3. Set up renv for package management

This template expects you to use [renv](https://rstudio.github.io/renv/) to manage R package dependencies. This ensures that anyone who clones your repository can reproduce your exact R environment.

In R, run:

```r
install.packages("renv")
renv::init()
```

This creates:
- `renv.lock` - A lockfile recording exact package versions
- `renv/` - A project-local library
- `.Rprofile` - Activates renv when you open the project

As you work, install packages normally (e.g., `install.packages("tidyverse")`), then periodically run `renv::snapshot()` to update the lockfile.

**Restoring an environment**: When someone clones your repository, they simply run `renv::restore()` to install all packages at the correct versions.

#### 4. Update project metadata

Edit `_quarto.yml` to set your repository URLs:

```yaml
repo-url: https://github.com/YOUR-USERNAME/YOUR-REPO-NAME
site-url: https://YOUR-USERNAME.github.io/YOUR-REPO-NAME
```

Edit `index.qmd` to add your title, authors, and affiliations.

## Project Structure

```
├── index.qmd           # Main manuscript file
├── _quarto.yml         # Quarto project configuration
├── references.bib      # Bibliography in BibTeX format
├── R/                  # R scripts for data processing and analysis
├── data/               # Raw and processed data files
├── _freeze/            # Frozen computation outputs (auto-generated)
├── _site/              # Rendered output (auto-generated, gitignored)
├── _extensions/        # Quarto extensions (e.g., preprint-typst)
└── .github/workflows/  # GitHub Actions for CI/CD
```

### The `R/` Directory

To keep your main manuscript readable, **abstract complex code into the `R/` directory**. This includes:

- Data cleaning and preprocessing scripts
- Custom functions used in the analysis
- Long or complex computations

In your manuscript, source these scripts, for example:

```r
source("R/data-cleaning.R")
source("R/custom-functions.R")
```

This separation makes `index.qmd` easier to read and review, while keeping all code version-controlled and reproducible.

## Rendering Your Manuscript

### Render all formats locally

```bash
quarto render
```

This produces output in `_site/` (HTML) and standalone PDF/Word files.

### Render a specific format

```bash
quarto render index.qmd --to html
quarto render index.qmd --to preprint-typst
quarto render index.qmd --to docx
```

### Understanding the freeze system

This template uses Quarto's [freeze](https://quarto.org/docs/projects/code-execution.html#freeze) feature (`freeze: auto` in `_quarto.yml`). When you render locally:

1. Quarto executes your R code
2. Results are saved to `_freeze/`
3. Subsequent renders reuse these frozen results unless the source changes

**Why this matters**: The GitHub Actions workflows render with `--no-execute`, meaning they rely entirely on the frozen outputs you commit. This approach:

- Avoids installing R and all dependencies in CI
- Ensures the published version exactly matches what you rendered locally
- Makes builds fast and reliable

**Your workflow**:
1. Make changes to your `.qmd` or R scripts
2. Run `quarto render` locally
3. Commit both your source changes AND the `_freeze/` directory
4. Push to GitHub

## Git Workflow

### Protect the main branch

To maintain a clean history and catch errors before they reach production, **protect your main branch**:

1. Go to your repository on GitHub
2. Navigate to Settings > Branches
3. Click "Add branch protection rule"
4. Set branch name pattern to `main`
5. Enable:
   - "Require a pull request before merging"
   - "Require status checks to pass before merging" (select the Quarto PR Check workflow)

### Making changes via pull requests

With branch protection enabled, all changes go through pull requests:

```bash
# Create a feature branch
git checkout -b feat/add-analysis

# Make your changes, render, and commit
quarto render
git add .
git commit -m "feat: add regression analysis"

# Push and create a pull request
git push -u origin feat/add-analysis
```

Then open a pull request on GitHub. The `quarto-pr.yml` workflow automatically checks that your changes render correctly.

### Commit message conventions

This template recommends [conventional commits](https://www.conventionalcommits.org/):

- `feat:` - New features or analyses
- `fix:` - Bug fixes
- `docs:` - Documentation changes
- `refactor:` - Code restructuring without changing functionality

## GitHub Actions

Two workflows are included:

### `quarto-pages.yml`
Runs on pushes to `main`. Renders the site (using frozen computations) and deploys to GitHub Pages.

### `quarto-pr.yml`
Runs on pull requests to `main`. Checks that the manuscript renders without errors, catching problems before they're merged.

### Enabling GitHub Pages

1. Go to Settings > Pages
2. Under "Build and deployment", select "GitHub Actions" as the source
3. The site will deploy automatically on the next push to main

## Tips for Reproducible Manuscripts

### Use relative paths
Always use relative paths for data and scripts:
```r
data <- read_csv("data/raw-data.csv")  # Good
data <- read_csv("/Users/me/project/data/raw-data.csv")  # Bad
```

### Document your environment
Besides `renv`, consider documenting:
- R version (check with `R.version.string`)
- Operating system used for final renders
- Any system dependencies (e.g., for certain R packages)

### Version control your data (when appropriate)
Small, non-sensitive datasets can live in `data/`. For larger or sensitive data:
- Use `.gitignore` to exclude the files
- Document where the data can be obtained
- Consider using a data repository (OSF, Zenodo, etc.)

### Cite your software
Use `grateful::cite_packages()` or similar to generate citations for R packages you use.

## Troubleshooting

### "Extension not found" error when rendering PDF
Run `quarto add mvuorre/preprint` to install the Typst extension.

### Changes not appearing on GitHub Pages
Ensure you've committed the `_freeze/` directory. The CI does not execute code.

### Package not found errors
Run `renv::restore()` to install packages from the lockfile, or `renv::snapshot()` after installing new packages.

## License

This template is released under the GPL-3.0 License. See [LICENSE](LICENSE) for details.
