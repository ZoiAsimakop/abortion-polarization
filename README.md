# Abortion and Polarization

An agent-based simulation in R of how activist campaigns and backlash can polarize public opinion about abortion, with an interactive Shiny app and a statistical analysis of the simulated data.

**Research question:** How do campaign intensity (weak vs. strong) and backlash (off vs. on) affect a campaign's effect on opinions and the polarization of opinions about abortion?

**Main finding (in the model):** Without backlash, a pro-choice campaign moves opinions only slightly. With backlash, a strong campaign pushes most people *away* from its message and polarizes the population: 94% of agents end up with extreme opinions, compared with 12% without backlash.

This is an explanatory model of how opinions *could* change under its assumptions, not a prediction about real people.

## 🏘️ Welcome to the town of Haters!

> *"This is a community where we love to discuss abortion and polarize about it. [...] We are not so excited about the issue of abortion as much as about the sheer thrill of hating on each other and telling our neighbours that they are surely wrong about everything they believe in!"*

Our Shiny app lets you visit this town and watch its inhabitants polarize, or not, as you move the sliders. Start it in RStudio with `shiny::runApp(shiny::shinyAppFile("simulation_code.R"))`, and read the town's own guided tour in **[docs/app_guide.md](docs/app_guide.md)** (also included at the end of `main.qmd`).

## How the model works

200 agents each hold an opinion from −1 (pro-life) to +1 (pro-choice). At every time step, each agent talks to a random other agent and moves toward their opinion, but only if it is close enough to their own (bounded confidence). Activists run a pro-choice campaign; agents who notice it move toward it if the message is close enough to their view. If backlash is switched on, agents who reject the message move *away* from it. Zealots never change their opinion. See `main.qmd` for the full description.

## Getting started

### 1. Install the requirements

- [R](https://cran.r-project.org/) (≥ 4.2) and [RStudio](https://posit.co/download/rstudio-desktop/) (includes Quarto)
- R packages, installed once in the R Console:

```r
install.packages(c("ggplot2", "scales", "knitr", "rmarkdown", "shiny", "plotly", "testthat"))
```

### 2. Get the project

Clone the repository and open `abortion-polarization.Rproj` in RStudio:

```bash
git clone https://github.com/ZoiAsimakop/abortion-polarization.git
```

### 3. Run the main document

Open `main.qmd` and click **Render**. It explains the model, shows an example run, loads the experiment results and presents the analysis and plots. Rendering takes under a minute, because the experiment results are saved in `data/`. If those files are deleted, the experiment reruns automatically (about 8 minutes).

### 4. Explore the app

```r
shiny::runApp(shiny::shinyAppFile("simulation_code.R"))
```

Or open `simulation_code.R` and click **Run App**. See the town of Haters guide above for a walk-through of all sliders.

## Repository structure

```
abortion-polarization/
├── main.qmd                   # main deliverable: model, experiment, results
├── simulation_code.R          # the model (sections 1–4) and the Shiny app
├── src/                       # functions used by main.qmd
│   ├── experiment.R           #   run the experiment, save/load results
│   ├── analysis.R             #   paired analysis, effect sizes
│   ├── plots.R                #   ggplot figures
│   ├── results_report.R       #   results text, tables and figures
│   └── example_runs.R         #   single example run for illustration
├── data/                      # saved experiment results
├── tests/testthat/            # tests for the experiment and analysis code
├── run_experiment_script.R    # optional: rerun the experiment from scratch
├── docs/app_guide.md          # guide to the Shiny app
└── references.bib             # references cited in main.qmd
```

To run the tests: `testthat::test_dir("tests/testthat")`.

## Authors and roles

- **Zoi Asimakopoulou**: project structure, README and `main.qmd`
- **Soma Breitner**: simulation model and Shiny app
- **Vanessa Filip**: experiment, statistical analysis and plots

Research Master Psychology, University of Amsterdam: *Programming in Psychological Science* (2026).
