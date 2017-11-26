# SPL_Project

Statistical Programming Languages Group Project

![image](https://imgs.xkcd.com/comics/machine_learning.png)

## Project Details

**RESEARCH QUESTION**: How is the labor force participation behavior of individuals aged 50-64 in 11 European countries influenced by different health indicators (both self-reported and tested)?

**Model**: Logistic regression

**Why is this of interest**: Older Europeans are experiencing increased longevity, better health outcomes, and more personal freedom compared to generations prior; it is of interest to us to find the observable (and unobservable?) health-related factors that influence their labor force participation behavior, using data collected by SHARE (the Survey on Health, Aging, and Retirement in Europe). Importance is on devising and implementing policies that aim to increase labor force participation of the elderly (some targets aim for 50% of this group).

**Description of data**: Cross-sectional data from Wave 1 of the SHARE dataset.

## Quantlet Proposals

### Section 1: Data and Descriptive Statistics

1. Data import, cleaning, imputation, prep variables for model input (dummy variables, standardization)
2. Summary statistics tables

### Section 2: Estimation

1. Apply standard probit model to each country and each gender - generate table with marginal effects and apply a Wald test to check joint impact of all health-related variables

### Section 3: Counter-Factual Exercise

1. Counter-factual exercise

### Section 4: Enhancements

1. Counter-Factual: Use current health outcomes as a benchmark for improved counter-factual.
2. Graphics
    - Show distribution for each country (and overlay with average) to visualize within-country inequalities (e.g. between East and West Germany). See [this](https://ourworldindata.org/wp-content/uploads/2017/04/The-distribution-of-life-satisfaction.png) for inspiration.
    - Show a choropleth of scores (for each metric metric) for countries contained within data set.
    - Measure [labor force participation over time](https://ourworldindata.org/grapher/share-of-people-who-say-they-are-happy) (using new waves of data).
    - Measure [health-related indicators](https://ourworldindata.org/wp-content/uploads/2017/04/Happiness-Inequality-Clark-et-al-2015.png) over time (using new waves of data). Consider using deciles to show inequalities.
    - [For each country](https://ourworldindata.org/wp-content/uploads/2017/04/Happiness-by-Income-Quintiles-Small-Multiples.png) (use small multiples) show how labor-force participation may change based on the distribution of health.
    - [Show correlation](https://ourworldindata.org/grapher/gdp-vs-happiness) of each indicator and labor-force participation on a scatterplot. Size the dots by country size.

Add notes on data quality and measurement. Do we believe the variables selected are meaningful?

## Team Members

1. Günther, Claudia
2. Nguyen, Phi
3. Winkel, Julian
