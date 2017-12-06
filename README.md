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
    - 11 countries: Austria, Belgium, Denmark, France, Germany, Greece, Italy, the Netherlands, Spain, Sweden, Switzerland (**country_mod**)
    - Filter: Only men and women between 50 and 64, only wave 1
    - Select: (8 health indicators):
        1. Serious Health Condition (**chronic_mod**)
        2. Mild Health Condition (**chronic_mod**)
        3. Activities of Daily Living (**adla**)
        4. Max. Grip Strength (**maxgrip**)
        5. Overweight (**bmi2**)
        6. Obese (**bmi2**)
        7. Bad Mental Health (**eurod**)
        8. Good Self-Perceived Health (**sphus**)
    - Select: (demographic variables):
        1. Secondary education (**isced1997_r**)
        2. Higher education (**isced1997_r**)
        3. Children (**ch001_**)
        4. Marriage Status (**mar_stat**)
    - Outcome: Labor participation (**ep013_mod**)
    - Standardize:
        1. Grip strength (**maxgrip**)
        2. Number of children (**ch001_**)
        3. Number of chronic diseases (**chronic_mod**)

2. Summary statistics tables
    - Tables to be built (pages indicate location in paper):
        1. Sample statistics and age classes per country (p. 17)
            1. number of observation per country
            2. percentage of observation within each country
            3. stratified by age groups:
                1. Age 50-54, Age 55-59, Age 60-64
        2. Health indicators per country (p. 17-18)
            1. percentage of entire sample (Europe) within category of each of eight health indicators
        3. Labour force participation men per country (p. 18)
            1. percentage of working men within each country stratified by age groups:
                1. Age 50-54, Age 55-59, Age 60-64
        4. Labour force participation women per country (p. 19)
            1. percentage of working women within each country stratified by age groups:
                1. Age 50-54, Age 55-59, Age 60-64
        5. Labour supply choice men (p. 19)
            1. percentage of men withhin each country stratified by labor groups:
                1. Nonparticipation, Half Time, Full Time
        6. Labour supply choice women (p. 20)
        1. percentage of men withhin each country stratified by labor groups:
                1. Nonparticipation, Half Time, Full Time

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
