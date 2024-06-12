# Trends in Cardiovascular Risk Factors in Canada: Variation by Migration and Temporal Factors, 2001-2018

## Purpose of this repository

This repository contains all data and code necessary to reproduce the analyses presented in the manuscript "[Trends in Cardiovascular Risk Factors in Canada: Variation by Migration and Temporal Factors, 2001-2018](https://doi.org/10.1016/j.cjco.2024.04.006)" by Chen et al. (doi: [10.1016/j.cjco.2024.04.006](https://doi.org/10.1016/j.cjco.2024.04.006)). This includes all code for tables, figures, and supplementary material. Supplementary material output related to bootstrapped response can be found on [Open Science Framework](https://osf.io/k26cf/). Supplementary material related to table outputs can be found on [Canadian Journal of Cardiology - Open](https://www.cjcopen.ca/cms/10.1016/j.cjco.2024.04.006/attachment/367a51a7-3a02-4220-afe9-78085c9f7550/mmc1.docx). Note that the included scripts produce additional analysis beyond those directly presented in the manuscript. The CCHS has a [Statistic Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence).

## Requirements

All code is written in the programming language [R](https://www.r-project.org/). The easiest way to run it is to use the [RStudio](https://rstudio.com/) IDE. An `.Rproj` file is included with this repository for ease of use with RStudio. The scripts should run with any modern version of R.

The R packages required to reproduce the tables and figures are listed at the top of their respective scripts. They must be installed using `install.packages` or similar functionality within RStudio prior to running the script.

## Reproducing tables and figures

Run `harmonized_data.R` to create metadata for the study.

Run Rmarkdown files in the `analysis` folder to create the output tables and figures.

## Tables

- [Table S2](analysis/Table2-Descriptive-Statistics.Rmd)
- [Table S3](analysis/Table3-Age-Sex-Prevalence.Rmd)
- [Table S4](analysis/Table4-Age-Prevalence.Rmd)
- [Table S5](analysis/Table5-Regional-Prevalence.Rmd)

## Figures

- [Figure 1](analysis/Figure1&2-Risk-Factor-Plots.Rmd)
- [Figure 2](analysis/Figure1&2-Risk-Factor-Plots.Rmd)


## Supplementary

- [Response Rate](analysis/Appendix-2+Risk-Factors.Rmd)
- [2+ Risk Factors](analysis/Appendix-Heart-Disease.Rmd)
- [Heart Disease](analysis/Appendix-Response.Rmd)
