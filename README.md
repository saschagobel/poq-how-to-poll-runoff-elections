# Supplementary and replication materials for <br /> "Instant Runoff Forecasting"
This repository contains supplementary and replication materials for the paper "Instant Runoff Forecasting" by Peter Selb, Romain Lachat, and Sascha Göbel, forthcoming in [Public Opinion Quarterly](URL).

## Abstract
We present a polling strategy to predict and analyze runoff elections using the 2017 French presidential race as an empirical case. We employ replicate probability sampling based on auxiliary information from past elections to identify a sample of as few as 20 out of 65,000 polling stations that reflects the national electorate well. We then survey the voters’ candidate evaluations in first-round exit polls. We post-stratify the voter sample to first-round election returns while imputing missing candidate evaluations to emulate campaign learning. Finally, we redistribute the votes for eliminated competitors according to their supporters’ lower-order preferences. The approach yields individual-level predictions which are consistent with standard theories of voting behavior. Our aggregate-level predictions outperform most other polls and aggregations. The strategy is less vulnerable to survey errors and is a fraction of the cost compared to common polls. Finally, the data produced offer analytic potential beyond its original use for election forecasting.

## Description of folders and files
code
- contains R code for data processing, sampling, forecasts, validation, figures, and additional analyses

data
- contains all data required for replication of all steps of the analyses, see R scripts in code folder for details. Note that "id_posterior2" must be generated using the "07-additional-analyses.R" script and that "fpe2002.txt" must be unzipped before running scripts that import these files.

figures
- contains all figures (.png and. pdf) presented in the paper plus some extra

online-appendix

## Author information
**Peter Selb** (corresponding author) <br />
Department of Politics and Public Administration <br />
University of Konstanz <br />
Box 85 <br />
78457 Konstanz, Germany <br />
Email: peter.selb [at] uni-konstanz.de

**Romain Lachat** <br />
Cevipof <br />
Sciences Po <br />
98, rue de l'Université <br />
75007 Paris, France <br />
Email: mail [at] romain-lachat.ch

**Sascha Göbel** (repository maintainer) <br />
Graduate School of Decision Sciences <br />
University of Konstanz <br />
Box 85 <br />
78457 Konstanz, Germany <br />
Email: sascha.goebel [at] uni-konstanz.de