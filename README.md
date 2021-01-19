# Supplementary and replication materials for the paper <br /> "How to Poll Runoff Elections"
This repository contains supplementary and replication materials for the paper "How to Poll Runoff Elections" by Peter Selb, Sascha Göbel, and Romain Lachat, forthcoming in *Public Opinion Quarterly*.

## Abstract
We present a polling strategy to predict and analyze runoff elections using the 2017 French presidential race as an empirical case. We employ rejective probability sampling to identify a small sample of polling stations which is balanced with respect to past election results. We then survey the voters’ candidate evaluations in first-round exit polls. We poststratify the voter sample to first-round election returns to account for nonresponse and coverage issues, and impute missing candidate evaluations to emulate campaign learning. Finally, we redistribute the votes for eliminated competitors according to their supporters’ lower-order preferences. We validate the predictions against official results and other polls, and discuss the advantages and limitations of our approach in conclusion.

## Description of folders and files
* [Code](https://github.com/saschagobel/poq-how-to-poll-runoff-elections/tree/master/code): contains R code for replication of analyses in the paper and appendix
* [Data](https://github.com/saschagobel/poq-how-to-poll-runoff-elections/tree/master/data): contains data required for replication of analyses in the paper and appendix. Note that "id_posterior2" must be generated using the "07-additional-analyses.R" script and "validation_sample_large" must be generated using the "04-validation-sampling.R" script. These files exceed GitHub's file size limit.
* [Figures](https://github.com/saschagobel/poq-how-to-poll-runoff-elections/tree/master/figures): contains all figures presented in the paper and appendix.  

## Author information

**Peter Selb** (corresponding author) <br />
Department of Politics and Public Administration <br />
University of Konstanz <br />
Box 85 <br />
78457 Konstanz, Germany <br />
Email: peter.selb [at] uni-konstanz.de

**Sascha Göbel** (repository maintainer) <br />
Graduate School of Decision Sciences <br />
University of Konstanz <br />
Box 85 <br />
78457 Konstanz, Germany <br />
Email: sascha.goebel [at] uni-konstanz.de

**Romain Lachat** <br />
Cevipof <br />
Sciences Po <br />
98, rue de l'Université <br />
75007 Paris, France <br />
Email: mail [at] romain-lachat.ch