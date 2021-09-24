# CAS
A repository containing code for summarizing, modeling, and predicting with the CAS data.

Data was provided by a private insurance carrier to the CAS after anonymizing the data set. This data is available to actuarial researchers for well-defined research projects that have universal benefit to the insurance industry and the public. 
Our hope is that the methods demonstrated here can be a good foundation for future ratemaking models to be developed and tested more efficiently.
Please contact Brian Fannin (bfannin@casact.org) at the CAS if you have a project proposal.

## Setup

In order to run the server and analysis scripts, ensure that you have the following R packages installed
```{r}
c(
  “dplyr", 
  “purrr", 
  “stringr", 
  “usethis", 
  “data.table", 
  “fs", 
  “tidyr", 
  “ggplot2", 
  “sparklyr", 
  “tictoc", 
  “BBmisc", 
  “caret", 
  “h2o", 
  “readr", 
  “rlang”, 
  "furrr",
  "future",
  "mshap",
  "magrittr",
  "patchwork"
)
```

In addition, for the python scripts the following modules are required:
```{python}
logging
numpy
pandas
sklearn
shap
```

Note that in order to correctly install `{h2o}` as it will be used in the scripts, run the following code:

```{r}
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Then, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
```

## The Data

This data is split into three different data sets:

* Bodily Injury (BI) which is contained in the `CAS_RAW_1_*.txt` files
* Property Damage (PD) which is contained in the `CAS_RAW_2_*.txt` files
* Collision (COLL) which is contained in the `CAS_RAW_3_*.txt` files

At the top of each file, there is a line to specify which data set is to be worked on in the file.
All files should work with all data sets.

The data consists of the columns `EARNED_EXPOSURE`, `ULTIMATE_AMOUNT`, `ULTIMATE_CLAIM_COUNT`, and `X_VAR1`:`X_VAR46`.

* `EARNED_EXPOSURE` is the amount of time the policy was taken out for.
* `ULTIMATE_AMOUNT` is the ultimate cost of the policy, in US dollars.
* `ULTIMATE_CLAIM_COUNT` is the number of claims filed for the given policy.  It is often not a round number and is therefore rounded when cleaning the data (See Rscripts_Server/01_clean_all_data.R).
* `X_VAR1`:`X_VAR46` are 46 unidentified variables, all of which are discrete (ordinal numeric or text string). It should be noted that we do know that X_VAR27 is state and X_VAR41 is year. For further summaries and details on these variables, see inst/summaries/*_fac_sum.csv or Rscripts_Server/02_summarize_data.R.

We also note here that `X_VAR19`, `X_VAR34`, and `X_VAR46` have numerous levels and are often left out of the modelling process for this reason.

In the data, there are quite a few rows (policies) with zero or negative EARNED_EXPOSURE.
For most of our analyses, these are simply filtered out, but a table describing these values is shown here for convenience.

|Description| Num Records| % of Raw|EARNED_EXPOSURE|ULTIMATE_AMOUNT|% of ULT AMT of raw|ULTIMATE_CLAIM_COUNT|% of ULT CNT of raw|
|:--|:--|:--|:--|:--|:--|:--|:--|
|BI|30,342,067|100%|3,830,558|634,080,483|100.00%|32,293|100.00%|
|Subset of raw_1 with 0 exposures|6,724,652|22.16%|-|6,958,737|1.10%|367|1.14%|
|Subset of raw_1 with negative exposures|3,885,178|12.80%|(33)|10,848,560|1.71%|606|1.88%|
|PD|20,201,841|100.00%|2,665,037|520,665,847|100.00%|151,842|100.00%|
|Subset of raw_2 with 0 exposures|4,138,323|20.48%|- |6,981,221|1.34%|1,898|1.25%|
|Subset of raw_2 with negative exposures|2,590,939|12.83%|(129)|9,330,567|1.79%|2,487|1.64%|
|COLL|30,285,873|100.00%|3,835,828|443,291,671|100.00%|135,419|100.00%|
|Subset of raw_3 with 0 exposures|6,634,314|21.91%|- |5,078,430|1.15%|1,621|1.20%|
|Subset of raw_3 with negative exposures|3,889,473|12.84%|(118)|7,738,811|1.75%|2,291|1.69%|


## The `CODE_*` Directories

There are three types of code files, and they are housed in directories specific to their functions.

* `CODE_Analysis` contains R scripts that are meant to provide a starting point for analyzing data that has been output by the scripts in `CODE_Server`. These scripts are numbered in accordance with the numbering in `CODE_Server`.
* `CODE_Reference` contains RMarkdown documents that are useful for referential matters, such as installing Spark or H2O. The files can be used to give general instruction on how to employ the methods used in this repository, with the specific aim of pulling information from the CAS data.
* `CODE_Server` contains R scripts that are meant to be run on a server (or a machine with a large amount of RAM). These scripts are computational, and they run on their own.  In other words, once started, they will save relevant output to the locations supplied at the beginning of the file.  Each of these files has a section at the top titled "User Inputs" where various variables and values should be edited to fit the configuration of the machine you are using.

## The `output` Directory

Files in this directory are direct outputs from the R scripts in `CODE_Server`, with the exception of the `img` sub-directory, which contains images used in the paper.
File names are descriptive and should give the user a good idea of what the file contains, including tuning results, subsets of predictions on the test data, computed shap values on the test data, etc.

## Reference

* [CAS Website](https://www.casact.org/)
* Article that Accompanies the Repo
* [H2O reference Page](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html)
* [Spark Home Page](https://spark.apache.org/)
* [Sparklyr Rstudio Page](https://spark.rstudio.com/)
* [SHAP Github Page](https://github.com/slundberg/shap)
* [MSHAP Github Page](https://github.com/srmatth/mshap)
* [MSHAP Paper on arXiv](https://arxiv.org/abs/2106.08990)

This repository is authored and maintained by [Spencer Matthews](https://github.com/srmatth) with the help and support of [Brian Hartman](https://github.com/brianmhartman) and [Brian Fannin](https://github.com/PirateGrunt).

