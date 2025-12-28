# Models Folder

This folder contains the R simulation codes used in my thesis for evaluating the performance of different models in hierarchical data analysis.

## Models and Files

- **RT:** Standard Regression Tree  
- **RF:** Random Forest  
- **LM:** Simple Linear Regression  
- **LME:** Linear Mixed Effects model  
- **MERT:** MERT model based on Hajjem (2011); the algorithm is inspired by the paper, but the implementation was adapted and refined by my supervisor and me  

## How to Use

1. Install the required R packages before running the codes:

```r
install.packages(c("lme4", "randomForest", "dplyr"))
