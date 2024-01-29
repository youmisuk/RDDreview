# Regression Discontinuity Designs in Education: A Practitioner's Guide

Youmi Suk

## Overview

Regression discontinuity (RD) designs have gained significant popularity as a quasi-experimental device for evaluating education programs and policies. In this paper, we present a comprehensive review of RD designs, focusing on the continuity-based framework, the most widely adopted RD framework. We first review the fundamental aspects of RD designs, drawing on potential outcomes and causal graphs. We then discuss the validity threats in RD designs, including manipulation, discreteness of the running variable, statistical power, and generalizability. Additionally, we provide an overview of the existing extensions to RD designs. To exemplify the application of RD methods, we analyze the effect of New Jersey's pre-kindergarten program on children's vocabulary test scores, using an educational dataset. Finally, we offer practical guidelines in the conclusion to promote the appropriate use of RD methods in educational research.

Please find our paper [here](https://doi.org/10.31234/osf.io/rhxs7). 
Here, we provide `R` codes to replicate our data analysis, but with the synthetic data that is similar to the original data used in the paper. 

## New Jersey's Pre-K Program Study

#### `synNJdata.csv`

- The data provided is not the original, real data, but synthetic data created using a generative AI model. This synthetic data captures the statistical properties of their real-world counterparts while preserving privacy.
   - The average similarity measure between real data and synthetic data is 0.93, with each variable's similarity ranging from 0.86 to 0.99. Note a variable's similarity value can vary between 0 and 1. 


| Variables   | Description |
| ----------- | ----------- |
| ppvt      | PPVT score (vocabulary test score; outcome)       |
| cav  | centered assignment variable (running variable)        |
| assign  | treatment assignment status |
| treat  | treatment receipt status |
| childgen    | child's gender |
| testtyp  | indicator of English (PPVT) and Spanish version (TVIP) |
| childethnic  | child's ethnicity |
| flunch  | free lunch status |


#### `DataAnalysisCodes.R` 
 
- `DataAnalysisCodes.R` file can be used to replicate our data analysis. The analysis results using the synthetic data are not exactly the same as those included in the paper based on the real data. But this code file can be seamlessly used for following our analysis steps and visualizing the results. 
   
#### `Functions.R` 
 
- `Functions.R` file contains three functions for RD analysis. These functions were originally developed by  Dr. Peter Steiner (University of Maryland-College Park), with only minor revisions subsequently made by the first author. This R script should be loaded prior to conducting the main RD analysis.
