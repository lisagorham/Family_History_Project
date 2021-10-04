# Family_History_Project
This is all of the code for the family history interview project. 

## Weeks of Depression Final Code for Github
This document has the demographic analyses and linear regressions for the weeks of depression analyses described in both the main paper and the supplement.

## MFQ Final Code for Github
This document has the demographic analyses and LMEs for the MFQ analyses described in both the main paper and the supplement.

## Weeks of Depression Sensitivity Analsyses Final Code
This document has three sensitivity analyses for the Weeks of Depression dataset: using only diagnosis of depression, using anxeity instead of depression, and excluding all inpatients. These analyses are described in the supplement.

## MFQ Sensitivity Analsyses Final Code
This document has three sensitivity analyses for the MFQ dataset: using only diagnosis of depression, using anxeity instead of depression, and excluding all inpatients. These analyses are described in the supplement.

## MFQ Linear Discriminant Analysis Final Code
This document has the linear discriminant analysis using the MFQ data described in the supplement.

# data and notebooks folders 
Have the rest of the code and data used for the analyses. 

## Notebooks contains python code for fitting models and running cross validated predicitons

Use conda to create the environment:

```
conda create -c ejolly -c conda-forge -p ./env python pandas=1.1.5 numpy scipy pymer4 matplotlib seaborn jupyter notebook pymer4=0.7.3 statsmodels scikit-learn  
# activate the conda environment
conda activate ./env
```

Then you can use jupyter notebook to run the notebooks in the notebook directory. The data directory contains cross-validated subject level predictions for each of the weeks of depression models and the mfq model. 


In addition you will need to use r to install simr if you would like to run the mfq power analysis notebooks.

Here is the relationship between notebooks and figures/analyses in the paper:
* get_cv_predictions.ipynb
    * Figure 1&2, Table S1-S5, Table S7-S19, Supplemental Figures FigS1-S6 & FigS16
* Eval_power_mfq.ipynb
    * Supplemental Materials, Power Analysis for MFQ; Supplemental Figure FigS14
* Eval_power_weeks.ipynb
    * Supplemental Materials, Power Analysis for Weeks of Depression; Supplemental Figure FigS15
* get_cv_predictions_dx_sensitivity.ipynb
    * Supplemental Materials, Sensitivity Analyses (use only diagnosis of depression); Supplemental Figure FigS7
* get_cv_predictions_anx_sensitivity.ipynb
    * Supplemental Materials, Sensitivity Analyses (use anxiety instead of depresssion); Supplemental Figure FigS9
* get_cv_predictions_no_inpatient_sensitivity.ipynb
    * Supplemental Materials, Sensitivity Analyses (exclude all inpatients); Supplemental Figure FigS11
* get_cv_predictions_mfq_dep_immed_dx.ipynb
    * Supplemental Materials, Sensitivity Analyses (use only diagnosis of depression); Supplemental Figure FigS8
* get_cv_predictions_mfq_anx_immed_dx.ipynb
    * Supplemental Materials, Sensitivity Analyses (use anxiety instead of depresssion); Supplemental Figure FigS10
* get_cv_predictions_mfqNOINPATIENT.ipynb
    * Supplemental Materials, Sensitivity Analyses (exclude all inpatients); Supplemental Figure FigS12
* get_cv_predictions_try_elasticnet.ipynb, get_cv_predictions_try_extratrees.ipynb, get_cv_predictions_try_mfqsingle.ipynb, compare_approaches.ipynb
    * Supplemental Materials, Sensitivity Analyses (comparison of different regression approaches); Supplemental Figure FigS13
* chance_lda_performance.ipynb
    * Supplemental Materials, Sensitivity Analyses (LDA chance performance)
