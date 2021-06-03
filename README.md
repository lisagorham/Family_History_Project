## Family_History_Project
This is all of the code for the family history interview project. 

#Master Family History Code
This document takes all of the raw data from SDQ and translates it into usable variables (i.e. if fixes the formatting of the family tree and matches each family member with their diagnoses so that you can create summary variables.

#Weeks of depression FHI Code
This document is all of the analyses for analyzing the relationship between family history and weeks of depression.

#Analysis Number 2 MFQ Code
This document is all of the MFQ analyses with family history.

#equivalence testing
This document is all of the equivalence tests. 

# Notebooks contains python code for fitting models and running cross validated predicitons

Use conda to create the environment:

```
conda create -c ejolly -c conda-forge -p ./env python pandas=1.1.5 numpy scipy pymer4 matplotlib seaborn jupyter notebook pymer4=0.7.3 statsmodels scikit-learn  
# activate the conda environment
conda activate ./env
```

Then you can use jupyter notebook to run the notebook in the notebook directory. The data directory contains cross-validated subject level predictions for each of the weeks of depression models and the mfq model. 