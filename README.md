## Family_History_Project
This is all of the code for the family history interview project. 

#Weeks of depression FHI Code
This document has the demographic analyses and linear regressions for the weeks of depression analyses described in both the main paper and the supplement.

#Analysis Number 2 MFQ Code
This document has the demographic analyses and LMEs for the MFQ analyses described in both the main paper and the supplement.

#data and notebooks folders 
Have the rest of the code and data used for the analyses. 

# Notebooks contains python code for fitting models and running cross validated predicitons

Use conda to create the environment:

```
conda create -c ejolly -c conda-forge -p ./env python pandas numpy scipy pymer4 matplotlib seaborn jupyter notebook pymer4=0.7.3
# activate the conda environment
conda activate ./env
```

Then you can use jupyter notebook to run the notebook in the notebook directory. The data directory contains cross-validated subject level predictions for each of the weeks of depression models and the mfq model. 
