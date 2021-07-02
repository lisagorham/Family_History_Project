## Family_History_Project
This is all of the code for the family history interview project. There are two sets of analyses, one for weeks of depression and one for the MFQ. If you are interested in following the analyses laid out in the paper, use the following documents. Please note that the results you find will be different than what you see reported in the paper, due to the fact that we had to truncate all of our participant's ages in order to upload a PII free dataset. 



#Weeks of Depression Final Code for Github
This document gets the demographic data on the weeks of depression dataset and also runs the linear regressions for each of the five models. 

#get_cv_predictions (within the notebooks folder)
This document does the cross validation for both the weeks of depression and MFQ analyses. It spits out a file that is saved under the data folder that is then used for equivalence testing. 

#Weeks of Depression Cross Validated Equivalence Testing
This document takes the Weeks of Depression output file created by the get_cv_predictions script (saved in the data folder) and does equivalence testing to compare the 5 different weeks of depression models. 




#MFQ Final Code for Github
This document gets the demographic data on the MFQ dataset and also runs the LMEs for each of the models. 

#get_cv_predictions (within the notebooks folder)
This document does the cross validation for both the weeks of depression and MFQ analyses. It spits out a file that is saved under the data folder that is then used for equivalence testing. 

#MFQ Cross Validated Equivalence Testing
This document takes the MFQ output file created by the get_cv_predictions script (saved in the data folder) and does equivalence testing to compare the different MFQ models. 




# Notebooks contains python code for fitting models and running cross validated predicitons

Use conda to create the environment:

```
conda create -c ejolly -c conda-forge -p ./env python pandas=1.1.5 numpy scipy pymer4 matplotlib seaborn jupyter notebook pymer4=0.7.3 statsmodels scikit-learn  
# activate the conda environment
conda activate ./env
```

Then you can use jupyter notebook to run the notebook in the notebook directory. The data directory contains cross-validated subject level predictions for each of the weeks of depression models and the mfq model. 
