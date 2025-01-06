# This is the workflow to reproduce the results of the ST 495 final project by
# Bryant Willoughby

#EDA & analysis for real dataset (plots and tables included)
Rscript RealDataAnalysis.r

#synthetic data generation & simulation study design 
Rscript SimStudy.r

#All plots and output for simulation study 
Rscript SimStudyResults.r

# bootstrapping linear regression data 
Rscript Bootstrap.r

# All plots and output for boostrapping results 
Rscript BootstrapResults.r