# ST 495: Advanced Computing for Statistical Methods
This project is based on a dataset that originates from the StatLib library at Carnegie Mellon University about city-cycle field consumption. I developed a model fitting procedure to predict city-cycle fuel consumption based on provided population features. To assess the performance of this estimation procedure, I constructed a simulation study. This involves simulating the estimation procedure numerous times from synthetically generated data. Then, I reconstructed the simulation study with a bootstrapping approach. This involves subsampling from the observed data numerous times before conducting the same estimation procedure.

Please take a look at the ST_495_Final_Project.pdf for a description of my analysis.

The data set I used is publicly available on the UCI Machine Learning Repository (https://archive.ics.uci.edu/dataset/9/auto+mpg). 

The code used in this project has the following files and methods: 
1. RealDataAnalysis.r
2. SimStudy.r
3. SimStudyResults.r
4. Bootstrap.r
5. BootstrapResults.r
6. workflow.sh

