
# Load my real data set 
carsdata <- read.csv("cleancarsdata.csv")

# Define my variable(s)
y_real <- carsdata$mpg
ln_y_real <- log(y_real) #this is ln(); log base e

# Define my covariates
X <- cbind(1, carsdata$horsepower, carsdata$weight)

# Fit my model using the real data
XtX_inv <- solve(t(X) %*% X)
beta_hat_real <- XtX_inv %*% t(X) %*% ln_y_real
cat("estimated coefs for real data = ",beta_hat_real,"\n")

p = dim(X)[2]
n = dim(X)[1]
sigma_sq_hat_real = sum((ln_y_real - X %*% beta_hat_real)^2) / (n-p)
cat("estimated variance for real data = ",sigma_sq_hat_real,"\n")

# Simulate a large number of synthetic data sets resembling my real data.
# Fit my model on each synthetic data set.
# Evaluate, summarize, plot, etc., the sampling distribution of my estimator(s) across the synthetic data sets. 
num_sims = 10000
beta_hat_mat = matrix( NA, nrow=num_sims, ncol=p)
set.seed(1)
for(k in 1:num_sims){
  ln_y_sim = X %*% beta_hat_real + rnorm( n, mean=0, 
                                          sd=sqrt(sigma_sq_hat_real))
  beta_hat_mat[k,] = XtX_inv %*% t(X) %*% ln_y_sim
}

sim_results = list( beta_hat_real=beta_hat_real,
                    sigma_sq_hat_real=sigma_sq_hat_real,
                    X=X,
                    beta_hat_mat=beta_hat_mat)

save( sim_results, file="sim_results.rda")

