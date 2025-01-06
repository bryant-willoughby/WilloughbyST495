
# Bootstrapping for linear regression

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

# First we will sub-sample from the joint distribution of (x_i, y_i)
m = 70 # size of each bootstrap sample
num_boot_samples = 1000
bootstrap_beta = matrix( NA, num_boot_samples, p)
set.seed(1)
for(k in 1:num_boot_samples){
  sub_index = sample( 1:n, size=m, replace=T)
  X_star = X[sub_index,]
  y_star = ln_y_real[sub_index]
  bootstrap_beta[k,] = solve(t(X_star)%*%X_star) %*% t(X_star) %*% y_star	
}

bootstrap_results = list( bootstrap_beta = bootstrap_beta, 
                          num_boot_samples = num_boot_samples, 
                          beta_hat_real = beta_hat_real)

save( bootstrap_results, file="bootstrap_results.rda")

