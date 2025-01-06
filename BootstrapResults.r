
# Code for producing plots and tables 

# Load my bootstrap results
load("bootstrap_results.rda")

bootstrap_beta = bootstrap_results$bootstrap_beta
num_boot_samples = bootstrap_results$num_boot_samples
beta_hat_real = bootstrap_results$beta_hat_real


#add grid lines according to standard deviation of each respective beta estimate

pdf("plot_beta_hat_0.pdf")
# Histogram of the bootstrapped sampling distribution for the estimated beta_0
# Plot histogram with density overlay
hist(exp(bootstrap_beta[,1]), breaks = floor(sqrt(length(bootstrap_beta))), freq = F,
     main = "beta_0_hat",  xlab = "")
lines(density(exp(bootstrap_beta[,1])), lwd = 2)  # Overlay density curve
abline(v = exp(beta_hat_real[1]), col = "green", lwd = 3)  # Add vertical line
dev.off()

pdf("plot_beta_hat_1.pdf")
# Histogram of the bootstrapped sampling distribution for the estimated beta_1
hist( exp(bootstrap_beta[,2]), breaks=floor(sqrt(num_boot_samples)), freq=F,
      main="beta_1_hat", xlab = "")
lines(density(exp(bootstrap_beta[,2])), lwd = 2)
abline( v=exp(beta_hat_real[2]), col="green", lwd=3)
dev.off()

pdf("plot_beta_hat_2.pdf")
# Histogram of the bootstrapped sampling distribution for the estimated beta_2
hist( exp(bootstrap_beta[,3]), breaks=floor(sqrt(num_boot_samples)), freq=F,
      main="beta_2_hat", xlab = "")
lines(density(exp(bootstrap_beta[,3])), lwd = 2)
abline( v=exp(beta_hat_real[3]), col="green", lwd=3)
dev.off()

#summaries of coefficient estimates (on back-transformed scale)
exp(mean(bootstrap_beta[,1])); exp(sd(bootstrap_beta[,1]))
exp(mean(bootstrap_beta[,2])); exp(sd(bootstrap_beta[,2]))
exp(mean(bootstrap_beta[,3])); exp(sd(bootstrap_beta[,3]))

# Percentile bootstrap confidence interval for the coefficients
alpha = 0.05
beta_hat_real

# Individual CI for intercept 
lower = exp(quantile(bootstrap_beta[,1], probs = alpha/2)); lower
upper = exp(quantile(bootstrap_beta[,1], probs = 1 - alpha/2)); upper
exp(beta_hat_real[1])

# Individual CI for beta_1
lower = exp(quantile(bootstrap_beta[,2], probs = alpha/2)); lower
upper = exp(quantile(bootstrap_beta[,2], probs = 1 - alpha/2)); upper
exp(beta_hat_real[2])

# Individual CI for beta_2
lower = exp(quantile(bootstrap_beta[,3], probs = alpha/2)); lower
upper = exp(quantile(bootstrap_beta[,3], probs = 1 - alpha/2)); upper
exp(beta_hat_real[3])


