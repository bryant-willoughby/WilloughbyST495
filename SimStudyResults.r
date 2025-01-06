
# Load my simulation results
load("sim_results.rda")

beta_hat_real = sim_results$beta_hat_real
sigma_sq_hat_real = sim_results$sigma_sq_hat_real
X = sim_results$X
beta_hat_mat = sim_results$beta_hat_mat

XtX_inv = solve(t(X) %*% X)

p = dim(beta_hat_mat)[2]
num_sims = dim(beta_hat_mat)[1]

# Plot the sampling distributions of the least squares estimates
pdf("plot_sampling_dist.pdf")
par(mfrow=c(1,3))
for(j in 1:p){
  
  sd_beta_j = sqrt(sigma_sq_hat_real) * sqrt(XtX_inv[j,j])
  
  grid = seq( beta_hat_real[j] - 3*sd_beta_j, 
              beta_hat_real[j] + 3*sd_beta_j, by=.01)
  
  hist( beta_hat_mat[,j], freq=F, main=paste0("beta_hat_",j), xlab=NULL,
        xlim=c(grid[1],tail(grid,1)), breaks=floor(sqrt(num_sims)))
  abline( v=beta_hat_real[j], col="green", lwd=3)
  
  sd_beta_j = sqrt(sigma_sq_hat_real) * sqrt(XtX_inv[j,j])
  lines(grid, dnorm( grid, mean=beta_hat_real[j], sd=sd_beta_j), lwd=3)
}
dev.off()

pdf("plot_beta_hat_1.pdf")
j = 1
sd_beta_j = sqrt(sigma_sq_hat_real) * sqrt(XtX_inv[j, j])

grid = seq(beta_hat_real[j] - 3 * sd_beta_j,
           beta_hat_real[j] + 3 * sd_beta_j, by = 0.000001)

hist(beta_hat_mat[, j], freq = F, main = paste0("beta_hat_", j), xlab = NULL,
     xlim = c(grid[1], tail(grid, 1)), breaks = floor(sqrt(num_sims)), 
     cex.main = 2.0)
abline(v = beta_hat_real[j], col = "green", lwd = 3)
lines(grid, dnorm(grid, mean = beta_hat_real[j], sd = sd_beta_j), lwd = 3)
dev.off()


pdf("plot_beta_hat_2.pdf")
j = 2
sd_beta_j = sqrt(sigma_sq_hat_real) * sqrt(XtX_inv[j, j])

grid = seq(beta_hat_real[j] - 3 * sd_beta_j,
           beta_hat_real[j] + 3 * sd_beta_j, by = 0.000001)

hist(beta_hat_mat[, j], freq = F, main = paste0("beta_hat_", j), xlab = NULL,
     xlim = c(grid[1], tail(grid, 1)), breaks = floor(sqrt(num_sims)), 
     cex.main = 2.0)
abline(v = beta_hat_real[j], col = "green", lwd = 3)
lines(grid, dnorm(grid, mean = beta_hat_real[j], sd = sd_beta_j), lwd = 3)
dev.off()


pdf("plot_beta_hat_3.pdf")
j = 3
sd_beta_j = sqrt(sigma_sq_hat_real) * sqrt(XtX_inv[j, j])

grid = seq(beta_hat_real[j] - 3 * sd_beta_j,
           beta_hat_real[j] + 3 * sd_beta_j, by = 0.000001)

hist(beta_hat_mat[, j], freq = F, main = paste0("beta_hat_", j), xlab = NULL,
     xlim = c(grid[1], tail(grid, 1)), breaks = floor(sqrt(num_sims)), 
     cex.main = 2.0)
abline(v = beta_hat_real[j], col = "green", lwd = 3)
lines(grid, dnorm(grid, mean = beta_hat_real[j], sd = sd_beta_j), lwd = 3)
dev.off()

# Check whether confidence intervals for the least squares estimates have 
# the correct coverage
level = .95
coverage = rep( 0, p)
for(k in 1:num_sims){
  for(j in 1:p){
    sd_beta_j = sqrt(sigma_sq_hat_real) * sqrt(XtX_inv[j,j])
    lower = beta_hat_mat[k,j] - sd_beta_j * qnorm( (1-level)/2, lower.tail=F)
    upper = beta_hat_mat[k,j] + sd_beta_j * qnorm( (1-level)/2, lower.tail=F)
    
    #adding one here if argument in parentheses evaluates to T
    coverage[j] = coverage[j] + (lower < beta_hat_real[j] & 
                                   beta_hat_real[j] < upper)
  }
}

#should be (approximately) equal to the level 
coverage = coverage / num_sims; coverage


