################################################
#
# Capture-recapture models (specifically: Cormack-Jolly-Seber models)
#
# IPM workshop, virtual Montpellier November 2020
#
################################################
# Based on the book "Bayesian population analysis using WinBUGS - a hierarchical perspective" 
# by Marc K�ry & Michael Schaub (2012, Academic Press)
#########################################################################

# Michael Schaub, 24.1.2012, revised 9.7.2012, revised 10.12.2013, revised 12.9.2017, revised 15.10.19
# Olivier Gimenez November 2020

#########################################################################

# Make sure that you have installed JAGS version 3.2.0
library(jagsUI)  # Load package

#########################################################################

setwd("C:/....")     # set working directory (Optional)


####################################################################
#
# 7. Estimation of survival probabilities using capture-recapture data
#
#####################################################################

#####################################################################
#### 7.3. Models with constant parameters
#####################################################################

# Define parameter values
n.occasions <- 6                   # Number of capture occasions
marked <- rep(50, n.occasions-1)   # Annual number of newly marked individuals
phi <- rep(0.65, n.occasions-1)
p <- rep(0.4, n.occasions-1)

# Define matrices with survival and recapture probabilities
PHI <- matrix(phi, ncol = n.occasions-1, nrow = sum(marked))
P <- matrix(p, ncol = n.occasions-1, nrow = sum(marked))

# Define function to simulate a capture-history (CH) matrix
simul.cjs <- function(PHI, P, marked){
  n.occasions <- dim(PHI)[2] + 1
  CH <- matrix(0, ncol = n.occasions, nrow = sum(marked))
  # Define a vector with the occasion of marking
  mark.occ <- rep(1:length(marked), marked[1:length(marked)])
  # Fill the CH matrix
  for (i in 1:sum(marked)){
    CH[i, mark.occ[i]] <- 1       # Write an 1 at the release occasion
    if (mark.occ[i]==n.occasions) next
    for (t in (mark.occ[i]+1):n.occasions){
      # Bernoulli trial: does individual survive occasion?
      sur <- rbinom(1, 1, PHI[i,t-1])
      if (sur==0) break		# If dead, move to next individual 
      # Bernoulli trial: is individual recaptured? 
      rp <- rbinom(1, 1, P[i,t-1])
      if (rp==1) CH[i,t] <- 1
    } #t
  } #i
  return(CH)
}

# Execute function
CH <- simul.cjs(PHI, P, marked)

# Create vector with first occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)

# Specify model in BUGS language
cat(file = "cjs-c-c.txt", "
model {
    # Constraints
    for (i in 1:nind){
      for (t in 1:(n.occasions-1)){
        phi[i,t] <- mean.phi
        p[i,t] <- mean.p
        } #t
      } #i
    
    # Priors
    mean.phi ~ dunif(0, 1)         # Prior for mean survival
    mean.p ~ dunif(0, 1)           # Prior for mean recapture
    
    # Likelihood 
    for (i in 1:nind){
      
      # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        
        # State process
        z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
        
        # Observation process
        y[i,t] ~ dbern(p[i,t-1] * z[i,t])
        } #t
      } #i
  }
")

# Bundle data
jags.data <- list(y = CH, 
                  f = f, 
                  nind = nrow(CH), 
                  n.occasions = ncol(CH))

# Initial values
# It is vital to specify "good" initial values for the latent state variable z. 
# Here is one possibility that works well.
z.inits <- function(ch){
  state <- ch
  state[state==0] <- 1
  #get.first <- function(x) min(which(x!=0))
  f <- apply(ch, 1, get.first)
  for (i in 1:nrow(ch)){
    state[i,1:f[i]] <- NA
  }
  return(state)
}


inits <- function(){list(mean.phi = runif(1, 0, 1), 
                         mean.p = runif(1, 0, 1), 
                         z = z.inits(CH))}

# Parameters monitored
parameters <- c("mean.phi", "mean.p")

# MCMC settings
ni <- 1000
nt <- 1
nb <- 500
nc <- 3

# Call JAGS from R (BRT 1 min)
cjs.c.c <- jags(data = jags.data, 
                inits = inits, 
                parameters.to.save = parameters, 
                model.file = "cjs-c-c.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb)
?jags
# Summarize posteriors
print(cjs.c.c, digits = 3)

# Compare the posterior means and medians of survival and detection 
# to the values we used to simulate data. 

# Note: You may also get posterior distribution for the latent 
# parameters z. 


#####################################################################
#### 7.4. Models with time-variation
#####################################################################

### 7.4.1. Fixed time effects

cat(file = "cjs.txt", "
model {
    # Constraints
    for (i in 1:nind){
      for (t in 1:(n.occasions-1)){
        phi[i,t] <- alpha[t]
        p[i,t] <- beta[t]
        } #t
      } #i
    # Priors
    for (t in 1:(n.occasions-1)){
      alpha[t] ~ dunif(0, 1)        # Priors for time-spec. recapture
      beta[t] ~ dunif(0, 1)         # Priors for time-spec. recapture
      }
    # Likelihood 
    for (i in 1:nind){
      # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        # State process
        z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
        # Observation process
        y[i,t] ~ dbern(p[i,t-1] * z[i,t])
        } #t
      } #i
  }
")

# Bundle data
jags.data <- list(y = CH, 
                  f = f, 
                  nind = nrow(CH), 
                  n.occasions = ncol(CH))

# Initial values
inits <- function(){list(z = z.inits(CH))}  

# Parameters monitored
parameters <- c("alpha", "beta")

# MCMC settings
ni <- 5000
nt <- 1
nb <- 1000
nc <- 3

# Call JAGS from R
cjs.t.t <- jags(data = jags.data, 
                inits = inits, 
                parameters.to.save = parameters, 
                model.file = "cjs.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                parallel = TRUE)

# Summarize posteriors
print(cjs.t.t, digits = 3)

# Again, compare the estimates with the values we used to simulate the data. 





### UNDERSTANDING PARAMETER REDUNDANCY

# Look at the parameter estimates: do you see anything conspicuous?
cjs.t.t$summary

# Plot the posteriors
par(mfrow = c(3,5))
for (i in 1:5){
  plot(density(cjs.t.t$sims.list$alpha[,i]), main = paste("Occasion ", i), ylim = c(0,5))
  abline(h = 1, lty = 2)
}
for (i in 1:5){
  plot(density(cjs.t.t$sims.list$beta[,i]), main = paste("Occasion ", i), ylim = c(0,5))
  abline(h = 1, lty = 2)
}
for (i in 1:5){
  plot(cjs.t.t$sims.list$alpha[,i], cjs.t.t$sims.list$beta[,i], main = paste("Occasion ", i), pch = 16, cex = 0.5)
}


### 7.4.2. Random time effects

# Define parameter values
n.occasions <- 12                  # Number of capture occasions
marked <- rep(50, n.occasions-1)   # Annual number of newly marked individuals
mean.phi <- 0.65
sigma2.phi <- 1                    # Temporal variance of survival
p <- rep(0.4, n.occasions-1)

# Determine annual survival probabilities (on logistic scale)
logit.phi <- rnorm(n.occasions-1, qlogis(mean.phi), sigma2.phi^0.5)
logit.phi
phi <- plogis(logit.phi)
phi

# Define matrices with survival and recapture probabilities
PHI <- matrix(phi, ncol = n.occasions-1, nrow = sum(marked), byrow = TRUE)
P <- matrix(p, ncol = n.occasions-1, nrow = sum(marked))

# Simulate capture-histories
CH <- simul.cjs(PHI, P, marked)

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)

# Specify model in BUGS language
cat(file = "cjs-temp.txt", "
model {
    
    # Constraints
    for (i in 1:nind){
       for (t in 1:(n.occasions-1)){
          logit(phi[i,t]) <- mu + epsilon[t]
          p[i,t] <- mean.p
          } #t
       } #i
    for (t in 1:(n.occasions-1)){
       epsilon[t] ~ dnorm(0, tau)
       
       # calculate annual estimates of survival (optional)
       phi.est[t] <- ilogit(mu + epsilon[t])
       } #t
    
    # Priors
    mean.p ~ dunif(0, 1)                     # Prior for mean recapture
    mean.phi ~ dunif(0, 1)                   # Prior for mean survival
    mu <- logit(mean.phi)                    # Logit transformation
    sigma ~ dunif(0, 5)                      # Prior for standard deviation
    tau <- pow(sigma, -2) 
    sigma2 <- pow(sigma, 2)                  # Temporal variance
    
    # Likelihood 
    for (i in 1:nind){
      # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        # State process
        z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
        # Observation process
        y[i,t] ~ dbern(p[i,t-1] * z[i,t])
      } #t
    } #i
}
")

# Bundle data
jags.data <- list(y = CH, 
                  f = f, 
                  nind = dim(CH)[1],
                  n.occasions = dim(CH)[2])

# Initial values
inits <- function(){list(z = z.inits(CH), 
                         mean.phi = runif(1, 0, 1), 
                         sigma = runif(1, 0, 5), 
                         mean.p = runif(1, 0, 1))}  

# Parameters monitored
parameters <- c("mean.phi", 
                "phi.est", 
                "mean.p", 
                "sigma2")

# MCMC settings
ni <- 2000
nt <- 1
nb <- 1000
nc <- 3

# Call JAGS from R (~ 1 min)
cjs.ran <- jags(data = jags.data, 
                inits = inits, 
                parameters.to.save = parameters, 
                model.file = "cjs-temp.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                parallel = TRUE)

# Summarize posteriors
print(cjs.ran, digits = 3)

# You know the drill: take a minute or two to compare your estimates
# with the values we used to simulate the data

# Produce histogram
hist(cjs.ran$sims.list$sigma2, 
     col = "gray", 
     nclass = 35, 
     las = 1, 
     xlab = expression(sigma^2), 
     main = "")
abline(v = sigma2.phi, 
       col = "red", 
       lwd = 2)


### 7.4.3. Temporal covariates

# Define parameter values
n.occasions <- 12                  # Number of capture occasions
marked <- rep(50, n.occasions-1)   # Annual number of newly marked individuals
mean.phi <- 0.65
p <- rep(0.4, n.occasions-1)
beta <- -0.3                       # Negative effect of winter severity (regression coeff)
r.sigma2 <- 0.2                    # Residual temporal variance

# Draw annual survival probabilities
winter <- rnorm(n.occasions-1, 0, 1^0.5)
logit.phi <- qlogis(mean.phi) + beta * winter + rnorm(n.occasions-1, 0, r.sigma2^0.5)
phi <- plogis(logit.phi)

# Look at the survival - winter relationship
plot(winter, 
     logit.phi, 
     ylab = 'survival (logit scale)')
abline(a = qlogis(mean.phi), 
       b = beta, 
       col = 'blue', 
       lwd = 2)

# Define matrices with survival and recapture probabilities
PHI <- matrix(phi, ncol = n.occasions-1, nrow = sum(marked), byrow = TRUE)
P <- matrix(p, ncol = n.occasions-1, nrow = sum(marked))

# Simulate capture-histories
CH <- simul.cjs(PHI, P, marked)

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)

# Specify model in BUGS language
cat(file = "cjs-cov.txt", "
model {
    
    # Constraints
    for (i in 1:nind){
       for (t in 1:(n.occasions-1)){
          logit(phi[i,t]) <- mu + beta * x[t] + epsilon[t]
          p[i,t] <- mean.p
          } #t
       } #i
    for (t in 1:(n.occasions-1)){
       epsilon[t] ~ dnorm(0, tau)
       }
    # Derived parameters (could be calculated offline from the mcmc chains)
    for (t in 1:(n.occasions-1)){
       phi.est[t] <- ilogit(mu + beta * x[t] + epsilon[t]) # Yearly survival
       }
    # Priors
    mu ~ dnorm(0, 1)                     # Prior for logit of mean survival
    mean.phi <- ilogit(mu)                   # Logit transformation
    beta ~ dunif(-10, 10)                   # Prior for slope parameter
    sigma ~ dunif(0, 10)                     # Prior on standard deviation
    tau <- pow(sigma, -2)
    sigma2 <- pow(sigma, 2)                  # Residual temporal variance
    mean.p ~ dunif(0, 1)                     # Prior for mean recapture
    
    # Likelihood 
    for (i in 1:nind){
      # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        # State process
        z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
        # Observation process
        y[i,t] ~ dbern(p[i,t-1] * z[i,t])
      } #t
    } #i
}
")

# Bundle data
jags.data <- list(y = CH, 
                  f = f, 
                  nind = nrow(CH), 
                  n.occasions = ncol(CH), 
                  x = winter)

# Initial values
inits <- function(){list(z = z.inits(CH), 
                         mu = rnorm(1), 
                         sigma = runif(1, 0, 5), 
                         beta = runif(1, -5, 5), 
                         mean.p = runif(1, 0, 1))}  

# Parameters monitored
parameters <- c("mean.phi", 
                "mean.p", 
                "phi.est", 
                "mu", 
                "sigma2", 
                "beta")

# MCMC settings
ni <- 5000
nt <- 1
nb <- 2500
nc <- 3

# Call JAGS from R
cjs.cov <- jags(data = jags.data, 
                inits = inits, 
                parameters.to.save = parameters, 
                model.file = "cjs-cov.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                parallel = TRUE)

# Summarize posteriors
print(cjs.cov, digits = 3)

# Produce graph
par(mfrow = c(1, 2), las = 1)
hist(cjs.cov$sims.list$beta, 
     nclass = 25, 
     col = "gray",
     main = "", 
     xlab = expression(beta), 
     ylab = "Frequency")
abline(v = -0.3, 
       col = "red", 
       lwd = 2)
hist(cjs.cov$sims.list$sigma2, 
     nclass = 50, 
     col = "gray", 
     main = "", 
     xlab = expression(sigma^2), 
     ylab = "Frequency", 
     xlim = c(0, 3))
abline(v = 0.2, 
       col = "red", 
       lwd = 2)

# Look at the survival - winter relationship
# True in blue, estimated in green
plot(winter, 
     logit.phi, 
     ylab = 'survival (logit scale)')
abline(a = qlogis(mean.phi), 
       b = beta, 
       col = 'blue', 
       lwd = 2)
abline(a = mean(cjs.cov$sims.list$mu), 
       b = mean(cjs.cov$sims.list$beta), 
       col = 'green', lwd = 2)


#####################################################################
# 7.5. Models with individual variation
#####################################################################

### 7.5.1. Fixed group effects

# Define parameter values
n.occasions <- 12                  # Number of capture occasions
marked <- rep(30, n.occasions-1)   # Annual number of newly marked individuals
phi.f <- rep(0.65, n.occasions-1)  # Survival of females
p.f <- rep(0.6, n.occasions-1)     # Recapture of females
phi.m <- rep(0.8, n.occasions-1)   # Survival of males
p.m <- rep(0.3, n.occasions-1)     # Reacpture of males

# Define matrices with survival and recapture probabilities
PHI.F <- matrix(phi.f, ncol = n.occasions-1, nrow = sum(marked))
P.F <- matrix(p.f, ncol = n.occasions-1, nrow = sum(marked))
PHI.M <- matrix(phi.m, ncol = n.occasions-1, nrow = sum(marked))
P.M <- matrix(p.m, ncol = n.occasions-1, nrow = sum(marked))

# Simulate capture-histories
CH.F <- simul.cjs(PHI.F, P.F, marked)
CH.M <- simul.cjs(PHI.M, P.M, marked)

# Merge capture-histories by row
CH <- rbind(CH.F, CH.M)

# Create group variable
group <- c(rep(1, dim(CH.F)[1]), rep(2, dim(CH.M)[1]))

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)

# Specify model in JAGS language
cat(file = "cjs-group.txt", "
model {
    
    # Constraints
    for (i in 1:nind){
       for (t in 1:(n.occasions-1)){
          phi[i,t] <- phi.g[group[i]]
          p[i,t] <- beta[t]
          } #t
       } #i
    # Priors
    for (u in 1:g){
       phi.g[u] ~ dunif(0, 1)        # Priors for group-specific survival
       }
    for (t in 1:(n.occasions-1)){
       beta[t] ~ dunif(0, 1)
       }
    
    # Likelihood 
    for (i in 1:nind){
      # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        # State process
        z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
        # Observation process
        y[i,t] ~ dbern(p[i,t-1] * z[i,t])
      } #t
    } #i
}
")

# Bundle data
jags.data <- list(y = CH, 
                  f = f, 
                  nind = nrow(CH), 
                  n.occasions = ncol(CH), 
                  g = length(unique(group)), 
                  group = group)

# Initial values
inits <- function(){list(z = z.inits(CH), 
                         phi.g = runif(length(unique(group)), 0, 1))}  

# Parameters monitored
parameters <- c("phi.g", 
                "beta")

# MCMC settings
ni <- 5000
nt <- 1
nb <- 2500
nc <- 3

# Call JAGS from R (~ 2 min)
cjs.group <- jags(data = jags.data, 
                  inits = inits, 
                  parameters.to.save = parameters, 
                  model.file = "cjs-group.txt", 
                  n.chains = nc, 
                  n.thin = nt, 
                  n.iter = ni, 
                  n.burnin = nb, 
                  parallel = TRUE)

# Summarize posteriors
print(cjs.group, digits = 3)

# The estimates are to be compared with the values we used to simulate the data.
# Note that detection is time-dependent, and fully identifiable. 


### 7.5.3. Individual random effects

# Define parameter values
n.occasions <- 12                 # Number of capture occasions
marked <- rep(50, n.occasions-1)  # Annual number of newly marked individuals
mean.phi <- 0.65
p <- rep(0.4, n.occasions-1)
v.ind <- 0.5

# Draw annual survival probabilities
logit.phi <- rnorm(sum(marked), qlogis(mean.phi), v.ind^0.5)
phi <- plogis(logit.phi)

# Define matrices with survival and recapture probabilities
PHI <- matrix(phi, ncol = n.occasions-1, nrow = sum(marked), byrow = FALSE)
P <- matrix(p, ncol = n.occasions-1, nrow = sum(marked))

# Simulate capture-histories
CH <- simul.cjs(PHI, P, marked)

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)

# Specify model in BUGS language
cat(file = "cjs-ind.txt", "
model {
    
    # Constraints
    for (i in 1:nind){
       for (t in 1:(n.occasions-1)){
          logit(phi[i,t]) <- mu + epsilon[i]
          p[i,t] <- mean.p
          } #t
       } #i
    for (i in 1:nind){
       epsilon[i] ~ dnorm(0, tau)   
       }
    # Priors
    mean.phi ~ dunif(0, 1)                   # Prior for mean survival
    mu <- logit(mean.phi)                    # Logit transformation
    sigma ~ dunif(0, 5)                      # Prior for standard deviation
    tau <- pow(sigma, -2)
    sigma2 <- pow(sigma, 2)
    mean.p ~ dunif(0, 1)                     # Prior for mean recapture 
    
    # Likelihood 
    for (i in 1:nind){
      # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        # State process
        z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
        # Observation process
        y[i,t] ~ dbern(p[i,t-1] * z[i,t])
      } #t
    } #i
}
")

# Bundle data
jags.data <- list(y = CH, 
                  f = f, 
                  nind = nrow(CH), 
                  n.occasions = ncol(CH))

# Initial values 
inits <- function(){list(z = z.inits(CH), 
                         mean.phi = runif(1, 0, 1), 
                         mean.p = runif(1, 0, 1), 
                         sigma = runif(1, 0, 2))}  

# Parameters monitored
parameters <- c("mean.phi", 
                "mean.p", 
                "sigma2")

# MCMC settings
ni <- 20000
nt <- 2
nb <- 10000
nc <- 3

# since this analysis takes a long time, we use a short run (1 chain) JUST as a demo
# (this is most probably too short for a real analysis)
ni <- 2000
nt <- 1
nb <- 1000
nc <- 1

# Call JAGS from R (NOTE: takes a long time to run!)
cjs.ind <- jags(data = jags.data, 
                inits = inits, 
                parameters.to.save = parameters, 
                model.file = "cjs-ind.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                parallel = TRUE)

# Summarize posteriors
print(cjs.ind, digits = 3)

# Produce graph
par(mfrow = c(1, 2), las = 1)
hist(cjs.ind$sims.list$mean.phi, 
     nclass = 25, 
     col = "gray",
     main = "", 
     xlab = expression(bar(phi)), 
     ylab = "Frequency")
abline(v = mean.phi, 
       col = "red", 
       lwd = 2)
hist(cjs.ind$sims.list$sigma2, 
     nclass = 15, 
     col = "gray", 
     main = "", 
     xlab = expression(sigma^2), 
     ylab = "Frequency", 
     xlim = c(0, 3))
abline(v = v.ind, 
       col = "red", 
       lwd = 2)



#####################################################################
# 7.7. Models with age effects
#####################################################################

# Define parameter values
n.occasions <- 10                   # Number of capture occasions
marked.j <- rep(200, n.occasions-1) # Annual number of newly marked juveniles
marked.a <- rep(30, n.occasions-1)  # Annual number of newly marked adults
phi.juv <- 0.3                      # Juvenile annual survival
phi.ad <- 0.65                      # Adult annual survival
p <- rep(0.5, n.occasions-1)        # Recapture
phi.j <- c(phi.juv, rep(phi.ad, n.occasions-2))
phi.a <- rep(phi.ad, n.occasions-1)

# Define matrices with survival and recapture probabilities
PHI.J <- matrix(0, ncol = n.occasions-1, nrow = sum(marked.j))
for (i in 1:length(marked.j)){
  PHI.J[(sum(marked.j[1:i])-marked.j[i]+1):sum(marked.j[1:i]),i:(n.occasions-1)] <- matrix(rep(phi.j[1:(n.occasions-i)],marked.j[i]), ncol = n.occasions-i, byrow = TRUE)
}
P.J <- matrix(rep(p, sum(marked.j)), ncol = n.occasions-1, nrow = sum(marked.j), byrow = TRUE)
PHI.A <- matrix(rep(phi.a, sum(marked.a)), ncol = n.occasions-1, nrow = sum(marked.a), byrow = TRUE)
P.A <- matrix(rep(p, sum(marked.a)), ncol = n.occasions-1, nrow = sum(marked.a), byrow = TRUE)

# Apply simulation function
CH.J <- simul.cjs(PHI.J, P.J, marked.j)  # individuals marked as juveniles
CH.A <- simul.cjs(PHI.A, P.A, marked.a)  # individuals marked as adults 

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f.j <- apply(CH.J, 1, get.first)
f.a <- apply(CH.A, 1, get.first)

# Create matrices X indicating age classes
x.j <- matrix(NA, ncol = dim(CH.J)[2]-1, nrow = dim(CH.J)[1])
x.a <- matrix(NA, ncol = dim(CH.A)[2]-1, nrow = dim(CH.A)[1])
for (i in 1:nrow(CH.J)){
  for (t in f.j[i]:(ncol(CH.J)-1)){
    x.j[i,t] <- 2
    x.j[i,f.j[i]] <- 1   
  } #t
} #i
for (i in 1:nrow(CH.A)){
  for (t in f.a[i]:(ncol(CH.A)-1)){
    x.a[i,t] <- 2
  } #t
} #i

CH <- rbind(CH.J, CH.A)
f <- c(f.j, f.a)
x <- rbind(x.j, x.a)

# Specify model in BUGS language
cat(file = "cjs-age.txt", "
model {
    # Constraints
    for (i in 1:nind){
       for (t in f[i]:(n.occasions-1)){
          phi[i,t] <- beta[x[i,t]]
          p[i,t] <- mean.p
          } #t
       } #i
    # Priors
    for (u in 1:2){
       beta[u] ~ dunif(0, 1)      # Priors for age-specific survival
       }
    mean.p ~ dunif(0, 1)         # Prior for mean recapture
    
    # Likelihood 
    for (i in 1:nind){
      # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        # State process
        z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
        # Observation process
        y[i,t] ~ dbern(p[i,t-1] * z[i,t])
      } #t
    } #i
}
")

# Bundle data
jags.data <- list(y = CH, 
                  f = f, 
                  nind = nrow(CH), 
                  n.occasions = ncol(CH), 
                  x = x)

# Initial values
inits <- function(){list(z = z.inits(CH), 
                         beta = runif(2, 0, 1), 
                         mean.p = runif(1, 0, 1))}  

# Parameters monitored
parameters <- c("beta", 
                "mean.p")

# MCMC settings
ni <- 5000
nt <- 1
nb <- 1000
nc <- 2

# Call JAGS from R
cjs.age <- jags(data = jags.data, 
                inits = inits, 
                parameters.to.save = parameters, 
                model.file = "cjs-age.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                parallel = FALSE)

print(cjs.age, digits = 3)



## Version of the code for modelling survival as a linear function of age

# Create matrix X indicating age classes
x <- matrix(NA, ncol = ncol(CH)-1, nrow = nrow(CH))
for (i in 1:nrow(CH)){
  for (t in f[i]:(ncol(CH)-1)){
    x[i,t] <- t-f[i]+1
  } #t 
} #i

## [change these lines in BUGS code]

# Constraints
for (i in 1:nind){
  for (t in f[i]:(n.occasions-1)){
    logit(phi[i,t]) <- mu + beta*x[i,t]
    p[i,t] <- mean.p
  } #t
} #i

# Priors (and derived parameters)
mu ~ dnorm(0, 0.01)             # Prior for mean of logit survival
beta ~ dnorm(0, 0.01)           # Prior for slope parameter
for (i in 1:(n.occasions-1)){
  phi.age[i] <- ilogit(mu + beta*i)   # Logit back-transformation 
}
mean.p ~ dunif(0, 1)                # Prior for mean recapture
