#######################################################################################
# File name: imputation.R
# Author: Osman Mahic
# Date: 22-01-2025
# Description: R code for simulating the impact of outcome misclassification in a RCT
#######################################################################################

#Generate simulated datasets

sim.fun <- function(
    numsim,
    n = 100,
    size = 1,
    prob.treat = 0.2,     #Specify probability of success in treatment and control arm
    prob.control = 0.4,
    prob.mis = 0.25       #Specify amount of misclassification
)
  {
  # Create empty lists
  simdat <- vector(mode = "list", length = numsim)
  simdat.mis <- vector(mode = "list", length = numsim)
  RD <- vector(mode = "list", length = numsim)
  RD.mis <- vector(mode = "list", length = numsim)
  RR <- vector(mode = "list", length = numsim)
  RR.mis <- vector(mode = "list", length = numsim)
  OR <- vector(mode = "list", length = numsim)
  OR.mis <- vector(mode = "list", length = numsim)
  
  for(i in 1:numsim){
    treat <- rbinom(n = n, size = size, prob = prob.treat) #Sample from binomial distributions
    control <- rbinom(n = n, size = size, prob = prob.control)
    
    outcome <- c(treat, control)
    group <- c(rep("treat", 100), rep("control", 100))
    d <- data.frame(
      arm = group,
      outcome = outcome
    )
    
    # Misclassify outcome
    subset_event <- d[d$outcome==1,]
    random.i <- sample(nrow(subset_event), size = 0.25 * nrow(subset_event), replace = FALSE)
    sampled_row_names <- as.numeric(rownames(subset_event)[random.i])
    d.mis <- d
    d.mis$outcome[sampled_row_names] <- 0 #Classify a random subset as 0 instead of 1
    simdat[[i]] <- d
    simdat.mis[[i]] <- d.mis
    
    # Calculate the effect estimates
    matrix <- table(d$arm, d$outcome)
    matrix.mis <- table(d.mis$arm, d.mis$outcome)
    # Calculate RD
    RD[[i]] <- (matrix[2,2] / (matrix[2,2] + matrix[2,1])) - (matrix[1,2] / (matrix
                                                                             [1,2] + matrix[1,1]))
    RD.mis[[i]] <- (matrix.mis[2,2] / (matrix.mis[2,2] + matrix.mis[2,1])) - (matrix.mis[1,2] / (matrix.mis[1,2] + matrix.mis[1,1]))
    
    # Calculate RR
    RR[[i]] <- (matrix[2,2] / (matrix[2,2] + matrix[2,1])) / (matrix[1,2] / (matrix
                                                                             [1,2] + matrix[1,1]))
    RR.mis[[i]] <- (matrix.mis[2,2] / (matrix.mis[2,2] + matrix.mis[2,1])) / (matrix.mis[1,2] / (matrix.mis[1,2] + matrix.mis[1,1]))
    # Calculate OR
    OR[[i]] <- (matrix[2,2] * matrix[1,1]) / (matrix[2,1] * matrix[1,2])
    OR.mis[[i]] <- (matrix.mis[2,2] * matrix.mis[1,1]) / (matrix.mis[2,1] * matrix.mis[1,2])
    
  }
  pars.spec <- data.frame(numsim,
                          n,
                          size,
                          prob.treat,
                          prob.control,
                          prob.mis)
  list(pars.spec = pars.spec,
       simdat = simdat,
       simdat.mis = simdat.mis,
       RD = RD,
       RD.mis = RD.mis,
       RR = RR,
       RR.mis = RR.mis,
       OR = OR,
       OR.mis = OR.mis)
}

sim <- sim.fun(numsim = 1000) #Specify any number of simulations

###Calculate bias by comparing effect measures in misclassified versus correctly classified datasets 
#OR, RR, RD bias
OR.est <- unlist(sim$OR)
OR.mis.est <- unlist(sim$OR.mis)
OR.bias <- mean(OR.mis.est - OR.est)
RR.est <- unlist(sim$RR)
RR.mis.est <- unlist(sim$RR.mis)
RR.bias <- mean(RR.mis.est - RR.est)
RD.est <- unlist(sim$RD)
RD.mis.est <- unlist(sim$RD.mis)
RD.bias <- mean(RD.mis.est - RD.est)




  
