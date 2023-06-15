#' Model definition of Two-phase recovery for individual single species trajectories 
#'
#' @description
#' This file defines the necessary functions to define the \code{model} object for the 
#' single species two-phase model at the level of individual trajectories. This 
#' object is required to use the MCMC functions in and predictive sampling functions
#' from \code{LTMPModellingTools.R}. In effect, this file takes the place of a *.stan 
#' model file, of a JAGS/BUGS model string.
#'
#' @section Warning:
#' As this is cablibrated on individual trajectories, the upper bounds of the \eqn{T_d} parameter
#' should be set to the second last observation of the data trajectory (See example pipelines). 
#' As a result, \code{model$upper["Td"] <- -1} by default to force an error if this is not over written.
#' 
#' @note This file (or a copy of it) needs to be modified to change the form of the ODEs
#' the prior definitions and other parameter constraints.
#'
#' @author
#' \itemize{
#'      \item David J. Warne[1,2,3] (\email{david.warne@qut.edu.au})
#' }
#' \enumerate{
#'  \item School of Mathematical Sciences, Faculty of Science, Queensland University of Technology
#'  \item Centre for Data Science, Queensland University of Technology
#'  \item ARC Centre of Excellence for Mathematical and Statistical Frontiers (ACEMS)
#' }
#' 
#' @docType data
#' @name AA_Model_Summary
NULL

#' Parameter labels
#'
#' @description Returns parameter vector with labels 
#'
#' @note The function relies upon a prespecified list of strings call \code{param_names}. 
#' 
#' @param theta parameter vector
#'
#' @return parameter vector with labels
#'
vlab <- function(theta) {
    labs <- c(1:length(theta))
    k <- 1
    for (L in param_names) {
        labs[k] <- L
        k <- k + 1
    }
    names(theta) <- labs
    return(theta)
}

#' Log density of the prior
#'
#' @description log density of the prior \eqn{p(\theta | \lambda)} where \epn{\lambda} is a vector of hyper-parameters
#'
#' @param theta Parameter vector
#' @param lambda Hyper-parameter vector
#'
logprior <- function(theta,lambda) {
    d <- sum(c(dnorm(theta[1], mean = lambda[1], sd = lambda[2], log = TRUE),
               dnorm(theta[2], mean = lambda[3], sd = lambda[4], log = TRUE),
               dnorm(theta[3], mean  = lambda[5], sd = lambda[6], log = TRUE),
               dnorm(theta[4], mean = lambda[7], sd = lambda[8], log = TRUE)))
    if(is.nan(d) || is.na(d)){
        return(-Inf)
    } else {
        return(d)
    }
}

#' Prior sampler
#'
#' @description Prior sampler for chain initialisation by generateion \eqn{\theta \sim p(.|\lambda)}. 
#' @note In general this function need not actually sample the prionr, but it at least must be 
#' consistent with log-prior is terms of support. 
#' To this end, the function can be used to specify many initialisation strategies
#'
#' @param lambda hyper-parameter vector
#' 
prior_sampler <- function(lambda) {
    return(c(rnorm(1, mean = lambda[1], sd = lambda[2]),
             rnorm(1, mean = lambda[3], sd = lambda[4]),
             rnorm(1, mean  = lambda[5], sd = lambda[6]),
             rnorm(1, mean = lambda[7], sd = lambda[8])))
}

#' Single species generalised two-phase model
#'
#' @description Implements right-hand-side (RHS) of the ODE model for coral recovery
#' @note This function is in a specific format for the \code{deSolve} package.
general_logistic_twophase <- function(t, X, theta) {
    with(as.list(c(X,theta)), {
        if (t < Td) {
            dCdt <- alphaD*(alpha/gamma)*C*(1.0 - (C/K)^(gamma))
        } else {
            dCdt <- (alpha/gamma)*C*(1.0 - (C/K)^(gamma))
        }
        list(c(dCdt))
    })
}

general_logistic_twophase_analytic <- function(t, X0, theta) {
  alpha <- theta["alpha"]
  alphaD <- theta["alphaD"]
  gamma <- theta["gamma"]
  Td <- theta["Td"]
  K <- theta["K"]
  Xt <- rep(0,length(t))
  t_ind1 <- which(t < Td)
  t_ind2 <- which(t >= Td)
  Xt[t_ind1] <- K*((1+((K/X0)^gamma -1)*exp(-alphaD*alpha*t[t_ind1]))^(-1/gamma))
  Xd <- K*((1+((K/X0)^gamma -1)*exp(-alphaD*alpha*Td))^(-1/gamma))
  Xt[t_ind2] <- K*((1+((K/Xd)^gamma -1)*exp(-alpha*(t[t_ind2]-Td)))^(-1/gamma))
  out <- matrix(rep(0,2*length(t)),nrow = length(t), ncol = 2)
  colnames(out) <- c("time","C")
  out[,'time'] <- t
  out[,'C'] <- Xt
  return(out)
}

#' Log likelihood
#'
#' @description Log likelihood for the data given parameters theta under the model 
#'
#' @param data Data list 
#' @param model Model object 
#' @param theta Parameter vector
#'
loglike <- function(data,model,theta) {
    ## extract initial condition
    X0 = c(C = data$c0)
    ## labeled parameter vector (with carrying capacity, K, appended)
    p <- c(model$varnames(theta), K = data$K)
    ## solve forwards problem for given theta
    if (is.null(model$ode_sol) == FALSE){
      C_mu <- model$ode_sol(c(data$t0,data$ts),X0,p)
    } else {
      C_mu <- ode(y = X0, times = c(data$t0,ata$ts), 
                  func = model$ode_func, parms = p, method = "ode45")   
    }
    ## observation error model
    Serr <- data$Serr[-1]
    Serr[Serr <= 0] <- 0.01
    d <- sum(dnorm(data$C,mean = C_mu[-1,'C'],sd = Serr, log = TRUE))
    if (is.nan(d) || is.na(d)) {
        return(-Inf)
    } else {
        return(d)
    }
}

#' Likelihood sampler
#' 
#' @description Generates synthetic data given a fix parameter value \eqn{\theta}. 
#' This is not used by the MCMC functions, but the posterior and prior predictive samplers.
#'
#' @param data Data list
#' @param model Model Object
#' @param theta Parameter vector
#'
like_sampler <- function(data,model,theta) {
    ## extract initial condition
    X0 = c(C = data$c0)
    ## labelled parameter vector (with carrying capacity, K, appended)
    p <- c(theta, K = data$K)
    ## solve forwards problem for given theta
    if (is.null(model$ode_sol) == FALSE){
      C_mu <- model$ode_sol(c(data$t0,data$ts),X0,p)
    } else {
      C_mu <- ode(y = X0, times = c(data$t0,ata$ts), 
                  func = model$ode_func, parms = p, method = "ode45")   
    }
    ## observation error model
    Serr <- data$Serr
    Serr[Serr <= 0] <- 0.01
    #print(C_mu)
    #print(rnorm(length(C_mu[,'C']), mean = C_mu[,'C'], sd = Serr))
    C_s <- rnorm(length(C_mu[,'C']), mean = C_mu[,'C'], sd = Serr)
    C_s[C_s < 0] <- 0 
    return(C_s)
}

# ROXYGEN_STOP

# define parameter names
param_names <- c("alphaD","alpha", "gamma","Td")

# build model structure
model <- list(ode_func = general_logistic_twophase,       # RHS for ODE model
              ode_sol = general_logistic_twophase_analytic,  # to use the analytic solution
              loglike = loglike,                          # log likelihood function
              like_sampler = like_sampler,                # simulation of data generation process (for pred. checks)
              logprior = logprior,                        # log prior density
              prior_sampler = prior_sampler,              # prior sampler
              lower = c(0.05,0,0,0),                      # lower parameter bounds 
              upper = c(0.95,Inf,Inf,-1),  # upper parameter bounds Note: last entry needs to be updated per trajectory
              hyp = c(0.5,0.5,0.3,0.3,0.0,0.5,0.0,4.0),   # prior hyper-parameters
              varnames = vlab)                            # parameter labels
# ROXYGEN_START
