#' LTMPModellingTools.R is a utility library for modelling coral recovery with LTMP data
#'
#' @description
#' This library provides various functions for fitting growth models and subsequent analysis using 
#' data from the Long-Term Monitoring Program (LTMP). This includes the tools that 
#' form the basis of the direct two-phase recovery identification, power/specificity analysis, and
#' Bayesian inference and prediction.
#'
#' @section Current Features:
#' \itemize{
#'   \item Analytic solutions to standard population growth models;
#'   \item Per-capita rate vs cover curve and uncertainty quantification;
#'   \item Multi-segment regression to identify delays;
#'   \item Maximum likelihood estimation for delay impact quantification;
#'   \item Markov chain Monte Carlo for Bayesian inference of generalise model;
#' }
#' @section Markov chain Monte Carlo (MCMC) samplers:
#' This library defines three very general MCMC sampler functions that support the specification of 
#' any statistical or ODE based model. Each of these functions take three list stuctures for the 
#' data, the model, and configuration of the sample. See the \code{models/} directory for example model
#' constuctions and \code{pipelines/} folder for example data structures and configurations.
#' 
#' @author
#' \itemize{
#'      \item David J. Warne[1,2,3] (\email{david.warne@qut.edu.au})
#'      \item Grace E. M. Heron[1,3] (\email{g.heron@qut.edu.au})
#' }
#' \enumerate{
#'  \item School of Mathematical Sciences, Faculty of Science, Queensland University of Technology
#'  \item Centre for Data Science, Queensland University of Technology
#'  \item ARC Centre of Excellence for Mathematical and Statistical Frontiers (ACEMS)
#' }
#' 
#' @note assumes processed recovery trajectories as per \code{LTMPDataTools.R} functions
#' @docType data
#' @name AA_Library_Summary
NULL
# ROXYGEN_STOP


# ROXYGEN_START

#-------------------------------------------------------------------------------
# Standard growth models
#-------------------------------------------------------------------------------
 
#' Exponential Growth Model
#'
#' @description 
#' Solution to the exponential growth model,
#'           \deqn{\frac{dC}{dt} = \alpha C(t)}
#' where \eqn{C(t)} is the population density, \eqn{\alpha} is the intrinsic growth rate.
#'
#' @param t time to evaluate solution
#' @param alpha intrinsic growth rate
#' @param c0 initial population density at \eqn{t = 0}
#'
#' @return the population density, \eqn{C(t)}, at time \eqn{t > 0}
#' @note for applications to coral growth, the implicit assumption is that % coral
#' cover is directly proportional to population density, furthermore that growth is
#' unbounded, therefore this model is only appropriate in early growth phases.
#' @family Analytic solutions to recovery models
exponential_sol <- function(t,alpha,c0) {
    return(c0*exp(alpha*t))
}

#'  Logistic Growth Model
#'
#' @description Solution to the standard logistic growth model,
#'           \deqn{\frac{dC}{dt} = \alpha C(t)(1-C(t)/K)}
#' where \eqn{C(t)} is the population density, \eqn{\alpha} is the intrinsic growth rate, and 
#' \eqn{K} is the carrying capacity density.
#'
#' @param t time to evalutate solution
#' @param alpha intrisic growth rate
#' @param K carrying capacity density
#' @param c0 initial population density at \eqn{t = 0}
#'
#' @return the population density, \eqn{C(t)}, at time \eqn{t > 0}
#' @note for applications to coral growth, the implicit assumption is that % coral
#' cover is directly proportional to population density
#' @family Analytic solutions to recovery models
logistic_sol <- function(t,alpha,K,c0) {
    return((K*c0)/((K-c0)*exp(-alpha*t) + c0))

}

#' Gompertz Growth Model
#' 
#' @description Solution to the Gompertz growth model,
#'           \deqn{\frac{dC}{dt} = -\alpha C(t)\ln{(C(t)/K)}}
#' where \eqn{C(t)} is the population density, \eqn{\alpha} is the intrinsic growth rate, and 
#' \eqn{K} is the carrying capacity density.
#'
#' @param t time to evalutate solution
#' @param alpha intrisic growth rate
#' @param K carrying capacity density
#' @param c0 initial population density at \eqn{t = 0}
#'
#' @return the population density, \eqn{C(t)}, at time \eqn{t > 0}
#' @note for applications to coral growth, the implicit assumption is that % coral
#' cover is directly proportional to population density
#' @family Analytic solutions to recovery models
gompertz_sol <- function(t,alpha,K,c0) {
    return(K*exp(log(c0/K)*exp(-alpha*t)))
}

#' Delayed Logistic Growth Model
#'
#' @description Two phase model of the form,
#'           \deqn{\frac{dC}{dt} = \alpha_1 C(t)}
#' if \eqn{t \le T}
#'           \deqn{\frac{dC}{dt} = \alpha_2 C(t)(1-C(t)/K)}
#' otherwise, where \eqn{C(t)} is the population density, \eqn{\alpha_1} and \eqn{\alpha_2} are the intrinsic growth 
#' rates in phases 1 and 2, \eqn{K} is the carrying capacity density, and \eqn{T} is the first
#' phase duration. Furthermore, \eqn{\alpha_1 < \alpha_2} thereby representing a form of 
#'  delay in logistic growth
#'
#' @param t time to evalutate solution
#' @param T phase 1 duration (i.e., delay duration)
#' @param alpha1 intrisic growth rate in phase 1
#' @param alpha2 intrisic growth rate in phase 2
#' @param K carrying capacity density
#' @param c0 initial population density at \eqn{t = 0}
#'
#' @return the population density, \eqn{C(t)}, at time \eqn{t > 0}
#' @family Analytic solutions to recovery models
delay_logistic_sol <- function(t,T,alpha1,alpha2,K,c0) {
    if (t <= T) {
        return(exponential_sol(t,alpha1,c0))
    } else {
        return(logistic_sol(t-T,alpha2,K,exponential_sol(T,alpha1,c0)))
    }
}

#' Delayed Gompertz Growth Model
#'
#' @description  Two phase model of the form,
#'           \deqn{\frac{dC}{dt} = \alpha_1 C(t)}
#' if \eqn{t \le T}
#'           \deqn{\frac{dC}{dt} = -\alpha C(t)\ln{(C(t)/K)}}
#' otherwise, where \eqn{C(t)} is the population density, \eqn{\alpha_1} and \eqn{\alpha_2} are the intrinsic growth 
#' rates in phases 1 and 2, \eqn{K} is the carrying capacity density, and \eqn{T} is the first
#' phase duration. Furthermore, \eqn{\alpha_1 < \alpha_2} thereby representing a form of 
#'  delay in Gompertz growth
#'
#' @param t time to evalutate solution
#' @param T phase 1 duration (i.e., delay duration)
#' @param alpha1 intrisic growth rate in phase 1
#' @param alpha2 intrisic growth rate in phase 2
#' @param K carrying capacity density
#' @param c0 initial population density at \eqn{t = 0}
#'
#' @return the population density, \eqn{C(t)}, at time \eqn{t > 0}
#' @family Analytic solutions to recovery models
delay_gompertz_sol <- function(t,T,alpha1,alpha2,K,c0) {
    if (t <= T) {
        return(exponential_sol(t,alpha1,c0))
    } else {
        return(gompertz_sol(t-T,alpha2,K,exponential_sol(T,alpha1,c0)))
    }
}

#-------------------------------------------------------------------------------
# Finite difference functions 
#-------------------------------------------------------------------------------

#' First order forward finite difference of first derivative
#'
#' @description Given function \eqn{f(t)} and small time step \eqn{h >0} we have
#' \deqn{f'(t) = \frac{f(t+h) -f(t)}{h} + O(h)} 
#'
#' @param f function to be differentiated
#' @param t time point to evaluate derivative at
#' @param h step-size of discretisation
#'
#' @return \eqn{O(h)} approximation to \eqn{f'} at \eqn{t}
#' @family Numerical differentiation
finite_diff_1stF <- function(f,t,h) {
    return((f(t + h) - f(t))/h)    
}

#' First order backward finite difference of first derivative
#'
#' @description Given function \eqn{f(t)} and small time step \eqn{h >0} we have
#' \deqn{f'(t) = \frac{f(t) -f(t-h)}{h} + O(h)} 
#'
#' @param f function to be differentiated
#' @param t time point to evaluate derivative at
#' @param h step-size of discretisation
#'
#' @return \eqn{O(h)} approximation to \eqn{f'} at \eqn{t}
#' @family Numerical differentiation
finite_diff_1stB <- function(f,t,h) {
    return((f(t) - f(t - h))/h)    
}

#' Second order central finite difference of first derivative
#'
#' @description Given function \eqn{f(t)} and small time step \eqn{h >0} we have
#' \deqn{f'(t) = \frac{f(t+h) -f(t-h)}{2h} + O(h^2)} 
#'
#' @param f function to be differentiated
#' @param t time point to evaluate derivative at
#' @param h step-size of discretisation
#'
#' @return \eqn{O(h^2)} approximation to \eqn{f'} at \eqn{t}
#' @family Numerical differentiation
finite_diff_2ndC <- function(f,t,h) {
    return((f(t + h) - f(t - h))/(2.0*h))    
}

#' Approximate derivatives numerically
#'
#' @description Differentiate function \eqn{f} at points \eqn{t_i}. 
#' Uses finite differences with step (first order for boundary nodes and second 
#' order for internal nodes). 
#' 
#' @param f function to be differentiated
#' @param ti time points to evaluate derivative at
#' @param h step-size of discretisation
#'
#' @return an array containing derivative approximations \eqn{f'} at \eqn{t_i}.
#' @family Numerical differentiation
finite_diff <- function(f,ti,h) {
    n <- length(ti)
    dfdt <- numeric(n)
    # computing boundaries with first order finite differences
    dfdt[1] <- finite_diff_1stF(f,ti[1],h)
    dfdt[n] <- finite_diff_1stB(f,ti[n],h)
    # 2nd order finite differences for internal nodes
    for (k in 2:(n-1)){
        dfdt[k] <- finite_diff_2ndC(f,ti[k],h)
    }
    return(dfdt) 
}

#' Computes quantile function of the ratio of two correlated Gaussians
#'
#' @description Uses the approximation by Hinkley (1969) for the distribution of \eqn{W = X_1/X_2}
#' with \eqn{X_1 \sim N(\mu_1,\sigma_1^2)} \eqn{X_2 \sim N(\mu_2,\sigma_2^2)} with correlation \rho. 
#' Approximation assumption is that \eqn{\mu_2 \gg \sigma_2 > 0} i.e. \eqn{Pr(X_2 < 0) \to 0}
#'
#' @note See Hinkley D.V. (1969) Biometrika v56 
#'
#' @param u point in quantile functionto evaluate
#' @param mu1 mean of numerator
#' @param mu2 mean of denominator
#' @param sig1 standard deviation of numerator
#' @param sig2 standard deviation of denominator
#' @param rho correlation coefficient between \eqn{X_1} and \eqn{X_2}
#' @family Uncertainty quantification
ratio_quantile <- function(u,mu1,mu2,sig1,sig2,rho) {
   
    if (mu2 - 2*sig2 < 0)
    {
        # treat X(t) as constant for really small cover perc
        w <- qnorm(u,mu1/mu2,sig1/mu2)
        if (is.nan(w))
            print(c(u,mu1,mu2,sig1))
        return(w)
    } else {
        f <- function(w) {pnorm((mu2*w-mu1)/(sig1*sig2*sqrt((w/sig1)^2 - 2*(rho*w)/(sig1*sig2) + (1/sig2)^2 ))) - u}
        res <- uniroot(f,lower = -1.0, upper = 1.0, f.lower = -u, f.upper = 1)
        return(res$root)
    }
}

#' Computes per capita rates with uncertainty quantiles
#'
#' @description Uses the \code{finite_diff} to esitmate derivatives and \code{ratio_quantile} to propagate observation uncertainty through the calculation.
#'
#' @param f interpolation of coral cover
#' @param f_sig interpolation of standard error in cover estimates
#' @param h step-size to use
#' 
#' @return per-capita estimates along with 80% credible intervals
#' @family Per capita extimation
finite_diff_pc_uq <- function(f,f_sig,ti,h) {
    n <- length(ti)
    pc <- numeric(n)
    lb <- numeric(n)
    ub <- numeric(n)
    med <- numeric(n)

    # computing boundaries with first order finite differences
    mu1 <- finite_diff_1stF(f,ti[1],ti[2]-ti[1])
    sig1 <- sqrt((f_sig(ti[2])^2 + f_sig(ti[1])^2)/(ti[2]-ti[1])^2)
    
    mu2 <- f(ti[1])
    sig2 <- f_sig(ti[1])
    
    rho <-  -1*(f_sig(ti[1])/sqrt(f_sig(ti[2])^2 + f_sig(ti[1])^2))
    
    pc[1] <- mu1/mu2
    lb[1] <- ratio_quantile(0.1, mu1,mu2,sig1,sig2,rho)
    ub[1] <- ratio_quantile(0.9, mu1,mu2,sig1,sig2,rho)
    
    mu1 <- finite_diff_1stB(f,ti[n],ti[n]-ti[n-1])
    sig1 <- sqrt((f_sig(ti[n])^2 + f_sig(ti[n-1])^2)/(ti[n]-ti[n-1])^2)
    
    mu2 <- f(ti[n])
    sig2 <- f_sig(ti[n])
    
    rho <-  (f_sig(ti[n])/sqrt(f_sig(ti[n])^2 + f_sig(ti[n-1])^2))
    
    pc[n] <- mu1/mu2
    lb[n] <- ratio_quantile(0.1, mu1,mu2,sig1,sig2,rho)
    ub[n] <- ratio_quantile(0.9, mu1,mu2,sig1,sig2,rho)
    
    # 2nd order finite differences for internal nodes
    for (k in 2:(n-1)){
        h <- min(ti[k]-ti[k-1],ti[k+1]-ti[k])
        mu1 <- finite_diff_2ndC(f,ti[k],h)
        sig1 <- sqrt((f_sig(ti[k]+h)^2 + f_sig(ti[k]-h)^2)/(2*h)^2)
        
        mu2 <- f(ti[k])
        sig2 <- f_sig(ti[k])
        
        rho <-  0
        
        pc[k] <- mu1/mu2
        lb[k] <- ratio_quantile(0.1, mu1,mu2,sig1,sig2,rho)
        ub[k] <- ratio_quantile(0.9, mu1,mu2,sig1,sig2,rho)
    }
   
    return(list(pc,lb,med,ub))
}

#' Linear model log-likelihood 
#'
#' @description Computes log likelihood for a linear regression model with Gaussian
#' residuals
#' 
#' @param model fitted linear model (e.g., \code{model <- lm(y ~ x)})
#' @return log likelihood using unbaised esimate of variance
#' @family Model calibration
lmLogLike <- function(model) {
    N <- length(model$residuals)
    SSE <- sum(model$residuals^2)
    return(-(N-2)/2 - N*log(2*pi)/2 - N*log(SSE/(N-2))/2)
}

#' Change point linear regression using supermum of the likelihood ratio.
#'
#' @description Computes change point regression for all possible data partitions
#' selects the one with highest likelihood ratio with respect to single linear 
#' regression.
#'
#' @param traj - recovery trajectory time series data for a single site recovery 
#'               period.
#' @return a list containing the following elements: the data index of the change
#'         point, the two linear models, result of Chow test, \eqn{R^2} and adjusted \eqn{R^2}
#'         for change point regression, and single linear regression model. 
#' @family Model calibration
sup_Like_Ratio <- function(traj) {

    N <- length(traj$HC)
    ind <- 1:N
    traj <- traj %>% mutate(IND=ind) %>% arrange(HC)
    llr <- numeric(N)
    llr[1:N] <- -Inf 
    for (k in 3:(N-2)) {
        # 2. for each possible change point
        # 2.1 fit two linear models to split data [1,k],[k,N]
        traj1 <-traj %>% slice(1:k)
        traj2 <- traj %>% slice(k:N)
        y1 <- traj1$pcHC_lin_fd
        x1 <- traj1$HC
        mod1 <- lm(y1 ~ x1)
        y2 <- traj2$pcHC_lin_fd
        x2 <- traj2$HC
        mod2 <- lm(y2 ~ x2)
        # 1. fit linear model to entire data
        y <- c(y1,y2)
        x <- c(x1,x2)
        mod <- lm(y ~ x)
        # 2.2 compute log likelihood ratio (with possible penalty?)
        llr[k] <- lmLogLike(mod1) + lmLogLike(mod2) - lmLogLike(mod)
    }
    # 3. find K = argmax loglike ratio
    K <- which.max(llr)
    traj1 <-traj %>% slice(1:K)
    traj2 <- traj %>% slice(K:N)
    y1 <- traj1$pcHC_lin_fd
    x1 <- traj1$HC

    mod1 <- lm(y1 ~ x1)
    y2 <- traj2$pcHC_lin_fd
    x2 <- traj2$HC
    mod2 <- lm(y2 ~ x2)
    # 1. fit linear model to entire data
    y <- c(y1,y2)
    x <- c(x1,x2)
    mod <- lm(y ~ x)

    # coefficient of determination for each model
    R2_cp <- 1 - (sum(mod1$residuals^2) + sum(mod2$residuals^2))/(sum((y-mean(y))^2))
    adjR2_cp <- 1 - (1 - R2_cp)*(length(y)-1)/(length(y)-2) 

    # 4. apply Chow test to obtain F-stat and p-value for change-point
    cpt <- chow.test(y1,x1,y2,x2)
    # 5. return both change-point model and single linear model
    return(list(traj$IND[K],mod1,mod2,cpt,R2_cp,adjR2_cp,mod,traj$IND[K-1],traj$IND[K+1]))
}

#' Change point non-linear regression using supermum of the likelihood ratio.
#'
#' @description computes change point regression for all possible data partitions
#' selects the one with highest likelihood ratio with respect to single linear 
#' regression. Here the model fitted to the first partition is linear, but the second
#' is logarithmic (to match Gompertz assumption). 
#'
#' @param traj - recovery trajectory time series data for a single site recovery 
#'               period.
#' @return a list containing the following elements: the data index of the change
#'         point, the two models (one linear and the other logarithmic), result 
#'         of Chow test, \eqn{R^2} and adjusted \eqn{R^2} for change point regression, and 
#'         single linear regression model. 
#' @family Model calibration
sup_Like_Ratio2 <- function(traj) {

    N <- length(traj$HC)
    ind <- 1:N
    traj <- traj %>% mutate(IND=ind) %>% arrange(HC)
    llr <- numeric(N)
    llr[1:N] <- -Inf 
    for (k in 3:(N-2)) {
        # 2. for each possible change point
        # 2.1 fit two linear models to split data [1,k],[k,N]
        traj1 <-traj %>% slice(1:k)
        traj2 <- traj %>% slice(k:N)
        y1 <- traj1$pcHC_lin_fd
        x1 <- traj1$HC
        mod1 <- lm(y1 ~ x1)
        y2 <- traj2$pcHC_lin_fd
        x2 <- traj2$HC
        mod2 <- lm(y2 ~ x2)
        # 1. fit linear model to entire data
        y <- c(y1,y2)
        x <- c(x1,x2)
        mod <- lm(y ~ x)
        # 2.2 compute log likelihood ratio (with possible penalty?)
        llr[k] <- lmLogLike(mod1) + lmLogLike(mod2) - lmLogLike(mod)
    }
    # 3. find K = argmax loglike ratio
    K <- which.max(llr)
    traj1 <-traj %>% slice(1:K)
    traj2 <- traj %>% slice(K:N)
    y1 <- traj1$pcHC_lin_fd
    x1 <- traj1$HC

    mod1 <- lm(y1 ~ x1)
    y2 <- traj2$pcHC_lin_fd
    x2 <- traj2$HC
    mod2 <- nls(y2 ~ -a*log(b*x2),start = list(a = 0.001,b = 0.01),algorithm = "port",
               lower = list(a = 0, b = 0.01), upper = list(a = 1, b = 1.0))
    # 1. fit linear model to entire data
    y <- c(y1,y2)
    x <- c(x1,x2)
    mod <- lm(y ~ x)

    # coefficient of determination for each model
    R2_cp <- 1 - (sum(mod1$residuals^2) + sum(mod2$residuals^2))/(sum((y-mean(y))^2))
    adjR2_cp <- 1 - (1 - R2_cp)*(length(y)-1)/(length(y)-2) 

    # 4. apply Chow test to obtain F-stat and p-value for change-point
    cpt <- chow.test(y1,x1,y2,x2)
    # 5. return both change-point model and single linear model
    return(list(traj$IND[K],mod1,mod2,cpt,R2_cp,adjR2_cp,mod,traj$IND[K-1],traj$IND[K+1]))
}

#' Model intersection
#'
#' @description finds the intersection point of two models.
#'
#' @param model1 calibrated statistical model 1 \eqn{y \sim M_1(x)}
#' @param model2 calibrated statistical model 2 \eqn{y \sim M_2(x)}
#'
#' @return the point x such that \eqn{M_1(x) = M_2(x)}
#' @family Model calibration
compute_intersection <- function(model1,model2) {

    xx <- optimize(function(x) abs(predict(model1,list(x1 = x))- predict(model2,list(x2 = x))),c(0.0, 100.0))$minimum
    return(xx)
}



#' Estimates the effect of the delay
#'
#' @description fits standard growth model to second phase of delayed trajectory 
#' then evaluate the % cover after a fixed interval and the number of years required 
#' to reach given cover assuming no-delay. This can then be paired with the actual 
#' delayed data. 
#'
#' @param traj - processed trajectory including group and benthos codes
#' @param cp_ind - index of change point assuming data sorted by visit number
#' @param TInterval - the lenght of time to project cover for
#' @param CoverThresh - % cover to deterimin time to reach this target
#' @family Two-phase impact
quantify_delay_impact <- function(traj,cp_ind, TInterval,CoverThresh){

    traj <- traj %>% arrange(VISIT_NO)
    Nobs <- nrow(traj)
    # estimate Carrying capacity K = 100 - AB (abiotic) + ST (silt transient)
    K <- 100.0 - traj$AB_G[Nobs] + traj$ST_B[Nobs]

    # filter trajectory for post-change point period
    y_obs <- traj$HC_G[cp_ind:Nobs]
    sigma <- traj$HC_se_G[cp_ind:Nobs]
    # reset time to cp and scale time to order of years
    t_obs <- (traj$T[cp_ind:Nobs] - traj$T[cp_ind])/365.0
    # Apply MLE using Gompertz and logistic models
    
    # neg-loglike for Gompertz model
    Gompertz_nll <- function(alpha) {
        hc_mu <- y_obs
        if (alpha > 0) {
            for (i in 2:length(t_obs)){
                hc_mu[i] <- gompertz_sol(t_obs[i],alpha,K,y_obs[1])
            }
            return(-sum(dnorm(y_obs,hc_mu,sigma,log = TRUE)))
        } else {
            return(NA)
        }
    }
    # neg-loglike for logistic model
    Logistic_nll <- function(alpha) {
        hc_mu <- y_obs
        if (alpha > 0) {
            for (i in 2:length(t_obs)){
                hc_mu[i] <- logistic_sol(t_obs[i],alpha,K,y_obs[1])
            }
            return(-sum(dnorm(y_obs,hc_mu,sigma,log = TRUE)))
        } else {
            return(NA)
        }
    }

    # compute MLE for both models
    fit_G <- mle(Gompertz_nll, start = list(alpha = 0.5),nobs = length(y_obs))
    fit_L <- mle(Logistic_nll, start = list(alpha = 0.5),nobs = length(y_obs))
    # choose best model
    if (AIC(fit_G) < AIC(fit_L)) {
        mle_alpha <- coef(fit_G)
        model_func <- gompertz_sol
    } else {
        mle_alpha <- coef(fit_L)
        model_func <- logistic_sol
    }

    cover_res <- c()
    time_res <- c()

    # return % cover after X years 
    cover_res[1] <- model_func(TInterval,mle_alpha,K,traj$HC_G[1])

    # return time to reach Y % cover
    f <- function(t) model_func(t,mle_alpha,K,traj$HC_G[1]) - CoverThresh
    res <- uniroot(f,lower = 0.0, upper = 20.0, f.lower = - CoverThresh, f.upper = K - CoverThresh)
    time_res[1] <- res$root

    # same thing, but taking the delay offset into account
    delay_cover_res <- c()
    delay_time_res <- c()
    if (TInterval > traj$T[cp_ind]/365.0) {
        delay_cover_res[1] <- model_func(TInterval - traj$T[cp_ind]/365.0,
                                         mle_alpha,K,traj$HC_G[cp_ind])
    } else {
        # get the closest data point to the TInterval
        I <- which.min(abs(traj$T[cp_ind]/365.0 - TInterval))
        delay_cover_res[1] <- traj$HC_G[I]
    }
    f <- function(t) model_func(t,mle_alpha,K,traj$HC_G[cp_ind]) - CoverThresh
    res <- uniroot(f,lower = 0.0, upper = 20.0, f.lower = - CoverThresh, f.upper = K - CoverThresh)
    delay_time_res[1] <- res$root + traj$T[cp_ind]/365.0

    return(list(cover_res,time_res,delay_cover_res,delay_time_res))
}

#' Bayesian Inference for dynamical systems model using Stan.
#'
#' @description Use Stan to perform Bayesian parameter inference for a statistical
#' model based on underlying system of ODEs.
#'
#' @details This function utilises Stan's Hamiltonian Monte Carlo sampler.
#'
#' @param data  data structure as required by Stan model 
#' @param model structure that contains the stan model source code/filename
#' @param config any sampling configuration options. the format of this list
#'               is subject to change.
#' @section Warning: This is not fully implemented as we had issues when introducing the conditional of the 
#'  \eqn{T_d} parameter for two-phase recovery in the stan model file (i.e., stan would not converge unless \eqn{T_d} was fixed). Contact David for details (\email{david.warne@qut.edu.au})
#'
#' @return a stan_fit object representing posterior samples and derived quantities 
#' @family MCMC methods for ODE models
stan_fit_ode_model <- function(data,model,config) {
    # run Stan model fit using sampler configs
    mcmc_fit <- stan(model$filename, data = data, 
                chains = config$chains, iter = config$iter, 
                warmup = config$burnin, algorithm = 'HMC', 
                verbose = TRUE, cores = config$CPUs) 
    # return stan fit object
    return(mcmc_fit);
}


#' Apply convergence diagnostics
#'
#' @description computes the R-hat statistic and Effective Sample size for
#' for a mcmc.list object
#' 
#' @param samples an mcmc.list object
#' @return a list containing the diagnostics
mcmc.diag <- function(samples){
    # convergence diagnostics
    Rhat <- gelman.diag(samples)
    ESS <- effectiveSize(samples)
    #ESS <- 0 #effectiveSize(samples)
    
    # prinf for feedback
    fprintf("R-hat:\n")    
    print(Rhat)
    fprintf("Effective Sample Size:\n")    
    print(ESS)
    # return results
    return(list(Rhat = Rhat, ESS = ESS))
}

#' Bayesian inference for dynamical systems model using Random Walk Metropolis-Hastings.
#'
#' @description Use MCMC to perform Bayesian parameter inference for a statistical
#' model based on underlying system of ODEs.
#'
#' @details This function utilises Geyer's Random-walk Metropolis-Hastings
#' Monte Carlo sampler (\code{mcmc} package) with Petzold et al's ODE solver routines 
#' (\code{deSolve} package) for the model implemenation. \eqn{\hat{R}} (Gelman-Rubin) and 
#' effective sample size diagnostics (\code{coda} package) are used to determine stopping 
#' criteria and Roberts-Rosenthal adaptive re-scalings are applied for improved
#' mixing.
#'
#' @param data   data definition (named list)
#' @param model  model definition (named list)
#' @param config any sampling configuration options. the format of this list
#'               is subject to change.
#' @return An mcmc.list object representing posterior samples 
#' @family MCMC methods for ODE models
mcmc_fit_ode_model <- function(data, model, config) {
    
    # build log-likelihood function
    logl_fact <- function(D,M,ll) function(theta) {
        return(ll(D,M,theta))
    }
    logl <- logl_fact(data,model,model$loglike)
    
    # build log-prior function
    logp_fact <- function(lower,upper,lambda,lp) function(theta) {
        d <- prod(lower <= theta & theta <= upper)
        if (!is.na(d) && d > 0) {
            return(lp(theta,lambda))
        } else {
            return(-Inf)
        }
    }
    logp <- logp_fact(model$lower,model$upper,model$hyp,model$logprior)

    # build log-unnormalised posterior
    logq <- function(theta) {
        if (is.infinite(logp(theta))) {
            return(logp(theta))
        } else {
            return(logl(theta)+logp(theta))
        }
    }


    # If the maxChecks is not specified then repeat until 
    # convergence conditions statisfied
    if (!("maxChecks" %in% names(config))){
        config$maxChecks <- Inf
    }
    
    # default initialisation attempts
    if (!("maxInits" %in% names(config))){
        config$maxInits <- 1000
    }

    if (config$CPUs > 1) {
        doFuture::registerDoFuture()
        cl <- makeCluster(config$CPUs)
        plan(cluster, workers = cl)
    }
    
    # Start warm-up/burn-in procedure
    i <- 1
    theta0 <- model$prior_sampler(model$hyp)
    while (is.infinite(logq(theta0)) && i < config$maxInits) {
        theta0 <- model$prior_sampler(model$hyp)
        i <- i + 1
    }
    if (i == config$maxInits) {
        fprintf("Failed to start chains after %d attempts... aborting.\n", i)
        # shut off multicore parallel if multiple CPUs used
        if (config$CPUs > 1) {
            stopCluster(cl)
        }
        # return empty chains
        return(list())
    }
    logl(theta0)
    L_opt <- config$initscale
    for (j in 1:config$nadapt) {
        fprintf("burnin and tuning [stage %d of %d]...", j, config$nadapt)
        # perform tuning (burnin, then re-scale proposals) 
        mcmc_tune <- foreach(i=1:config$chains, .export=ls(.GlobalEnv), .combine='c', 
                         .packages = c('mcmc','deSolve','truncnorm')) %dorng% {
            # initialise chain
            theta0 <- model$prior_sampler(model$hyp)
            while (is.infinite(logq(theta0))) {
                theta0 <- model$prior_sampler(model$hyp)
            }
            list(metrop(logq,theta0, nbatch = config$burnin,scale = L_opt))
        }
        fprintf("done!\n")
        
        samples <- mcmc.list()
        for (i in 1:config$chains) {
            samples[[i]] <- mcmc(mcmc_tune[[i]]$batch)
            varnames(samples[[i]]) <- labels(model$varnames(samples[[i]][1,]))
            print(mcmc_tune[[i]]$accept)
        }   
        diag.res <- mcmc.diag(samples)
        
        # compute Roberts and Rosenthal optimal scale factor
        pooled_samples <- mcmc(mcmc_tune[[1]]$batch)
        if (config$chains > 1) {
            for (i in 2:config$chains) {
                pooled_samples <- rbind(pooled_samples, mcmc(mcmc_tune[[i]]$batch))
            }
        }
        L_opt <- t(2.38*chol(cov(pooled_samples))/sqrt(ncol(pooled_samples)))
    }

    fprintf("Inital Sampling ...")
    # perform sampling (distribute chains in parallel)
    mcmc_fit <- foreach(i=1:config$chains,  .export=ls(.GlobalEnv), .combine='c',
                        .packages = c('mcmc','deSolve','truncnorm')) %dorng% {
        list(metrop(mcmc_tune[[i]],nbatch = config$iter,scale = L_opt))
    }
    fprintf("done!\n")
    
    # do the diagnostics
    samples <- mcmc.list()
    for (i in 1:config$chains) {
        samples[[i]] <- mcmc(mcmc_fit[[i]]$batch)
        varnames(samples[[i]]) <- labels(model$varnames(samples[[i]][1,]))
    }   
    diag.res <- mcmc.diag(samples)
    numChecks <- 0
    # if enabled, continue until convergence diagnostics are satisfied
    if (config$convcheck) {
        # convergence diagnostics
        fprintf("Convergence diagnostic test ...")
        while ((diag.res$Rhat$mpsrf > config$Rthresh || sum(diag.res$ESS < config$ESSthresh) > 0) && numChecks < config$maxChecks) {
            fprintf("[Failed %g of %g]\nAdditional sampling ...",numChecks, config$maxChecks)
            # perform continued sampling (distribute chains in parallel)
            mcmc_fit2 <- foreach(i=1:config$chains,  .export=ls(.GlobalEnv), .combine='c',
                                .packages = c('mcmc','deSolve','truncnorm')) %dorng% {
                list(metrop(mcmc_fit[[i]],nbatch = config$burnin,scale = L_opt))
            }
            fprintf("done!\n")
            mcmc_fit <- mcmc_fit2
            # do the diagnostics
            for (i in 1:config$chains) {
                samples[[i]] <- mcmc(rbind(samples[[i]],mcmc_fit[[i]]$batch))
                varnames(samples[[i]]) <- labels(model$varnames(mcmc_fit[[i]]$batch[1,]))
            
            }        
            # convergence diagnostics
            diag.res <- mcmc.diag(samples)
            fprintf("Convergence diagnostic test ...")
            numChecks <- numChecks + 1
        }
        if (numChecks < config$maxChecks) {
            fprintf("[Passed]\n")
        } else {
            fprintf("[Failed %g of %g] Aborting.\n",numChecks, config$maxChecks)
        }
    }

    # shut off multicore parallel if multiple CPUs used
    if (config$CPUs > 1) {
        stopCluster(cl)
    }
    # return chains
    return(samples)
}


#' Bayesian Inference for dynamical systems model using Adaptive Random Walk Metropolis-Hastings.
#' 
#' @description Use adaptive MCMC to perform Bayesian parameter inference for a statistical
#' model based on underlying system of ODEs.
#'
#' @details This function utilises Vihola's robust adaptive Metropolis method
#' with coerced acceptance rates (\code{adaptMCMC} package) with Petzold et al's ODE 
#' solver routines (\code{deSolve} package) for the model implemenation. \eqn{\hat{R}} (Gelman-Rubin) and 
#' effective sample size diagnostics (\code{coda} package) are used to determine stopping 
#' criteria.
#'
#' @param data   data definition (named list)
#' @param model  model definition (named list)
#' @param config any sampling configuration options. the format of this list
#'               is subject to change.
#' @return An mcmc.list object representing posterior samples 
#' @family MCMC methods for ODE models
adaptMCMC_fit_ode_model <- function(data, model, config) {
    
    # build log-likelihood function
    logl_fact <- function(D,M,ll) function(theta) {
        return(ll(D,M,theta))
    }
    logl <- logl_fact(data,model,model$loglike)
    
    # build log-prior function
    logp_fact <- function(lower,upper,lambda,lp) function(theta) {
        d <- prod(lower <= theta & theta <= upper)
        if (!is.na(d) && d > 0) {
            return(lp(theta,lambda))
        } else {
            return(-Inf)
        }
    }
    logp <- logp_fact(model$lower,model$upper,model$hyp,model$logprior)

    # build log-unnormalised posterior
    logq <- function(theta) {  
        if (is.infinite(logp(theta))) {
            return(logp(theta))
        } else {
            return(logl(theta)+logp(theta))
        }
    }

    # If the maxChecks is not specified then repeat until 
    # convergence conditions statisfied
    if (!("maxChecks" %in% names(config))){
        config$maxChecks <- Inf
    }
    
    # default initialisation attempts
    if (!("maxInits" %in% names(config))){
        config$maxInits <- 1000
    }

    if (config$CPUs > 1) {
        doFuture::registerDoFuture()
        cl <- makeCluster(config$CPUs)
        plan(cluster, workers = cl)
    }
    
    # Start warm-up/burn-in procedure
    i <- 1
    theta0 <- model$prior_sampler(model$hyp)
    while (is.infinite(logq(theta0)) && i < config$maxInits) {
        #print(theta0)
        theta0 <- model$prior_sampler(model$hyp)
        i <- i + 1
    }
    if (i == config$maxInits) {
        fprintf("Failed to start chains after %d attempts... aborting.\n", i)
        # shut off multicore parallel if multiple CPUs used
        if (config$CPUs > 1) {
            stopCluster(cl)
        }
        # return empty chains
        return(list())
    }

    logl(theta0)

    fprintf("Initial Sampling ...")
    mcmc_fit <- foreach(i=1:config$chains,
                        .export="param_names",
                        ## .export=ls(.GlobalEnv),  # Removed by Murray as it exports the entire
                                        # Global environment and much of it is too large
                        .combine='c',
                        .packages=c('adaptMCMC','deSolve','truncnorm')) %dorng% {
        # initialise chains
        theta0 <- model$prior_sampler(model$hyp)
        while (is.infinite(logq(theta0))) {
            theta0 <- model$prior_sampler(model$hyp)
        }
        list(MCMC(logq,n=config$burnin,init=theta0,scale=rep(config$initscale,length(theta0)),
                  adapt=TRUE,acc.rate=0.134,list=TRUE))
    }
    fprintf("done!\n")

    samples <- mcmc.list()
    for (i in 1:config$chains) {
        samples[[i]] <- convert.to.coda(mcmc_fit[[i]])
        varnames(samples[[i]]) <- labels(model$varnames(samples[[i]][1,]))
    }
    if (config$convcheck == TRUE){
        diag.res <- mcmc.diag(samples)
    }
    numChecks <- 0    
    # if enabled, continue until convergence diagnostics are satisfied
    if (config$convcheck == TRUE) {
        fprintf("Convergence diagnostic test ...")
        while ((diag.res$Rhat$mpsrf > config$Rthresh || sum(diag.res$ESS < config$ESSthresh) > 0) && numChecks < config$maxChecks) {
            fprintf("[Failed %g of %g]\nAdditional sampling ...",numChecks, config$maxChecks)
            # preform continued sampling and adaptation
            mcmc_fit2 <- foreach(i=1:config$chains,
                                 .export = "param_names",
                                 ## .export=ls(.GlobalEnv),
                                 .combine='c',
                                 .packages=c('adaptMCMC','deSolve','truncnorm')) %dorng% {
                list(MCMC.add.samples(mcmc_fit[[i]],n.update=config$iter))
            }
            fprintf("done!\n")
            mcmc_fit <- mcmc_fit2
            # do the diagnostics
            for (i in 1:config$chains) {
                samples[[i]] <- convert.to.coda(mcmc_fit[[i]])
                varnames(samples[[i]]) <- labels(model$varnames(samples[[i]][1,]))
            }

            # convergence diagnostics
            diag.res <- mcmc.diag(samples)
            numChecks <- numChecks + 1   
            fprintf("Convergence diagnostic test ...")
        }
        if (numChecks < config$maxChecks) {
            fprintf("[Passed]\n")
        } else {
            fprintf("[Failed %g of %g] Aborting.\n",numChecks, config$maxChecks)
        }
    } else {
        # preform continued sampling and adaptation
        fprintf("Sampling")
        mcmc_fit2 <- foreach(i=1:config$chains,
                             .export = "param_names",
                             ## .export=ls(.GlobalEnv),
                             .combine='c',
                             .packages=c('adaptMCMC','deSolve','truncnorm')) %dorng% {
                                 list(MCMC.add.samples(mcmc_fit[[i]],n.update=config$iter))
                             }
        fprintf("done!\n")
        mcmc_fit <- mcmc_fit2
        # do the diagnostics
        for (i in 1:config$chains) {
            samples[[i]] <- convert.to.coda(mcmc_fit[[i]])
            varnames(samples[[i]]) <- labels(model$varnames(samples[[i]][1,]))
        }
    }

    # shut off multicore parallel if multiple CPUS used
    if (config$CPUs > 1) {
        stopCluster(cl)
    }
    # return chains
    return(samples)
}    
 
#' Sampler of the prior predictive distribution
#' 
#' @description generates n samples from the prior distribution and then simulate a 
#' single sample from the data generation process (model simulation + obs process)
#'
#' @param model  model definition (named list)
#' @param n number of prior samples to generate
#' @family Predictive sampling
prior_predictive_samples <- function(data,model, n) {
    pps <- c()
    for (i in 1:n) {
        # sample the prior
        theta <- model$prior_sampler(model$hyp)
        # simulate data generation process
        pps <- rbind(pps,model$like_sampler(data,model,theta))
    }
    return(pps)
}

#' Sampler of the posterior predictive distribution
#' 
#' @description utilises MCMC samples from the posterior distribution and then simulates a 
#' single sample from the data generation process (model simulation + obs process).
#' For performance, the chain is thinned based on largest ESS in the sample, note 
#' that as a result the ESS of the posterior predictive samples is necessarily 
#' less than that of the orginial MCMC chains (i.e., using every sample in each chain
#' will be statistically a better estimate, but could be nearly as expensive as the original
#' MCMC simulation).
#'
#' @param model  model definition (named list)
#' @param mcmc_chains \code{mcmc.list} (form \code{coda} package) of mcmc chains
#' @family Predictive sampling
posterior_predictive_samples <- function(data, model, mcmc_chains) {
    ESS <- effectiveSize(mcmc_chains)
    Neff <- max(ESS)
    tau <- floor(nrow(mcmc_chains[[1]])/Neff)
    pps <- c()
    for (j in 1:length(mcmc_chains)) {
        for (i in seq(1,nrow(mcmc_chains[[1]]),by=tau)) {
            theta <- mcmc_chains[[j]][i,]
            pps <- rbind(pps,model$like_sampler(data,model,theta))
        }
    }
    return(pps)
}


