# fit wordbank quantile data
require(tidyverse)
require(DEoptim)

#source("model-nonsampling.R")
source("model-childesfreqs.R")
load("data/wordbank_quantiles.Rdata") # quantiles from wordbank EN WG+WS productive vocab

max_age = 30 # months
n_learners = 500

qs <- c(0.10,0.25,0.50,0.75,0.90)

ex_parms = list(distro="uniform",
                input_rate = 1000,
                input_rate_sd = 100,
                threshold = 1000,
                threshold_sd = 100,
                mean_learning_rate = .5, # proc_speed_asymp
                learning_rate_sd = .1,
                proc_facilitates = T,
                proc_speed_dev = .72, 
                proc_speed_dev_sd = .1,
                start_age = 1
)

fit_loglik <- function(parms, proc_facilitates=T, distro="logzipf", graph="") {
  full_parms = list(distro=distro,
                    input_rate = parms[1],
                    input_rate_sd = parms[2],
                    threshold = parms[3],
                    threshold_sd = parms[4],
                    mean_learning_rate = parms[5], # proc_speed_asymp
                    learning_rate_sd = parms[6],
                    proc_facilitates = proc_facilitates,
                    proc_speed_dev = parms[7], 
                    proc_speed_dev_sd = parms[8],
                    start_age = round(parms[9], 0)
  )
  simdat = simulate(full_parms)$known_words
  
  # log(L) = sum log( Model(data | parms) )
  # for a given age, need probability of match between data and model
  # make an empirical observed and model predicted distribution: words x month,
  # (likelihood of observing the data given the model parameters);
  # so if observed = predicted, L=1; and at minimum L=0
  # estimate a density for empirical (per month) as well as for observed??
  #density()
}

# specify the single value normal probability function
norm_lik = function(x, m, s){
  # m = mean, s = sd
  return( 1 / sqrt(2*pi*s^2) * exp((-1/(2*s^2)) * (x-m)^2) )
}

#norm_lik(c(-.5, 0, .5), 0, 1)

fitSSE <- function(parms, proc_facilitates=T, distro="logzipf", graph="", regularize_input_rate=NA) {
  full_parms = list(distro=distro,
                  input_rate = parms[1],
                  input_rate_sd = parms[2],
                  threshold = parms[3],
                  threshold_sd = parms[4],
                  mean_learning_rate = parms[5], # proc_speed_asymp
                  learning_rate_sd = parms[6],
                  proc_facilitates = proc_facilitates,
                  proc_speed_dev = parms[7], 
                  proc_speed_dev_sd = parms[8],
                  start_age = round(parms[9], 0) # integer
  )
  simdat = simulate(full_parms)$known_cdi_words # known_words
  sim_quants <- simdat %>%
    group_by(month) %>%
    group_modify(~{
      quantile(.x$words, probs=qs) %>%
        tibble::enframe()
    }) %>% spread(name, value)
  
  SSE = sum((sim_quants[8:max_age,] - quants)^2)
  MSE = SSE / (nrow(quants)*(ncol(quants)-1))
  # regularize input rate (mean and sd) - 
  #  penalize more the farther estimated parm is far from LENA estimates 
  if(!is.na(regularize_input_rate)) {
    SSE = SSE + .004*(regularize_input_rate$m - full_parms$input_rate)^2 + 
                .004*(regularize_input_rate$sd - full_parms$input_rate_sd)^2
  }
  if(graph!="") {
    theme_set(theme_classic())
    ggplot(simdat, aes(x=month, y=words)) + geom_line(aes(group=id), alpha=.05) +  
      geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
      labs(colour="Quantile") + xlab("Age (months)") + ylab("Vocabulary Size") + xlim(1, max_age) +
      geom_text(x=2, y=1000, label=paste("MSE =",round(MSE, digits=1)))
    ggsave(paste0(graph,".pdf"), width=6, height=5)
  }
  
  return(SSE)
}


set.seed(1234)

regularize_input_rate = list(mean=1797.8, sd=1130.2) # AWC (CDS is lower: mean=1198)

#fit <- DEoptim(fitSSE, lower=c(1, 1, 1, 1, .01, .01, .01, .01, 1), distro="logzipf",
#               upper=c(9000, 9000, 9000, 9000, 1, 2, 1, 2, 9), 
#               regularize_input_rate = regularize_input_rate, # or NA
#               control = DEoptim.control(NP = 120, itermax = 100)) # , F = 1.2, CR = 0.7
# 256282.537500 bestmemit:  
#pars = c(377.67, 2091.39, 8034.29, 2638.25, 0.402713, 0.warnings(015136, 0.627340, 0.109277)
#fitSSE(pars, graph=T) # SSE= 265834
#pars = fit$optim$bestmem
#fitSSE(pars, graph="unconstrained_logzipf_proc_facil_TRUE") 

#pars = c(204, 2436, 6937, 2127, 0.56, 0.03, 0.72, 0.21, 1) # best fit for CDI data.. (start_age=1)
#fitSSE(pars, graph="constrained_proc_facil_TRUE_start_age1") # SSE=256790

#fit2 <- DEoptim(fitSSE, lower=c(1, 1, 1, 1, .555, .01, .715, .01), upper=c(9000, 9000, 9000, 9000, .564, 2, .724, 2), 
#                control = DEoptim.control(NP = 80, itermax = 100))
# SSE=254416.43
#pars2 = c(111.33, 2886.68, 7965.46, 2340.04, 0.56, 0.011, 0.716244, 0.129518)
#fitSSE(pars2, graph=T) # 224546.4
#fitSSE(fit2$optim$bestmem, graph="constrained_proc_facil_TRUE")