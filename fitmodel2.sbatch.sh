#!/bin/bash
#
#SBATCH --job-name=standardmodel2
#
#SBATCH -p hns
#SBATCH --time=48:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2G

# load the module
ml R

R --no-save << EOF

source("fit_quantiles.R")
set.seed(1234)

start_time <- Sys.time()

fit <- DEoptim(fitSSE, lower=c(1, 1, 1, 1, .01, .01, .01, .01, 1), 
               upper=c(9000, 9000, 9000, 9000, 1, 2, 1, 2, 9), 
               proc_facilitates=F, distro="uniform",
               regularize_input_rate = list(mean=1797.8, sd=1130.2),
               control = DEoptim.control(NP = 150, itermax = 160)) 

end_time <- Sys.time()
print(end_time - start_time)

save(fit, file="fits/fit_uncon_pfF.RData")
fitSSE(fit$optim$bestmem, graph="unif_unconstrained_proc_facil_FALSE")
EOF
