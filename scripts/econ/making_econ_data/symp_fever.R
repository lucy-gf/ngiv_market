#### Making health econ data ####

## from carrat (DOI: 10.1093/aje/kwm375)
# print('symptomatic, fever')
# symp_probs <- data.table(
#   outcome = c('symptoms','fever'),
#   med = c(66.9, 34.9)/100,
#   l95 = c(58.3, 26.7)/100,
#   u95 = c(74.5, 44.2)/100
#   )
# ## determining distribution
# f.gamma <- function(shape, rate, x) {
#   p <- pgamma(x, shape, rate)
#   # return both
#   return(c(p))
# }
# delta <- function(fit, actual) sum((fit-actual)^2)
# objective <- function(theta, x, prob, ...) {
#   ab <- (theta)
#   fit <- f.gamma(ab[1], ab[2], x=as.numeric(x),...)
#   # fit <- f.beta(ab[1], ab[2], x=as.numeric(x),...)
#   return (delta(fit, prob))
# }
# fcn_fitting <- function(rates,
#                         probs){
# 
#   x <- c(unlist(unname(rates)))
#   sol <- suppressWarnings(optim(f=objective,p=c(1,1),
#                                 # method="BFGS",
#                                 x=x,
#                                 prob=c(probs),
#                                 control = list(reltol = 1e-15)
#   ))
#   parms <- (sol$par)
#   return(parms)
# }
# 
# for(i in 1:nrow(symp_probs)){
#   parms <- fcn_fitting(symp_probs[i,2:4], c(0.5, 0.025, 0.975))
#   symp_probs[i,"shape"] <- parms[1]
#   symp_probs[i,"rate"] <- parms[2]
#   symp_probs[i,"med_fit"] <- qgamma(p=c(0.5), shape=parms[1], rate=parms[2])
#   symp_probs[i,"l95_fit"] <- qgamma(p=c(0.025), shape=parms[1], rate=parms[2])
#   symp_probs[i,"u95_fit"] <- qgamma(p=c(0.975), shape=parms[1], rate=parms[2])
# }
# 
# symp_samples <- data.table(
#   simulation_index = 1:100,
#   symp_prob = rgamma(100, shape = unlist(symp_probs[outcome=='symptoms','shape']), rate = unlist(symp_probs[outcome=='symptoms','rate'])),
#   fever_prob = rgamma(100, shape = unlist(symp_probs[outcome=='fever','shape']), rate = unlist(symp_probs[outcome=='fever','rate'])))
# write_csv(symp_samples, file='data/econ/symp_samples.csv')

symp_samples <- data.table(read_csv('data/econ/symp_samples.csv', show_col_types=F))




