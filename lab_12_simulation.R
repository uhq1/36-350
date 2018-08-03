generate_data = function(n, p){
  covariates = matrix(rnorm(n*p), n, p)
  responses = as.vector(rnorm(n))
  return(list(covariates = covariates, responses = responses))
}

generate_data(2,3)

model_select = function(covariates, responses, cutoff){
  regr = lm(responses ~ covariates)
  coeffs = coef(regr)[-1]
  inds = which((coeffs <= cutoff) & (!is.na(coeffs)))
  new.regr = lm(responses ~ covariates[, inds])
  return(new.regr)
}

gen = generate_data(10,10)
model_select(gen$covariates, gen$responses, 0)

run_simulation = function(n_trials = 1000, n = c(100, 1000, 10000), 
                          p = c(10, 20, 50), cutoff = 0.05){
  combs = expand.grid(n, p)
  helper = function(n, p, cutoff){
    data = generate_data(n, p)
    result = model_select(data$covariates, data$responses, cutoff)
    summ = summary(result)
    return(summ$coefficients[-1,"Pr(>|t|)"])
  }
  for (i in 1:nrow(combs)){
    trials = unlist(replicate(n_trials, helper(combs[i,1], combs[i,2], cutoff)))
    png(paste('trial',i,'.png', sep=''))
    hist(trials, xlab = "p-value", main = paste('Trial',i,sep=" "))
    dev.off()   
  }
  
}

run_simulation()
