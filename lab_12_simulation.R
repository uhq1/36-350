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
