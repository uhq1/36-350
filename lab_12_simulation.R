generate_data = function(n, p){
  covariates = matrix(rnorm(n*p), n, p)
  responses = as.vector(rnorm(n))
  return(list(covariates = covariates, responses = responses))
}

generate_data(2,3)
