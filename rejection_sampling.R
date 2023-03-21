#' Rejection Sampling algorithm
#' 
#' @description 
#' Generate random values with a specified distribution
#' of probability. 
#' For more details on the algorithm: https://en.wikipedia.org/wiki/Rejection_sampling
#' 
#' @param n Number of values to generate
#' @param f Distribution from which we want to generate
#' @param g "Proposal distribution" 
#' @param rg Function that generates random values following the g distribution
#' @param k Factor by which we multiply g
#' @param report Boolean arguent used to determine whether to print details or not
#' 
#' @return A numeric vector of values generated from a random variable with 
#' probability distribution f.
#' 
#' @examples
#' With the following code we are able to generate 10'000 random numbers
#' following a Beta(2,2) distrution by using a Unif(0,1) as our proposal 
#' distribution.
#' f <- function(x) dbeta(x, 2, 2)
#' k <- 1.5
#' rejection.sampling(1e+05, f, dunif, runif, k, FALSE)
rejection.sampling <- function(n,
                               f,
                               g,
                               rg,
                               k,
                               report = TRUE){
  res <- rep(NA, n) 
  ntry <- 0 
  while(any(is.na(ris))) { 
    x <- rg(n) 
    y <- k * g(x) * runif(n) 
    accepted <- y < f(x) 
    
    free_spaces <- is.na(ris) 
    res[accepted & free_spaces] <- x[accepted & free_spaces] 
    ntry <- ntry + 1
  }
  if (report)
    cat("tentativi:", ntry)
  return(res)
}
