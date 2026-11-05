## Functions Created

get_zscore <- function(x1, x2, n1, n2) {
  x1 <- sum(x1)
  x2 <- sum(x2)
  
  n1 <- sum(n1)
  n2 <- sum(n2)
  
  p1 <- x1/n1
  p2 <- x2/n2
  
  p <- (x1+x2)/(n1+n2)
  
  SE <- (p*(1-p)*(1/n1+1/n2))^0.5
  
  z <- (p1-p2)/SE
  
  z
}

## This function was used for the two proportion z test for vaccinated vs. unvaccinated cases of hospitalizations and deaths.
