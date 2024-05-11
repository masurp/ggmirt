#' Helper function to simulate IRT data
#'
#' Function to simulate data that can be used to fit IRT models. 
#'  
#' @param n.obs Number of observations that should be included in the data set
#' @param n.items Number of items that should be simulated
#' @param discrimination Standard deviation on the log scale
#' @param seed Seed for the random number generation process
#' @param cut Either "random" for a randomized transformation of the model probability matrix into the model 0-1 matrix or an integer value between 0 and 1.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(ggmirt)
#' 
#' sim_irt(n.obs = 200, n.items = 10)
#'
sim_irt <- function(n.obs = 100, 
                    n.items = 10, 
                    discrimination = 0, 
                    seed = NULL, 
                    cut = "random") {
  
  # Get item difficulty distribution
  if (length(n.items) == 1) {
    if (!is.null(seed)) 
      set.seed(seed)
    
    difficulty <- rnorm(n.items)
    no.items <- n.items
    
  } else {
    
    difficulty <- n.items
    no.items <- length(n.items)
  }
 
  # Get person ability distribution
  if (length(n.obs) == 1) {
    if (!is.null(seed)) 
      set.seed(seed)
    
    ability <- rnorm(n.obs)
    no.obs <- n.obs
    
  } else {
    
    ability <- n.obs
    no.obs <- length(n.obs)
  }
  
  # Draw discrimination distribution if needed
  if (length(discrimination) > 1) {
    alpha <- discrimination
    
  } else {
    
    if (!is.null(seed)) 
      set.seed(seed)
    
    alpha <- rlnorm(no.items, 0, sdlog = discrimination)
  }
  
  # Create empty matrix
  psolve <- matrix(0, no.obs, no.items)
  
  
  # Simulate response pattern
  for (i in 1:no.obs) for (j in 1:no.items) psolve[i, j] <- exp(alpha[j] * (ability[i] - difficulty[j]))/(1 + exp(alpha[j] * (ability[i] - difficulty[j])))
  
  # Transform into binary items
  if (cut == "random") {
    if (!is.null(seed)) 
      set.seed(seed)
    
    m <- (matrix(runif(no.items * no.obs), no.obs, no.items) < psolve) * 1
    
  } else {
    
    m <- (cut < psolve) * 1
  }
  
  d <- as_tibble(m, .name_repair = \(x) paste0("V", 1:n.items))
  
  return(d)
}


