#' Add index for the number of simul for each SA value
#'
#' @description Add index for the number of simul for each SA value
#' @param dataset_sim dataset with simulated data from the simul_fitnessdata_normal()

#' @return the same dataset with new columns
#' @export 
#'
#' @examples add_sim_number_SA(dataset_sim = sim)

add_sim_number_SA <- function(dataset_sim = sim) {
  
  ## Index SA_Gen to keep only 100 simulations per value of SA_Gen
  SA_Gen <- sort(unique(dataset_sim$SA_Gen))
  dataset_sim$index_SA_Gen <- NA
  for (i in SA_Gen){
    nmax <- sum(dataset_sim$SA_Gen==i)
    dataset_sim$index_SA_Gen[dataset_sim$SA_Gen==i] <- 1:nmax
  }
  
  ## Index SA_NonGen to keep only 100 simulations per value of SA_NonGen
  SA_NonGen <- sort(unique(dataset_sim$SA_NonGen))
  dataset_sim$index_SA_NonGen <- NA
  for (i in SA_NonGen){
    nmax <- sum(dataset_sim$SA_NonGen==i)
    dataset_sim$index_SA_NonGen[dataset_sim$SA_NonGen==i] <- 1:nmax
  }
  
  
  return (dataset_sim)
}